{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forever, when, void)
import Network (PortID(..))
import Network.Socket hiding (recv)
import Network.Socks5 (defaultSocksConf, socksConnect)
import Network.Socks5.Lowlevel
import Network.Socks5.Types
import Network.BSD
import qualified Data.ByteString.Char8 as BC
import System.Directory
import System.FilePath
import System.Environment
import System.Console.GetOpt
import System.Timeout
import System.Posix.Signals
import System.Exit
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracketOnError)
import qualified Control.Exception as E
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word
import Data.Hourglass
import Text.Printf

import qualified Data.ByteString as B

-- import Network.Connection

import SocketTable
import Config

import System.Console.ANSI
import System.IO
import System.IO.Unsafe

listenPortDefault = 2080

data NetHandlerType =
      HandlerType_SOCKS String PortID
    | HandlerType_Direct
    deriving (Show,Eq)

data NetHandler = NetHandler
    { netHandlerType :: NetHandlerType
    }
    deriving (Show,Eq)

terminalLock = unsafePerformIO $ newMVar ()
{-# NOINLINE terminalLock #-}

withTerminalLock :: IO () -> IO ()
withTerminalLock f = modifyMVar_ terminalLock $ \() -> f >> return ()

logErrAddr :: SockAddr -> String -> IO ()
logErrAddr addr s = withTerminalLock $ do
    hSetSGR stdout [SetColor Foreground Vivid Red]
    hPutStr stdout "[ERROR] "
    hSetSGR stdout [SetColor Foreground Vivid Green]
    hPutStr stdout (show addr)
    hSetSGR stdout []
    hPutStrLn stdout (": " ++ s)

logAddr :: SockAddr -> String -> IO ()
logAddr addr s = withTerminalLock $ do
    hSetSGR stdout [SetColor Foreground Vivid Blue]
    hPutStr stdout "[ LOG ] "
    hSetSGR stdout [SetColor Foreground Vivid Green]
    hPutStr stdout (show addr)
    hSetSGR stdout []
    hPutStrLn stdout (": " ++ s)

displaySpeed :: Int -> Int -> IO ()
displaySpeed written recv = do
    setTitle ("sent: " ++ toHU written ++ " recv: " ++ toHU recv)
    when (written /= 0 || recv /= 0) $ withTerminalLock $ do
        hSetSGR stdout [SetColor Foreground Vivid Yellow]
        hPutStr stdout "## sent = "
        hSetSGR stdout [SetColor Foreground Vivid Magenta]
        hPutStr stdout (toHU written)
        hSetSGR stdout [SetColor Foreground Vivid Yellow]
        hPutStr stdout "   recv = "
        hSetSGR stdout [SetColor Foreground Vivid Magenta]
        hPutStr stdout (toHU recv)
        hSetSGR stdout []
        hPutStrLn stdout ""

toHU :: Int -> String
toHU n
    | n > 1 * 1024 * 1024 = printf "%.2f Mb/s" (fromIntegral n / (1024 * 1024) :: Double)
    | n > 1 * 1024        = printf "%.2f Kb/s" (fromIntegral n / 1024 :: Double)
    | otherwise           = printf "%f b/s" (fromIntegral n :: Double)

data Direction    = ClientToProvider
                  | ProviderToClient
                  deriving (Show,Eq)
data Notification = NotificationErrRead
                  | NotificationReadFinished
                  | NotificationErrWrite
                  deriving (Show,Eq)

proxy st dir chan src dst = allocaBytesAligned bufSz 8 loop
  where loop :: Ptr Word8 -> IO ()
        loop buf = do
            eb <- E.try (withSocket st src $ recvBuf src buf bufSz)
            case eb of
                Left (_:: E.SomeException) -> writeChan chan (dir, NotificationErrRead)
                Right 0 -> writeChan chan (dir, NotificationReadFinished)
                Right n -> do
                              when (dir == ClientToProvider) $ socketHasWritten st src n
                              evoid <- sendBufAll buf n
                              case evoid of
                                  Left _   -> writeChan chan (dir, NotificationErrWrite)
                                  Right () -> loop buf

        sendBufAll :: Ptr Word8 -> Int -> IO (Either E.SomeException ())
        sendBufAll iBuf toSend = go iBuf toSend
            where go _   0 = return $ Right ()
                  go buf n = do
                        esent <- E.try (sendBuf dst buf n)
                        case esent of
                            Left e     -> return (Left e)
                            Right sent -> do
                                when (dir == ProviderToClient) $ socketHasRead st dst sent
                                go (buf `plusPtr` sent) (n - sent)
        bufSz = 16384

doListen (PortNumber serv) = do
    proto <- getProtocolNumber "tcp"
    -- We should probably specify addrFamily = AF_INET6 and the filter
    -- code below should be removed. AI_ADDRCONFIG is probably not
    -- necessary. But this code is well-tested. So, let's keep it.
    let hints = defaultHints { addrFlags      = [AI_ADDRCONFIG, AI_PASSIVE]
                             , addrSocketType = Stream
                             , addrProtocol   = proto }
    addrs <- getAddrInfo (Just hints) Nothing (Just (show serv))
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr   = if null addrs' then head addrs else head addrs'
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        sClose
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress addr)
            listen sock 512
            return sock
        )

createLocalServer socksPort st chan = do
    sock <- doListen socksPort
    insertSocketTable st sock Listen
    forever $ loop sock
  where loop sock = do
            (client,clientAddr) <- withSocket st sock (accept sock)
            insertSocketTable st client Client
            forkIO $ processClient client clientAddr
        processClient clientSock clientAddr = do
            req <- timeout 1000000 $ withSocket st clientSock (socksListen clientSock)
            case req of
                Nothing -> logErrAddr clientAddr "timeout" >> closeSocket st clientSock >> return ()
                Just r  -> do
                    --logAddr clientAddr ("connection request for = " ++ show (requestDstAddr r) ++ ":" ++ show (requestDstPort r))
                    writeChan chan (clientSock,clientAddr,r)

-- biproxy from sock to sock2
biproxy dstAddr st clientAddr sock sock2 = do
    chan  <- newChan
    cppid <- forkIO $ proxy st ClientToProvider chan sock sock2
    pcpid <- forkIO $ proxy st ProviderToClient chan sock2 sock
    (d,n) <- readChan chan
    --putStrLn (show clientAddr ++ " " ++ show d ++ " " ++ show n)
    case n of
        NotificationErrRead  -> case d of
                                    ClientToProvider -> shutdown sock2 ShutdownSend >> killThread pcpid
                                    _                -> shutdown sock ShutdownSend >> killThread cppid
        NotificationErrWrite -> case d of
                                    ClientToProvider -> shutdown sock2 ShutdownReceive
                                    _                -> shutdown sock ShutdownReceive
        NotificationReadFinished -> case d of
                                        ClientToProvider -> shutdown sock2 ShutdownSend >> killThread pcpid
                                        _                -> shutdown sock ShutdownSend >> killThread cppid
    (d2,n2) <- readChan chan
    --putStrLn (show clientAddr ++ " " ++ show d2 ++ " " ++ show n2)
    [info,info2] <- mapM (closeSocket st) [sock,sock2]
    case info of
        Nothing -> logAddr clientAddr ("closing " ++ show dstAddr)
        Just i  ->
            let logS = printf "closing %s info: %s seconds sent=%s recv=%s"
                            (show dstAddr)
                            (show (socketLastUsed i `timeDiff` socketCreated i))
                            (toHU $ socketSent i) (toHU $ socketRecv i)
             in logAddr clientAddr logS

replySocksError st sock err =
    withSocket st sock $ sendSerialized sock (SocksResponse (SocksReplyError err) (SocksAddrIPV4 0) 0)

-- | Connect a stream to an external SOCKS server.
-- for example a SSH proxy can be used there.
doConnectProvider st (ProviderExternal socksHost socksPort) clientAddr clientSock dstaddr port = do
    let socksConf = defaultSocksConf socksHost socksPort
    e <- E.try $ socksConnect socksConf (SocksAddress dstaddr port)
    case e of
        Left (exn :: E.SomeException)  -> do
            logErrAddr clientAddr ("cannot connect to provider " ++ socksHost ++ ":" ++ show socksPort ++ " to get to " ++ show dstaddr)
            replySocksError st clientSock SocksErrorGeneralServerFailure
            _ <- closeSocket st clientSock
            return ()
        Right (provider, (serverBoundAddr, serverBoundPort)) -> do
            insertSocketTable st provider Provider
            logAddr clientAddr ("[External] connect to " ++ show dstaddr ++ ":" ++ show port)
            -- reply to the client that this has been successful.
            sendSerialized clientSock (SocksResponse SocksReplySuccess serverBoundAddr serverBoundPort)

            -- loop for IO between socket
            biproxy dstaddr st clientAddr clientSock provider

doConnectProvider st ProviderDirect clientAddr clientSock dstaddr port = do
    -- try to establish a direct connection
    r <- E.try $ do
        saddr <- getDestSockAddr
        proto <- getProtocolNumber "tcp"
        bracketOnError (socket AF_INET Stream proto) sClose $ \sock -> do
            connect sock saddr
            return sock
    case r of
        -- on error we reply to our client that something went wrong and close the socket
        Left (e:: E.SomeException) -> do
            logAddr clientAddr ("[Direct] connect to " ++ show dstaddr ++ ":" ++ show port ++ " failed " ++ show e)
            replySocksError st clientSock SocksErrorHostUnreachable
            _ <- closeSocket st clientSock
            return ()
        -- on success we establish a biproxy.
        Right providerSock -> do
            insertSocketTable st providerSock Provider
            logAddr clientAddr ("[Direct] connect to " ++ show dstaddr ++ ":" ++ show port)
            serverBoundName <- getSocketName providerSock
            let (sbp, sba) = case serverBoundName of
                                    SockAddrInet serverPort ha       -> (serverPort, SocksAddrIPV4 ha)
                                    SockAddrInet6 serverPort _ ha6 _ -> (serverPort, SocksAddrIPV6 ha6)
                                    SockAddrUnix _                   -> error "sock addr unix"
            -- reply to the client that this has been successful.
            withSocket st clientSock $ sendSerialized clientSock (SocksResponse SocksReplySuccess sba sbp)
            biproxy dstaddr st clientAddr clientSock providerSock
    where
        getDestSockAddr = do
            case dstaddr of
                SocksAddrIPV4 ha       -> return $ SockAddrInet port ha
                SocksAddrIPV6 ha6      -> return $ SockAddrInet6 port 0 ha6 0
                SocksAddrDomainName dn -> do he <- getHostByName (BC.unpack dn)
                                             return $ SockAddrInet port (hostAddress he)

doConnectProvider st ProviderNone clientAddr clientSock dstaddr port = do
    logAddr clientAddr ("[None] connect to " ++ show dstaddr ++ ":" ++ show port)
    replySocksError st clientSock SocksErrorHostUnreachable
    _ <- closeSocket st clientSock
    return ()

providerFromRoute defaultRoute routes _ addr =
    case addr of
        SocksAddrDomainName dn -> case findPrefix routes dn of
                                    Just p  -> p
                                    Nothing -> defaultRoute
        SocksAddrIPV4 _ -> defaultRoute
        SocksAddrIPV6 _ -> defaultRoute
  where findPrefix [] _      = Nothing
        findPrefix ((r,p):rs) dn | B.isSuffixOf r dn = Just p
                                 | otherwise         = findPrefix rs dn


-- | Route a connection request to the proper provider.
doConnect defaultRoute routes st clientAddr clientSock dstaddr port = do
    let provider = providerFromRoute defaultRoute routes port dstaddr
    void $ forkIO $ doConnectProvider st provider clientAddr clientSock dstaddr port
    return ()

configToRoutes :: ProviderName -> Config -> (Maybe Provider, [(B.ByteString, Provider)])
configToRoutes n cfg =
    case lookup ("rules=" ++ n) (configRoutes cfg) of
        Nothing  -> error ("cannot find rules for " ++ show n)
        Just kvs -> foldl accRules (Nothing, []) kvs
  where accRules (_, rs) (CatchAll k)   = (Just $ lookupPN k, rs)
        accRules (def, rs) (Inherit i) = let (iDef, irs) = configToRoutes i cfg
                                          in (maybe iDef Just def, rs ++ irs)
        accRules (def, rs) (Route s p) = (def, rs ++ [(BC.pack s,lookupPN p)])
        lookupPN s = case lookup s (configProviders cfg) of
                        Nothing -> error ("cannot find provider " ++ s)
                        Just p  -> p

mainListen cfg n o = withSocketsDo $ do
    let (def_, routes) = configToRoutes n cfg
        def = maybe (error "no default configured") id def_
    mapM_ (putStrLn . show) routes

    chan <- newChan
    -- create a generic SOCKS server that listen for connection from localhost
    st <- newSocketTable

    let handler = Catch $ dumpSocketTable st
    _ <- installHandler sigUSR1 handler Nothing

    let pn = PortNumber $ maybe listenPortDefault id $ foldl (\_ x -> case x of
                    ListenAt p -> Just (fromIntegral $ (read p :: Integer))
                    _          -> Nothing) Nothing o

    _ <- forkIO $ reportUsage st

    void $ forkIO $ createLocalServer pn st chan
    forever $ do
        (clientSock,clientAddr,req) <- readChan chan
        case requestCommand req of
            SocksCommandConnect -> doConnect def routes st clientAddr clientSock (requestDstAddr req) (requestDstPort req)
            _                   -> putStrLn "unsupported socks command" >> closeSocket st clientSock >> return ()

reportUsage st = forever $ do
    threadDelay 1000000
    (writtenData, readData) <- socketDumpSpeed st
    displaySpeed writtenData readData

data Flag = ListenAt String
          | AtWork
          | AtWorkSecure
          | HomeSecure
          | AllowSpam
          | Help
          deriving (Show,Eq)

options =
    [ Option ['l'] ["listen"] (ReqArg ListenAt "port") "socks server listening option (default 2080)"
    , Option ['h'] ["help"] (NoArg Help) "help me"
    ]

usage = putStrLn (usageInfo "usage: socksmaster [-l <port>] <name>" options)

main = do
    userDataDir <- getAppUserDataDirectory "socksmaster"
    let userDataFile = userDataDir </> "config"
    exists  <- doesDirectoryExist userDataDir
    exists2 <- doesFileExist userDataFile

    when (not exists || not exists2) $ error "user configuration doesn't exists"
    cfg <- readConfig userDataFile
    --putStrLn $ show iniCfg

    args <- getArgs
    case getOpt Permute options args of
        (o, [n], []) -> do when (Help `elem` o) (usage >> exitSuccess)
                           mainListen cfg n o
        (_, _, errs) -> usage >> exitFailure
