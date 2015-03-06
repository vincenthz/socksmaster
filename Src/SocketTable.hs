module SocketTable where

import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar
import Network.Socket (Socket, getPeerName, fdSocket, sClose)
import Data.Hashable
import qualified Control.Exception as E
import qualified Data.HashTable.IO as H
import Data.Hourglass
import System.Hourglass

type HashTable k v = H.BasicHashTable k v

data Type = Listen | Client | Provider
    deriving (Show,Eq)

data SocketLog = SocketLog
    { socketOpenTime  :: {-# UNPACK #-} !ElapsedP
    , socketType      :: !Type
    , socketUsed      :: MVar (ElapsedP, ElapsedP)
    , socketTraffic   :: MVar (Int, Int) -- (sent, received)
    }

data SocketInfo = SocketInfo
    { socketCreated  :: ElapsedP
    , socketLastUsed :: ElapsedP
    , socketSent     :: Int
    , socketRecv     :: Int
    } deriving (Show,Eq)

socketLogToInfo :: SocketLog -> IO SocketInfo
socketLogToInfo slog = do
    lastUsed <- snd <$> readMVar (socketUsed slog)
    (s,r)    <- readMVar (socketTraffic slog)
    return $ SocketInfo
        { socketCreated  = socketOpenTime slog
        , socketLastUsed = lastUsed
        , socketSent     = s
        , socketRecv     = r
        }

instance Hashable Socket where
    hashWithSalt salt = hashWithSalt salt . fdSocketInt
                    where fdSocketInt :: Socket -> Int
                          fdSocketInt = fromIntegral . fdSocket
        

data SocketTable = SocketTable
    (HashTable Socket SocketLog)
    (MVar Int)
    (MVar Int)

-- 7.6.2
--modifyIORef'

newSocketTable = SocketTable <$> H.new
                             <*> newMVar 0
                             <*> newMVar 0

insertSocketTable (SocketTable h _ _) socket ty = do
    currentTime <- timeCurrentP
    lastUsed    <- newMVar (currentTime,currentTime)
    traffic     <- newMVar (0,0)
    H.insert h socket (SocketLog currentTime ty lastUsed traffic)

deleteSocketTable (SocketTable h _ _) socket = do
    r <- H.lookup h socket
    case r of
        Nothing   -> return Nothing
        Just slog -> H.delete h socket >> (Just <$> socketLogToInfo slog)

withSocket (SocketTable h _ _) socket f = do
    socketlog <- maybe (error ("socket cannot be found")) id <$> H.lookup h socket
    c <- timeCurrentP
    modifyMVar_ (socketUsed socketlog) $ \(_,e) -> return (c,e)
    r <- f
    c2 <- timeCurrentP
    modifyMVar_ (socketUsed socketlog) $ \(s,_) -> return (s,c2)
    return r

onValidSocket :: SocketTable -> Socket -> (SocketLog -> IO ()) -> IO ()
onValidSocket (SocketTable h _ _) socket f = do
    mSocketLog <- H.lookup h socket
    case mSocketLog of
        Nothing        -> return ()
        Just socketLog -> f socketLog

socketHasRead st@(SocketTable _ _ r) socket n = do
    modifyMVar_ r (\v -> return (v + n))
    onValidSocket st socket $ \slog ->
        modifyMVar_ (socketTraffic slog) $ \(sent,recv) -> return (sent,recv+n)

socketHasWritten st@(SocketTable _ w _) socket n = do
    modifyMVar_ w (\v -> return (v + n))
    onValidSocket st socket $ \slog ->
        modifyMVar_ (socketTraffic slog) $ \(sent,recv) -> return (sent+n,recv)

-- | Dump global speed of the socket table and reset to 0
socketDumpSpeed (SocketTable _ w r) = do
    modifyMVar r $ \rv -> do
        z <- modifyMVar w $ \wv -> return (0, wv)
        return (0, (z, rv))

closeSocket st socket = sClose socket >> deleteSocketTable st socket

dumpSocketTable (SocketTable h _ _) = putStrLn (replicate 80 '=') >> (H.toList h >>= mapM_ printLine) >> putStrLn (replicate 80 '=')
    where printLine (socket, slog) = do
                pn <- if socketType slog == Listen 
                        then return ""
                        else either errTostr show <$> E.try (getPeerName socket)
                r  <- (\(s,e) -> "started=" ++ showTime s ++ " end=" ++ showTime e) <$> readMVar (socketUsed slog)
                putStrLn (show (socketType slog) ++ ":" ++ show pn
                         ++ " opened(" ++ showTime (socketOpenTime slog) ++ ") last_used(" ++ show r ++ ")")
          errTostr :: E.SomeException -> String
          errTostr e = show e

          showTime :: ElapsedP -> String
          showTime t = timePrint [Format_Year,dash,Format_Month2,dash,Format_Day2,Format_Hour,colon,Format_Minute,colon,Format_Second] t
            where colon = Format_Text ':'
                  dash  = Format_Text '-'
