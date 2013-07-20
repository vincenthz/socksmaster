module SocketTable where

import Control.Monad
import Control.Applicative
import Data.Time.Clock
import Data.Time.Format
import Control.Concurrent.MVar
import System.Locale
import Network.Socket (Socket, getPeerName, fdSocket, sClose)
import Data.Hashable
import qualified Control.Exception as E
import qualified Data.HashTable.IO as H


type HashTable k v = H.BasicHashTable k v

data Type = Listen | Client | Provider
    deriving (Show,Eq)

data SocketLog = SocketLog
    { socketOpenTime  :: {-# UNPACK #-} !UTCTime
    , socketType      :: !Type
    , socketUsed      :: MVar (UTCTime, UTCTime)
    }

instance Hashable Socket where
    hashWithSalt salt = hashWithSalt salt . fdSocketInt
                    where fdSocketInt :: Socket -> Int
                          fdSocketInt = fromIntegral . fdSocket
        

data SocketTable = SocketTable (HashTable Socket SocketLog)

-- 7.6.2
--modifyIORef'

newSocketTable = SocketTable <$> H.new

insertSocketTable (SocketTable h) socket ty = do
    c <- getCurrentTime
    u <- newMVar (c,c)
    H.insert h socket (SocketLog c ty u)

deleteSocketTable (SocketTable h) socket =
    H.delete h socket

withSocket (SocketTable h) socket f = do
    socketlog <- maybe (error ("socket cannot be found")) id <$> H.lookup h socket
    c <- getCurrentTime
    modifyMVar_ (socketUsed socketlog) $ \(_,e) -> return (c,e)
    r <- f
    c <- getCurrentTime
    modifyMVar_ (socketUsed socketlog) $ \(s,_) -> return (s,c)
    return r

closeSocket st socket = sClose socket >> deleteSocketTable st socket

dumpSocketTable (SocketTable h) = putStrLn (replicate 80 '=') >> (H.toList h >>= mapM_ printLine) >> putStrLn (replicate 80 '=')
    where printLine (socket, slog) = do
                pn <- if socketType slog == Listen 
                        then return ""
                        else either errTostr show <$> E.try (getPeerName socket)
                r  <- (\(s,e) -> "started=" ++ showTime s ++ " end=" ++ showTime e) <$> readMVar (socketUsed slog)
                putStrLn (show (socketType slog) ++ ":" ++ show pn
                         ++ " opened(" ++ showTime (socketOpenTime slog) ++ ") last_used(" ++ show r ++ ")")
          errTostr :: E.SomeException -> String
          errTostr e = show e

          showTime :: UTCTime -> String
          showTime t = formatTime defaultTimeLocale "%c" t
