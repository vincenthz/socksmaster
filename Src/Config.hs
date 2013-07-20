module Config
    ( Provider(..)
    , ProviderName
    , RoutesName
    , Route(..)
    , Routes
    , Config(..)
    , readConfig
    ) where

import Control.Applicative
import Data.Monoid
import Data.List
import Data.Char (isDigit)
import Network.Socket hiding (recv, sendAll)

type ProviderName = String
type RoutesName = String

data Provider = ProviderExternal String PortNumber
              | ProviderDirect
              | ProviderNone
              deriving (Show,Eq)

data Route = Route String ProviderName
           | Inherit RoutesName
           | CatchAll ProviderName
           deriving (Show,Eq)
type Routes = [Route]

data Config = Config
    { configProviders :: [(ProviderName, Provider)]
    , configRoutes    :: [(RoutesName, Routes)]
    } deriving (Show,Eq)

configOneRoute r = mempty { configRoutes = [r] }
configOneProvider p = mempty { configProviders = [p] }

instance Monoid Config where
    mempty        = Config [] []
    mappend c1 c2 = Config
                    { configProviders = configProviders c1 ++ configProviders c2
                    , configRoutes    = configRoutes c1 ++ configRoutes c2
                    }

parseConfig allLines = let (g, ls) = skipUntilGroup allLines in parseRoutes g [] ls
  where getGroupLine s
            | isPrefixOf "[" s && isSuffixOf "]" s = Just $ strip $ init $ tail s
            | otherwise                            = Nothing
        getNormalAssignment s
            | null s2   = Nothing
            | otherwise = Just (strip s1, strip $ tail s2)
          where (s1,s2) = break (== '=') s
        strip = reverse . stripForward . reverse . stripForward
        stripForward = dropWhile (\c -> c == ' ' || c == '\t') 
        skipUntilGroup (l:ls) = case getGroupLine l of
                                    Nothing -> skipUntilGroup ls
                                    Just g  -> (g, ls)
        parseRoutes name acc []     = [(name, reverse acc)]
        parseRoutes name acc (l:ls) =
            case getGroupLine l of
                Nothing  ->
                    case getNormalAssignment l of
                        Nothing                     -> parseRoutes name acc ls
                        Just (k,v) | k == "inherit" -> parseNext (Inherit v) ls
                                   | k == "*"       -> parseNext (CatchAll v) ls
                                   | otherwise      -> parseNext (Route k v) ls
                Just grp -> (name, reverse acc) : parseRoutes grp [] ls
          where parseNext val ls = parseRoutes name (val:acc) ls

structureConfig cfg (gname, kvs)
    | gname == "providers" = cfg `mappend` (Config (map toProvider kvs) [])
    | otherwise            = cfg `mappend` (Config [] [(gname, kvs)])
  where toProvider (Inherit _) = error "inherit in providers section not allowed"
        toProvider (CatchAll _) = error "default in providers section not allowed"
        toProvider (Route name pr)
            | pr == "none"   = (name, ProviderNone)
            | pr == "direct" = (name,  ProviderDirect)
            | "external:" `isPrefixOf` pr = let externalS = drop (length "external:") pr
                                                (e1, e2)  = break (== ':') externalS
                                             in case e2 of
                                                _ | null e2   -> error "missing a port number"
                                                  | otherwise ->
                                                    let port = tail e2
                                                     in if isNumber port
                                                        then (name, ProviderExternal e1 (fromIntegral $ read port))
                                                        else error ("external not a valid port: " ++ show port)
            | otherwise      = error ("unknown provider: " ++ name ++ "=" ++ pr)
        isNumber = and . map isDigit
        
readConfig path = foldl structureConfig mempty . parseConfig . lines <$> readFile path
