module Network.Statsd (
  StatsdClient,
  client,

  fromURI,

  Hostname,
  Port,
  Stat,
  Tags,
  Key,

  increment,
  decrement,
  count,
  gauge,
  timing,
  histogram,
) where

import Control.Monad
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BLazy
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Builder (int64LE, toLazyByteString)
import Data.Word
import Data.Byteable
import Data.Maybe
import Data.List

import System.Time
import System.IO.Error

import Crypto.Hash
import Crypto.Random.DRBG

import Text.Printf
import Data.Time.Units

import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net
import Network.URI

type Stat = String
type Tags = [(String, String)]

data StatsdClient = StatsdClient { getSocket :: Net.Socket
                                 , getNamespace :: Stat
                                 , getSigningKey :: Maybe Key
                                 }

type Hostname = String
type Port = Int

fromURI :: URI -> IO StatsdClient
fromURI (URI "statsd:" (Just (URIAuth auth regname port)) path _ _) =
  let regname' = uriRegName' regname
      port' = if null port
              then 8126
              else read $ stripLeading ':' port
      prefix = replace '/' '.' $ stripLeading '/' path
      key = case break (==':') (stripTrailing '@' auth) of
              (u, ':':p) -> Just p
              _          -> Nothing
   in client regname' port' prefix key

  where
    replace :: Eq a => a -> a -> [a] -> [a]
    replace from to list = (\a -> if a == from then to else a) <$> list

    uriRegName' :: String -> String
    uriRegName' = takeWhile (/=']') . dropWhile (=='[')

    stripLeading :: Eq a => a -> [a] -> [a]
    stripLeading x [] = []
    stripLeading x y@(y':ys')
      | x == y'   = ys'
      | otherwise = y

    stripTrailing :: Eq a => a -> [a] -> [a]
    stripTrailing x [] = []
    stripTrailing x xs = let init' = init xs
                             last' = last xs
                          in if last' == x
                             then init'
                             else xs

fromURI uri = error $ "invalid URI" ++ show uri

client :: Hostname -> Port -> Stat -> Maybe Key -> IO StatsdClient
client host port namespace key = do
  (addr:_) <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- Net.socket (Net.addrFamily addr) Net.Datagram Net.defaultProtocol
  Net.connect sock (Net.addrAddress addr)

  return $ StatsdClient sock namespace key

increment :: StatsdClient -> Stat -> Maybe Tags -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> Maybe Tags -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> Maybe Tags -> IO ()
count client stat value tags = void . send client $ encode (getNamespace client) stat value Count tags

gauge :: StatsdClient -> Stat -> Int -> Maybe Tags -> IO ()
gauge client stat value tags = void . send client $ encode (getNamespace client) stat value Gauge tags

timing :: StatsdClient -> Stat -> Millisecond -> Maybe Tags -> IO ()
timing client stat value tags = void . send client $ encode (getNamespace client) stat (fromIntegral value) Timing tags

histogram :: StatsdClient -> Stat -> Int -> Maybe Tags -> IO ()
histogram client stat value tags = void . send client $ encode (getNamespace client) stat value Histogram tags

encode :: Stat -> Stat -> Int -> Type -> Maybe Tags -> Payload
encode namespace stat value stat_type maybeTags =
  let prefix = if null namespace
               then ""
               else namespace ++ "."
      fmtTag x = (fst x) ++ ":" ++ (snd x)
      tagSuffix = case maybeTags of
                  Just tags -> "#" ++ (intercalate "," (map fmtTag tags))
                  _ -> ""
      message = printf "%s%s:%s|%s%s" prefix stat (show value) (show stat_type) tagSuffix
  in BC.pack message

type Payload = B.ByteString

send :: StatsdClient -> Payload -> IO (Either IOError ())
send client payload = do
  signedPayload <- signed (getSigningKey client) payload
  tryIOError . void $ Net.send (getSocket client) signedPayload

type Nonce = B.ByteString
type Key = String

signed :: Maybe Key -> Payload -> IO Payload
signed Nothing payload = return payload
signed (Just key) payload = do
  (TOD sec _) <- getClockTime
  let timestamp = B.concat . BLazy.toChunks . toLazyByteString . int64LE $ fromIntegral sec

  gen <- newGenIO :: IO CtrDRBG
  let (nonce, _) = throwLeft $ genBytes 4 gen

  let newPayload = B.concat [timestamp, nonce, payload]

  return $ sign key newPayload

sign :: Key -> Payload -> Payload
sign key payload = let keyBytes = BC.pack key
                       signature = toBytes (hmac keyBytes payload :: HMAC SHA256)
                    in B.append signature payload

data Type = Count | Gauge | Timing | Histogram

instance Show Type where
  show Count = "c"
  show Gauge = "g"
  show Timing = "ms"
  show Histogram = "h"
