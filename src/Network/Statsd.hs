module Network.Statsd (
  StatsdClient,
  client,

  fromURI,

  Hostname,
  Port,
  Stat,
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

data StatsdClient = StatsdClient { getSocket :: Net.Socket
                                 , getNamespace :: Stat
                                 , getSigningKey :: Maybe Key
                                 }

type Hostname = String
type Port = Int

fromURI :: URI -> IO StatsdClient
fromURI uri = case uriAuthority uri of
              Nothing   -> fail "invalid URI"
              Just auth -> let hostname = uriRegName' auth
                               port = let port' = (stripLeading ':' . uriPort) auth
                                       in if null port'
                                          then 8126
                                          else read port'
                               prefix = replace '/' '.' ((stripLeading '/' . uriPath) uri)
                               key = let userInfo = uriUserInfo auth
                                         init' = if null userInfo
                                                 then ""
                                                 else init userInfo
                                         (user, pass) = case break (==':') init' of
                                                        (u, ':':p) -> (u, p)
                                                        _          -> ("", "")
                                      in if null pass
                                         then Nothing
                                         else Just pass

                           in client hostname port prefix key

  where
    replace :: Eq a => a -> a -> [a] -> [a]
    replace from to list = (\a -> if a == from then to else a) <$> list

    uriRegName' :: URIAuth -> String
    uriRegName' auth = let hostname = uriRegName auth
                        in (takeWhile (/=']') . dropWhile (=='[')) hostname

    stripLeading :: Eq a => a -> [a] -> [a]
    stripLeading x y@(y':ys')
      | x == y'   = ys'
      | otherwise = y

client :: Hostname -> Port -> Stat -> Maybe Key -> IO StatsdClient
client host port namespace key = do
  (addr:_) <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- Net.socket (Net.addrFamily addr) Net.Datagram Net.defaultProtocol
  Net.connect sock (Net.addrAddress addr)

  return $ StatsdClient sock namespace key

increment :: StatsdClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> IO ()
count client stat value = void . send client $ encode (getNamespace client) stat value Count

gauge :: StatsdClient -> Stat -> Int -> IO ()
gauge client stat value = void . send client $ encode (getNamespace client) stat value Gauge

timing :: StatsdClient -> Stat -> Millisecond -> IO ()
timing client stat value = void . send client $ encode (getNamespace client) stat (fromIntegral value) Timing

histogram :: StatsdClient -> Stat -> Int -> IO ()
histogram client stat value = void . send client $ encode (getNamespace client) stat value Histogram

encode :: Stat -> Stat -> Int -> Type -> Payload
encode namespace stat value stat_type =
  let prefix = if null namespace
               then ""
               else namespace ++ "."
      message = printf "%s%s:%s|%s" prefix stat (show value) (show stat_type)
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
