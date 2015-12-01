module Network.Statsd (
  StatsdClient,
  client,

  Hostname,
  Port,
  Stat,

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

import System.Time
import System.IO.Error

import Crypto.Hash
import Crypto.Random.DRBG

import Text.Printf
import Data.Time.Units

import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net

type Stat = String

data StatsdClient = StatsdClient { socket :: Net.Socket
                                 , namespace :: Stat
                                 , signingKey :: Maybe String
                                 }

type Hostname = String
type Port = Int

client :: Hostname -> Port -> Stat -> Maybe String -> IO (Maybe StatsdClient)
client host port namespace key = do
  socket <- tryIOError $ Net.getAddrInfo Nothing (Just host) (Just $ show port) >>=
    \(addr:_) -> Net.socket (Net.addrFamily addr) Net.Datagram Net.defaultProtocol >>=
      \sock -> tryIOError (Net.connect sock $ Net.addrAddress addr) >>
      return sock

  case socket of
    Left e  -> return Nothing
    Right s -> return $ Just $ StatsdClient s namespace key

increment :: StatsdClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> IO ()
count client stat value = send client stat value Count

gauge :: StatsdClient -> Stat -> Int -> IO ()
gauge client stat value = send client stat value Guage

timing :: StatsdClient -> Stat -> Millisecond -> IO ()
timing client stat value = send client stat (fromIntegral value) Timing

histogram :: StatsdClient -> Stat -> Int -> IO ()
histogram client stat value = send client stat value Histogram

encode :: Stat -> Stat -> Int -> Type -> B.ByteString
encode namespace stat value stat_type =
  let prefix = if null namespace
               then ""
               else namespace ++ "."
      message = printf "%s%s:%s|%s" prefix stat (show value) (show stat_type)
  in BC.pack message

send :: StatsdClient -> Stat -> Int -> Type -> IO ()
send client stat value stat_type = do
  let payload = encode (namespace client) stat value stat_type
  signedPayload <- signed (signingKey client) payload
  sendPayload client signedPayload

sendPayload :: StatsdClient -> B.ByteString -> IO ()
sendPayload client payload = void (tryIOError $ Net.send (socket client) payload)

type Nonce = B.ByteString
type Key = String

signed :: Maybe Key -> B.ByteString -> IO B.ByteString
signed Nothing payload = return payload
signed (Just key) payload = do
  (TOD sec _) <- getClockTime
  let timestamp = B.concat $ BLazy.toChunks $ toLazyByteString $ int64LE $ fromIntegral sec

  gen <- newGenIO :: IO CtrDRBG
  let (nonce, _) = throwLeft $ genBytes 4 gen

  let newPayload = B.concat [timestamp, nonce, payload]

  return $ sign key newPayload

sign :: Key -> B.ByteString -> B.ByteString
sign key payload = let keyBytes = BC.pack key
                       signature = toBytes (hmac keyBytes payload :: HMAC SHA256)
                    in B.append signature payload

data Type = Count | Guage | Timing | Histogram

instance Show Type where
  show Count = "c"
  show Guage = "g"
  show Timing = "ms"
  show Histogram = "h"
