module Statsd (
  StatsdClient,
  client,
  
  increment,
  decrement,
  count,
  gauge,
  timing,
  histogram,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BLazy
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Builder (int64LE, toLazyByteString)
import Data.Word
import System.Time
import System.Random
import Crypto.Hash
import Data.Byteable
import Text.Printf

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

type Stat = String

data StatsdClient = StatsdClient { socket :: Socket
                                 , addr :: SockAddr
                                 , namespace :: Stat
                                 , signingKey :: Maybe String
                                 }

client :: String -> Int -> Stat -> Maybe String -> IO StatsdClient
client host port namespace key = do
  addrInfos <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfos
  
  socket <- Network.Socket.socket (addrFamily serverAddr) Datagram defaultProtocol
  return $ StatsdClient socket (addrAddress serverAddr) namespace key

increment :: StatsdClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> IO ()
count client stat value = Statsd.send client stat value Count
  
gauge :: StatsdClient -> Stat -> Int -> IO ()
gauge client stat value = Statsd.send client stat value Guage
  
-- duration in milliseconds
timing :: StatsdClient -> Stat -> Int -> IO ()
timing client stat value = Statsd.send client stat value Timing

histogram :: StatsdClient -> Stat -> Int -> IO ()
histogram client stat value = Statsd.send client stat value Histogram

send :: StatsdClient -> Stat -> Int -> Type -> IO ()
send client stat value stat_type = do
  let prefix = if null $ namespace client
               then ""
               else namespace client ++ "."
  let message = printf "%s%s:%s|%s" prefix stat (show value) (show stat_type)
  let payload = BC.pack message
  
  payload <- case signingKey client of 
             Nothing  -> return payload
             Just key -> signed key payload

  sendPayload client payload

sendPayload :: StatsdClient -> B.ByteString -> IO ()
sendPayload client payload = do
  Network.Socket.ByteString.sendTo (Statsd.socket client) payload $ addr client
  return ()

type Nonce = B.ByteString
type Key = String

signed :: Key -> B.ByteString -> IO B.ByteString
signed key payload = do
  sec <- getClockTime >>= (\(TOD sec _) -> return sec)
  let timestamp = B.concat $ BLazy.toChunks $ toLazyByteString $ int64LE $ fromIntegral sec

  gen <- getStdGen
  let nonce = randomBytes 4 gen
  
  let newPayload = B.concat [timestamp, nonce, payload]

  return $ sign key newPayload

sign :: Key -> B.ByteString -> B.ByteString
sign key payload = let keyBytes = BC.pack key
                       signature = toBytes (hmac keyBytes payload :: HMAC SHA256)
                   in B.append signature payload

randomBytes :: Int -> StdGen -> Nonce
randomBytes n g = B.pack $ bytes n g
  where
    bytes :: Int -> StdGen -> [Word8]
    bytes 0 _ = []
    bytes n g = let (x, nextG) = next g
                in fromIntegral x:bytes (n-1) nextG

data Type = Count | Guage | Timing | Histogram

instance Show Type where
  show Count = "c"
  show Guage = "g"
  show Timing = "ms"
  show Histogram = "h"
  