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
import Data.ByteString.Builder (int64LE, toLazyByteString)
import Data.Word
import System.Time
import System.Random
import Crypto.Hash
import Data.Byteable

type Stat = String
 
data StatsdClient = StatsdClient { host :: String
                                 , port :: Int
                                 , namespace :: Stat
                                 , key :: Maybe String
                                 }

client :: String -> Int -> Stat -> Maybe String -> StatsdClient
client = StatsdClient

increment :: StatsdClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> IO ()
count client stat value = send client stat value Count
  
gauge :: StatsdClient -> Stat -> Int -> IO ()
gauge client stat value = send client stat value Guage
  
-- duration in milliseconds
timing :: StatsdClient -> Stat -> Int -> IO ()
timing client stat value = send client stat value Timing

histogram :: StatsdClient -> Stat -> Int -> IO ()
histogram client stat value = send client stat value Histogram

send :: StatsdClient -> Stat -> Int -> Type -> IO ()
send client stat value stat_type = do
  let prefix = if null $ namespace client
               then ""
               else namespace client ++ "."
  let message = prefix ++ stat ++ ":" ++ show value ++ "|" ++ show stat_type
  let payload = BC.pack message
  
  let payload = case key client of 
                Nothing  -> payload
                Just key -> signed payload

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
  