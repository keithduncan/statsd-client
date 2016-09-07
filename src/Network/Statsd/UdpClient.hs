module Network.Statsd.UdpClient (
  UdpClient
, fromURI
, send
) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BLazy
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Builder (int64LE, toLazyByteString)
import Data.Byteable
import System.Time
import System.IO.Error
import Crypto.Hash
import Crypto.Random.DRBG
import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net
import Network.URI

type Hostname = String
type Port = Int
type Key = String
type Namespace = String
type Payload = B.ByteString
type Nonce = B.ByteString

data UdpClient = UdpClient { getSocket :: Net.Socket
                           , getNamespace :: Namespace
                           , getSigningKey :: Maybe Key
                           }

fromURI :: URI -> IO UdpClient
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

client :: Hostname -> Port -> Namespace -> Maybe Key -> IO UdpClient
client host port namespace key = do
  (addr:_) <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- Net.socket (Net.addrFamily addr) Net.Datagram Net.defaultProtocol
  Net.connect sock (Net.addrAddress addr)

  return $ UdpClient sock namespace key

send :: UdpClient -> String -> IO (Either IOError ())
send client datagram = do
  let namespace = getNamespace client
  let message = if null namespace then datagram else namespace ++ "." ++ datagram
  signedPayload <- signed (getSigningKey client) (BC.pack message)
  tryIOError . void $ Net.send (getSocket client) signedPayload

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
