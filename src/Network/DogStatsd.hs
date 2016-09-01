module Network.DogStatsd (
  UdpClient
, dogStatsdClient
, fromURI

, increment
, decrement
, count
, gauge
, timing
, histogram
) where

import Network.Statsd (Stat, Type(..), fmtDatagram)
import Network.UdpClient(UdpClient(..), Hostname, Port, Namespace, Key, client, fromURI, send)

import Control.Monad
import Data.Maybe
import Data.List
import Data.Time.Units
import Text.Printf
import Network.URI

type Tags = [(String, String)]

dogStatsdClient :: String -> IO UdpClient
dogStatsdClient url = (fromURI . fromJust . parseURI) url

increment :: UdpClient -> Stat -> Tags -> IO ()
increment client stat = count client stat 1

decrement :: UdpClient -> Stat -> Tags -> IO ()
decrement client stat = count client stat (-1)

count :: UdpClient -> Stat -> Int -> Tags -> IO ()
count client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Count tags

gauge :: UdpClient -> Stat -> Int -> Tags -> IO ()
gauge client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Gauge tags

timing :: UdpClient -> Stat -> Millisecond -> Tags -> IO ()
timing client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat (fromIntegral value) Timing tags

histogram :: UdpClient -> Stat -> Int -> Tags -> IO ()
histogram client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Histogram tags

fmtTag :: (String, String) -> String
fmtTag (a, "") = a
fmtTag (a, b) = a ++ ":" ++ b

fmtDogStatsdDatagram :: Stat -> Stat -> Int -> Type -> Tags -> String
fmtDogStatsdDatagram namespace stat value stat_type [] = fmtDatagram namespace stat value stat_type
fmtDogStatsdDatagram namespace stat value stat_type tags =
  let statsdDatagram = fmtDatagram namespace stat value stat_type
      tagSuffix = intercalate "," $ fmtTag <$> tags
  in printf "%s|#%s" statsdDatagram tagSuffix
