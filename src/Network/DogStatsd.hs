module Network.DogStatsd (
 dogStatsdClient
, increment
, decrement
, count
, gauge
, timing
, histogram
) where

import Network.Statsd (Stat, Type(..), fmtDatagram)
import Network.Statsd.UdpClient(UdpClient, fromURI, send)

import Control.Monad
import Data.Maybe
import Data.List
import Data.Time.Units
import Text.Printf
import Network.URI

type Tags = [(String, String)]
type Name = String
type Value = String

dogStatsdClient :: String -> IO UdpClient
dogStatsdClient url = (fromURI . fromJust . parseURI) url

increment :: UdpClient -> Stat -> Tags -> IO ()
increment client stat = count client stat 1

decrement :: UdpClient -> Stat -> Tags -> IO ()
decrement client stat = count client stat (-1)

count :: UdpClient -> Stat -> Int -> Tags -> IO ()
count client stat value tags = void . send client $ fmtDogStatsdDatagram stat value Count tags

gauge :: UdpClient -> Stat -> Int -> Tags -> IO ()
gauge client stat value tags = void . send client $ fmtDogStatsdDatagram stat value Gauge tags

timing :: UdpClient -> Stat -> Millisecond -> Tags -> IO ()
timing client stat value tags = void . send client $ fmtDogStatsdDatagram stat (fromIntegral value) Timing tags

histogram :: UdpClient -> Stat -> Int -> Tags -> IO ()
histogram client stat value tags = void . send client $ fmtDogStatsdDatagram stat value Histogram tags

fmtTag :: (Name, Value) -> String
fmtTag (a, "") = a
fmtTag (a, b) = a ++ ":" ++ b

fmtDogStatsdDatagram :: Stat -> Int -> Type -> Tags -> String
fmtDogStatsdDatagram stat value statType [] = fmtDatagram stat value statType
fmtDogStatsdDatagram stat value statType tags =
  let statsdDatagram = fmtDatagram stat value statType
      tagSuffix = intercalate "," $ fmtTag <$> tags
  in printf "%s|#%s" statsdDatagram tagSuffix
