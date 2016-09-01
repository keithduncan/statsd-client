module Network.Statsd (
  statsdClient,
  fromURI,

  Stat,
  Type(..),
  fmtDatagram,

  increment,
  decrement,
  count,
  gauge,
  timing,
  histogram,
) where

import Network.Statsd.UdpClient(UdpClient(..), fromURI, send)

import Control.Monad
import Data.Maybe
import Data.Time.Units
import Text.Printf
import Network.URI

type Stat = String
data Type = Count | Gauge | Timing | Histogram
instance Show Type where
  show Count = "c"
  show Gauge = "g"
  show Timing = "ms"
  show Histogram = "h"

statsdClient :: String -> IO UdpClient
statsdClient url = (fromURI . fromJust . parseURI) url

increment :: UdpClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: UdpClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: UdpClient -> Stat -> Int -> IO ()
count client stat value = void . send client $ fmtDatagram stat value Count

gauge :: UdpClient -> Stat -> Int -> IO ()
gauge client stat value = void . send client $ fmtDatagram stat value Gauge

timing :: UdpClient -> Stat -> Millisecond -> IO ()
timing client stat value = void . send client $ fmtDatagram stat (fromIntegral value) Timing

histogram :: UdpClient -> Stat -> Int -> IO ()
histogram client stat value = void . send client $ fmtDatagram stat value Histogram

fmtDatagram :: Stat -> Int -> Type -> String
fmtDatagram stat value stat_type = printf "%s:%s|%s" stat (show value) (show stat_type)
