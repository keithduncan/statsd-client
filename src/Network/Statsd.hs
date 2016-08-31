module Network.Statsd (
  UdpClient,
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

import Network.UdpClient(UdpClient(..), Hostname, Port, Namespace, Key, client, fromURI, send)

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
count client stat value = void . send client $ fmtDatagram (getNamespace client) stat value Count

gauge :: UdpClient -> Stat -> Int -> IO ()
gauge client stat value = void . send client $ fmtDatagram (getNamespace client) stat value Gauge

timing :: UdpClient -> Stat -> Millisecond -> IO ()
timing client stat value = void . send client $ fmtDatagram (getNamespace client) stat (fromIntegral value) Timing

histogram :: UdpClient -> Stat -> Int -> IO ()
histogram client stat value = void . send client $ fmtDatagram (getNamespace client) stat value Histogram

fmtDatagram :: Stat -> Stat -> Int -> Type -> String
fmtDatagram namespace stat value stat_type =
  let prefix = if null namespace then "" else namespace ++ "."
  in printf "%s%s:%s|%s" prefix stat (show value) (show stat_type)
