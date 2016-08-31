module Network.DogStatsd (DogStatsdClient, dogStatsdClient, fromURI, increment, decrement, count, gauge, timing, histogram) where

import Network.Statsd (Stat, Type(..), fmtDatagram)
import Network.UdpClient(UdpClient(..), Hostname, Port, Key, client, fromURI, send)

import Control.Monad
import Data.Maybe
import Data.List
import Data.Time.Units
import Text.Printf
import Network.URI

type Tags = [(String, String)]
type DogStatsdClient = UdpClient

dogStatsdClient :: String -> IO DogStatsdClient
dogStatsdClient url = (fromURI . fromJust . parseURI) url

dogStatsdClient' :: Hostname -> Port -> String -> Maybe Key -> IO DogStatsdClient
dogStatsdClient' = client

increment :: DogStatsdClient -> Stat -> Tags -> IO ()
increment client stat = count client stat 1

decrement :: DogStatsdClient -> Stat -> Tags -> IO ()
decrement client stat = count client stat (-1)

count :: DogStatsdClient -> Stat -> Int -> Tags -> IO ()
count client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Count tags

gauge :: DogStatsdClient -> Stat -> Int -> Tags -> IO ()
gauge client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Gauge tags

timing :: DogStatsdClient -> Stat -> Millisecond -> Tags -> IO ()
timing client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat (fromIntegral value) Timing tags

histogram :: DogStatsdClient -> Stat -> Int -> Tags -> IO ()
histogram client stat value tags = void . send client $ fmtDogStatsdDatagram (getNamespace client) stat value Histogram tags

fmtDogStatsdDatagram :: Stat -> Stat -> Int -> Type -> Tags -> String
fmtDogStatsdDatagram namespace stat value stat_type [] = fmtDatagram namespace stat value stat_type
fmtDogStatsdDatagram namespace stat value stat_type tags =
  let statsdDatagram = fmtDatagram namespace stat value stat_type
      fmtTag x = (fst x) ++ ":" ++ (snd x)
      tagSuffix = intercalate "," (map fmtTag tags)
  in printf "%s#%s" statsdDatagram tagSuffix
