module Network.Statsd.Cluster (
  Cluster,
  cluster,

  Network.Statsd.Cluster.increment,
  Network.Statsd.Cluster.decrement,
  Network.Statsd.Cluster.count,
  Network.Statsd.Cluster.gauge,
  Network.Statsd.Cluster.timing,
  Network.Statsd.Cluster.histogram,
) where

import Network.Statsd as Statsd
import Data.Digest.Pure.CRC32
import qualified Data.ByteString.Char8 as BC
import Data.Time.Units

data Cluster = Cluster { getCollector :: [StatsdClient] }
cluster :: [StatsdClient] -> Cluster
cluster = Cluster

increment :: Cluster -> Stat -> IO ()
increment cluster stat = Statsd.increment (collectorForStat cluster stat) stat

decrement :: Cluster -> Stat -> IO ()
decrement cluster stat = Statsd.decrement (collectorForStat cluster stat) stat

count :: Cluster -> Stat -> Int -> IO ()
count cluster stat = Statsd.count (collectorForStat cluster stat) stat

gauge :: Cluster -> Stat -> Int -> IO ()
gauge cluster stat = Statsd.gauge (collectorForStat cluster stat) stat

timing :: Cluster -> Stat -> Millisecond -> IO ()
timing cluster stat = Statsd.timing (collectorForStat cluster stat) stat

histogram :: Cluster -> Stat -> Int -> IO ()
histogram cluster stat = Statsd.histogram (collectorForStat cluster stat) stat

collectorForStat :: Cluster -> Stat -> StatsdClient
collectorForStat (Cluster collectors) stat = let statBytes = BC.pack stat
                                                 idx = fromIntegral (crc32 statBytes) `mod` length collectors
                                              in collectors !! idx
