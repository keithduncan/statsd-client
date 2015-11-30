module Network.Statsd.Cluster (
  Cluster,
  cluster,

  Collector,
  collector,

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

data Cluster = Cluster { getCollectors :: [Collector] }
cluster :: [Collector] -> Cluster
cluster = Cluster

data Collector = Collector { getClient :: StatsdClient }
collector :: Hostname -> Port -> Stat -> Maybe String -> IO Collector
collector host port stat key = do
  client <- client host port stat key
  return $ Collector client

increment :: Cluster -> Stat -> IO ()
increment cluster stat = Statsd.increment (getClient $ collectorForStat cluster stat) stat

decrement :: Cluster -> Stat -> IO ()
decrement cluster stat = Statsd.decrement (getClient $ collectorForStat cluster stat) stat

count :: Cluster -> Stat -> Int -> IO ()
count cluster stat = Statsd.count (getClient $ collectorForStat cluster stat) stat

gauge :: Cluster -> Stat -> Int -> IO ()
gauge cluster stat = Statsd.gauge (getClient $ collectorForStat cluster stat) stat

timing :: Cluster -> Stat -> Millisecond -> IO ()
timing cluster stat = Statsd.timing (getClient $ collectorForStat cluster stat) stat

histogram :: Cluster -> Stat -> Int -> IO ()
histogram cluster stat = Statsd.histogram (getClient $ collectorForStat cluster stat) stat

collectorForStat :: Cluster -> Stat -> Collector
collectorForStat (Cluster collectors) stat = let statBytes = BC.pack stat
                                                 idx = fromIntegral (crc32 statBytes) `mod` length collectors
                                              in collectors !! idx
