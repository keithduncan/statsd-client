module Network.Statsd.Cluster (
  Cluster,
  cluster,

  Collector,
  collector,

  Network.Statsd.Cluster.increment,
) where

import Network.Statsd as Statsd
import Data.Digest.Pure.CRC32
import qualified Data.ByteString.Char8 as BC

data Cluster = Cluster { getCollectors :: [Collector] }
cluster :: [Collector] -> Cluster
cluster = Cluster

data Collector = Collector { getClient :: StatsdClient }
collector :: Hostname -> Port -> Stat -> Maybe String -> IO Collector
collector host port stat key = do
  client <- client host port stat key
  return $ Collector client

increment :: Cluster -> Stat -> IO ()
increment cluster stat = let collector = collectorForStat cluster stat
                          in Statsd.increment (getClient collector) stat

collectorForStat :: Cluster -> Stat -> Collector
collectorForStat (Cluster collectors) stat = let statBytes = BC.pack stat
                                                 idx = fromIntegral (crc32 statBytes) `mod` length collectors
                                              in collectors !! idx
