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
send client stat value stat_type = undefined

data Type = Count | Guage | Timing | Histogram

instance Show Type where
  show Count = "c"
  show Guage = "g"
  show Timing = "ms"
  show Histogram = "h"