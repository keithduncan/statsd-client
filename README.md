## Statsd Client

A port of [github/statsd-ruby](http://github.com/github/statsd-ruby) to Haskell.

### Implements

- IPv4 and IPv6 support
- UDP statsd protocol
- Metric types
  - Count
  - Gauge
  - Timing
  - Histogram
- Collector clustering, stats are consistently routed using CRC32
- Packet signing using shared secret HMAC SHA256 signatures

### Examples

```haskell
import Network.Statsd
import Network.URI
import Data.Maybe (fromJust)

-- error handling omitted
client <- fromJust <$> (fromURI . fromJust . parseURI) "statsd://:mysecretkey@localhost:8126/prefix"

increment client "requests"
histogram client "numOfFilesCreated" 5
```
