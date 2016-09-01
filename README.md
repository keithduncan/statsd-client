## Statsd Client

A port of [github/statsd-ruby](http://github.com/github/statsd-ruby) to Haskell.

### Implements

- IPv4 and IPv6 support
- UDP statsd protocol
- UDP dogstatsd protocol (support for Datadog features like tags)
- Metric types
  - Count
  - Gauge
  - Timing
  - Histogram
- Collector clustering, stats are consistently routed using CRC32
- Packet signing using shared secret HMAC SHA256 signatures

### Examples

Run this helper script to listen to local UDP traffic:

``` sh
ruby script/listen.rb
```

Start sending metrics

``` haskell
import Network.Statsd

-- error handling omitted
client <- statsdClient "statsd://127.0.0.1:8125/prefix"

increment client "requests"
histogram client "numOfFilesCreated" 5

-- error handling omitted
client' <- statsdClient "statsd://:mysecret@127.0.0.1:8125/prefix"

increment client' "requests"
histogram client' "numOfFilesCreated" 5
```

Will result in these UDP packets being sent and received:

```
[#<UDPSocket:fd 9>, ["prefix.requests:1|c", #<Addrinfo: 127.0.0.1:64257 UDP>, 0]]
[#<UDPSocket:fd 9>, ["prefix.numOfFilesCreated:5|h", #<Addrinfo: 127.0.0.1:64257 UDP>, 0]]

[#<UDPSocket:fd 9>, ["\xD7\x8D?a9\xCB\xD5H\xFB\xA2?\xFE\x18\xFF9P\xB3\x9EY\e\xD9H\x86H\xEC\x84(N7B\xF7\x85\x04\\mV\x00\x00\x00\x00\x17\xFEq\x90prefix.requests:1|c", #<Addrinfo: 127.0.0.1:56756 UDP>, 0]]
[#<UDPSocket:fd 9>, ["A\x94\a\xBD\x8C9\x00\tU\xD0\x87\x8F\x9A\xCD\xC5\xA2l\xCA&\xA7T7\xEFT8\x9F\\b\xF7\xEB\x10\xD2\x06\\mV\x00\x00\x00\x00j\x86\xCD\xCBprefix.numOfFilesCreated:5|h", #<Addrinfo: 127.0.0.1:56756 UDP>, 0]]
```

 Send metrics to dogstatsd:

 ``` haskell
import Network.DogStatsd

-- error handling omitted
client <- dogStatsdClient "statsd://127.0.0.1:8125/prefix"

-- Don't send any tags
increment client "requests" []
-- Tags are key value pairs, omit the value to send a named tag
histogram client "numOfFilesCreated" 5 [("os", "mac"), ("tag2","")]
```

```
[#<UDPSocket:fd 9>, ["prefix.requests:1|c", #<Addrinfo: 127.0.0.1:64257 UDP>, 0]]
[#<UDPSocket:fd 9>, ["prefix.numOfFilesCreated:5|h|#os:mac,tag2", #<Addrinfo: 127.0.0.1:64257 UDP>, 0]]
```
