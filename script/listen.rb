# Simple test script to listen to UDP traffic.

require "socket"

listener= UDPSocket.new.tap do |s|
  s.bind("0.0.0.0", 8125)
end
loop do
  rd, _ = IO.select([listener])
  rd.each { |r| p [r, r.recvmsg] }
end
