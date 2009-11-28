require 'socket'      # Sockets are in standard library
require 'rubygems'
require 'bit-struct'
class Request < BitStruct
    unsigned    :magic,     8,    "Magic"
    unsigned    :opcode,    8,     "OpCode"
    unsigned    :data_id_len,   16,     "Data id length"
    unsigned    :data_num_id_len,  16,     "DataNum Id length"
    unsigned    :reserved,   48,     "reserved"
    unsigned    :total_body_length,  32,     "Total body length"
    rest        :body,            "Body of message"
end

hostname = 'localhost'
port = 8000

s = TCPSocket.open(hostname, port)

req=Request.new
req.magic=0x80
req.opcode=0x00
req.data_id_len=4
req.data_num_id_len=4
req.total_body_length=13
req.body="testtesthello"
p req.inspect
s.write(req)
s.close               # Close the socket when done