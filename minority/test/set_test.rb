require 'socket'      # Sockets are in standard library
require 'rubygems'
require 'bit-struct'
class Request < BitStruct
    unsigned    :magic,     8,    "Magic"
    unsigned    :opcode,    8,     "OpCode"
    unsigned    :data_id_len,   16,     "Data id length"
    unsigned    :data_num_id_len,  16,     "DataNum Id length"
    unsigned    :persistence,     8,    "Persistence"
    unsigned    :notify,          8,    "Notify immediately"
    unsigned    :reserved,   32,     "reserved"
    unsigned    :total_body_length,  32,     "Total body length"
    rest        :body,            "Body of message"
end

hostname = 'localhost'
port = 8000

s = TCPSocket.open(hostname, port)

req=Request.new
req.magic=0x80
req.opcode=0x01
req.data_id_len=4
req.data_num_id_len=4
req.persistence=1
req.notify=1
req.total_body_length=13
req.body="testtesthello"
p req.inspect
s.write(req)

req=Request.new
req.magic=0x80
req.opcode=0x00
req.data_id_len=4
req.data_num_id_len=0
req.persistence=0
req.notify=0
req.total_body_length=9
req.body="testhello"
s.write(req)

req=Request.new
req.magic=0x80
req.opcode=0x03
req.data_id_len=0
req.data_num_id_len=0
req.persistence=0
req.notify=0
req.total_body_length=0
p req
s.write(req)

req=Request.new
req.magic=0x80
req.opcode=0x04
req.data_id_len=4
req.data_num_id_len=0
req.persistence=0
req.notify=0
req.total_body_length=4
req.body="test"
p req
s.write(req)

req=Request.new
req.magic=0x80
req.opcode=0x05
req.data_id_len=4
req.data_num_id_len=4
req.persistence=0
req.notify=0
req.total_body_length=8
req.body="testtest"
p req
s.write(req)


s.close               # Close the socket when done