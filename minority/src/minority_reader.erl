-module(minority_reader).

-export([start_link/0]).

start_link()->
    io:format("client starting.."),
    receive
     {go,Sock}->
	    
           io:format("Received socket")
    end.
