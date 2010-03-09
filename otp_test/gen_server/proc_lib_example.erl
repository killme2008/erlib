-module(proc_lib_example).

-export([start/0,start2/0,loop/0]).
start()->
    spawn(?MODULE,loop,[]).

start2()->
    proc_lib:spawn(?MODULE,loop,[]).

loop()->
    receive
      parent ->
	    io:format("~p~n",[get('$ancestors')]),
            loop();
      init ->
            io:format("~p~n",[get('$initial_call')]),
            loop();
 	Any ->
            error
    end.
