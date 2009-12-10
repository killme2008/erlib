-module(use_my_behaviour).  
-behaviour(my_behaviour).  
%%behaviour callback function  
-export([init/1,  handle/2]).  
   
init(State) ->  
       io:format("init ~p~n", [State]),  
       State.  
   
handle(Request, State) ->  
       io:format("handle request:~p state:~p~n", [Request, State]),  
       State2 = State + 1,  
       {ok, state2}. 
