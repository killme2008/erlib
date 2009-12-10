-module(my_behaviour).  
-export([behaviour_info/1]).  
-export([start/1, stop/0]).  
behaviour_info(callbacks) ->  
     [{init,1},  
      {handle, 2}];  
behaviour_info(_Other) ->  
     undefined.  
   
start(Mod) ->  
     State = Mod:init(0),  
     {ok, State2} = Mod:handle(add,  State),  
     io:format("state :~p~n", [State2]).  
stop() ->  
     stop. 
