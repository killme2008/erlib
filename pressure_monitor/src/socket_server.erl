 -module(socket_server).
 -include("debug.hrl"). 
 -author('boyan (boyan@taobao.com)').
 -vsn('0.1').  
 -behavior(gen_server).  
      
 -export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).  
 -export([accept_loop/1]).  
 -export([start/3]).  
      
 -define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).  
     
 -record(server_state, {  
           port,  
           module,  
           ip=any,  
           sessions=[],
           lsocket=null}).  
     
 start(Name, Port, M) ->  
       State = #server_state{port = Port, module = M},  
       gen_server:start_link({local, Name}, ?MODULE, State, []).  
     
 init(State = #server_state{port=Port}) ->  
       case gen_tcp:listen(Port, ?TCP_OPTIONS) of  
           {ok, LSocket} ->  
               ?INFO("Starting server.."),
               NewState = State#server_state{lsocket = LSocket},  
               {ok, accept(NewState)};  
           {error, Reason} ->  
               {stop, Reason}  
       end.  
     
 handle_cast({accepted, _Pid}, State=#server_state{sessions=Sessions}) ->  
       NewState = State#server_state{sessions=[_Pid|Sessions]},
       ?DEBUG(NewState),
       {noreply, accept(NewState)}.  
     
 accept_loop({Server, LSocket, M}) ->  
       {ok, Socket} = gen_tcp:accept(LSocket),  
       % Let the server spawn a new process and replace this loop  
       % with the echo loop, to avoid blocking  
       gen_server:cast(Server, {accepted, self()}),
       % config socket
       State=M:init(Socket),
       M:loop(State,Socket).  
        
   % To be more robust we should be using spawn_link and trapping exits  
 accept(State = #server_state{lsocket=LSocket, module = M}) ->  
       proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, M}]),  
       State.  
     
   % These are just here to suppress warnings.
 handle_call(stop,_Caller,State =#server_state{lsocket=LSocket, module=M,sessions=Sessions}) ->
       ?DEBUG("close server..."),
       gen_tcp:close(LSocket),
       M:terminate(),
       %close all sessions
       lists:foreach(fun(_Pid)->
                  _Pid ! {stop,self()} end,Sessions),
       {stop,normal,stopped,State};         
 handle_call(_Msg, _Caller, State) -> {noreply, State}.  
 handle_info(_Msg, Library) -> {noreply, Library}.  
 terminate(_Reason, _Library) -> ok.  
 code_change(_OldVersion, Library, _Extra) -> {ok, Library}.  
