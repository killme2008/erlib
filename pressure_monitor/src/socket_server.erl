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
           acceptor=null,
           sessions=[],
           lsocket=null}).

start(Name, Port, M) ->
       State = #server_state{port = Port, module = M},
       gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
       process_flag(trap_exit,true),
       case gen_tcp:listen(Port, ?TCP_OPTIONS) of
           {ok, LSocket} ->
               ?INFO("Starting server.."),
               NewState = State#server_state{lsocket = LSocket},
               {ok, accept(NewState)};
           {error, Reason} ->
               {stop, Reason}
       end.

handle_cast({accepted, _Pid}, State=#server_state{sessions=Sessions}) ->
       %add new process to state
       NewState = State#server_state{sessions=[_Pid|Sessions]},
       ?DEBUG(NewState),
       {noreply, accept(NewState)}.

accept_loop({Server, LSocket, M}) ->
       {ok, Socket} = gen_tcp:accept(LSocket),
       % spawn a new process to accept
       gen_server:cast(Server, {accepted, self()}),
       % initialize
       State=M:init(Socket),
       M:loop(State,Socket).

   % To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, module = M}) ->
       Pid=proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, M}]),
       State#server_state{acceptor=Pid}.

handle_call(stop,_Caller,State =#server_state{lsocket=LSocket, module=M,sessions=Sessions}) ->
       ?DEBUG("close server..."),
       gen_tcp:close(LSocket),
       M:terminate(),
       %close all sessions
       lists:foreach(fun(_Pid)->
                  _Pid ! {stop,self()} end,Sessions),
       {stop,normal,stopped,State};
handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, #server_state{lsocket=LSocket}) ->
     gen_tcp:close(LSocket),
     ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
handle_info({'EXIT', Pid, normal},
            State=#server_state{acceptor=Pid}) ->
    ?INFO("normal acceptor down~n"),
    {noreply, accept(State)};
handle_info({'EXIT', Pid, Reason},
            State=#server_state{acceptor=Pid}) ->
    error_logger:error_report({?MODULE, ?LINE,
                               {acceptor_error, Reason}}),
    timer:sleep(100),
    {noreply, accept(State)};
handle_info({'EXIT', _LoopPid, Reason},
            State=#server_state{acceptor=Pid, sessions=Sessions}) ->
    case Reason of
        normal ->
            ok;
        _ ->
            error_logger:error_report({?MODULE, ?LINE,
                                       {child_error, Reason}})
    end,
    ?DEBUG2("Delete session",_LoopPid),
    State1 = State#server_state{sessions=lists:delete(_LoopPid,Sessions)},
    State2 = case Pid of
                 null ->
                     accept(State1);
                 _ ->
                     State1
             end,
    {noreply, State2};
handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.
