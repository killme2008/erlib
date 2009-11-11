-module(monitor_server).
-vsn('0.1').
-author("boyan(boyan@taobao.com)").
-include("debug.hrl").
-define(TCP_OPTS, [binary,{active,true},{packet,line},{nodelay,true},{reuseaddr,true}]).
-export([loop/2,start/0,start/1,terminate/0,stop/0,init/1,test/0]).
% just for test
test()->
    {ok,Socket}=gen_tcp:connect("localhost",8000,?TCP_OPTS),
    gen_tcp:send(Socket,"init:6000:true:-1:1024:1024:1\n").
% start server at port 8000
start()->
    start(8000).
%start server at port
start(Port)->
    socket_server:start(?MODULE,Port,?MODULE).
%stop server
stop()->
    gen_server:call(?MODULE,stop).
%called when server is stopped
terminate()->
    ok.

% called when a new socket is accpetted
init(Socket)->
    ?INFO("Accept socket..."),
    inet:setopts(Socket, ?TCP_OPTS),
    {ok,[Confs]}=file:consult(config_file_path()),
    process_flag(trap_exit,true),
    lists:foldl(fun({Host,Port},T)->
                  case connect(Socket,Host,Port) of
                     error->
                          T;
                     Pid ->
                          [Pid|T]
                  end
                  end,[],Confs).
% Loop with socket
loop([],Socket)->
    ?INFO("There is no connected clients,close socket"),
    gen_tcp:close(Socket);
loop(List,Socket)->
     receive
       {tcp,Socket,Line}->
            %?DEBUG(<<"Recv:">> ++ Line),
            try handle_request(List,Line) of
                _ ->
                  ok
            catch
               _:Why->
                 ?ERROR("Handle request error",Why)
            end,
            loop(List,Socket);
       {tcp_closed,Socket}->
            ?INFO("Close socket"),
            ok;
       {stop,_}->
            lists:foreach(fun(_Pid)->
                     _Pid ! closed end,List),
            gen_tcp:close(Socket);
       {'EXIT',Pid,Reason}->
            NewList=lists:delete(Pid,List),
            loop(NewList,Socket);
      _->
            loop(List,Socket)

   end.

%%internal api
% connect to a host and spawn a process to monitor socket.
connect(Msocket,Host,Port)->
    case gen_tcp:connect(Host,Port,[binary,{packet,line}]) of
        {ok,Socket} ->
                          ?INFO("Connect to " ++ Host ++ " Success"),
                      Pid=spawn_link(fun()-> dispatch(Msocket,Socket) end),
                          % Change controlling process for this socket
                          gen_tcp:controlling_process(Socket,Pid),
                          Pid;
    {error,Reason} ->
                          ?ERROR("connect error",Reason),
                      error
    end.
% loop with connected socket
dispatch(Msocket,Socket)->
    receive
       {tcp,Socket,Bin}->
           ?DEBUG("recv response"),
           %response to main socket
           gen_tcp:send(Msocket,Bin),
           dispatch(Msocket,Socket);
       {tcp_closed,Socket}->
           ?DEBUG("Socket closed"),
           ok;
       closed ->
           %?DEBUG("Trying to close connected socket..."),
           gen_tcp:close(Socket),
           ok;
       {send,Line}->
           ?DEBUG("send " ++ binary_to_list(Line)),
           gen_tcp:send(Socket,Line),
       dispatch(Msocket,Socket);
    Any ->
           ?ERROR("Unknow command",Any)
    end.

%get config file path
config_file_path()->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/client_config").
%handle request from loadrunner
handle_request(List,Line = <<Begin:3/binary,_/binary>>)->
    case Begin of
        <<"ini">> ->
            ?DEBUG("init,send to all connections"),
            all_send(List,Line);
        <<"end">> ->
            all_send(List,Line);
    <<"sen">> ->
            random_send(List,Line);
    Any ->
            ?ERROR("Unknown command",Any)
    end;
handle_request(_,Any)->
    ?ERROR("Unknow command",Any).
%send message to all connected clients.
all_send(List,Line)->
    lists:foreach(fun(Pid)->
              Pid ! {send,Line} end,List).
%random send message to one client.
random_send(List,Line)->
     Pid=lists:nth(random:uniform(length(List)),List),
     Pid ! {send,Line}.
