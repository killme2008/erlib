-module(minority_networking).
-author("boyan (boyan@taobao.com)").
-define(TCP_OPTS, [
        binary, 
        {packet, raw}, % no packaging 
        {reuseaddr, true}, % allow rebind without waiting 
        {nodelay, true}, % TCP_NODELAY - disable Nagle's alg.  
        %% {delay_send, true}, 
        {exit_on_close, false}
    ]).

-export([start/0]).
-export([start_tcp_listener/2,stop_tcp_listener/2,tcp_listener_started/2,tcp_listener_stopped/2]).

-export([start_client/1]).

start() ->
    {ok,_} = supervisor:start_child(
               minority_sup,
               {tcp_client_sup,
                {tcp_client_sup, start_link,
                 [{local, tcp_client_sup},
                  {minority_reader,start_link,[]}]},
                transient, infinity, supervisor, [tcp_client_sup]}),
    ok.

start_tcp_listener(Host, Port) ->
    start_listener(Host, Port, "TCP Listener",
                   {?MODULE, start_client, []}).

start_listener(Host, Port, Label, OnConnect) ->
    {IPAddress, Name} =
        check_tcp_listener_address(tcp_listener_sup, Host, Port),
    {ok,_} = supervisor:start_child(
               minority_sup,
               {Name,
                {tcp_listener_sup, start_link,
                 [IPAddress, Port, ?TCP_OPTS ,
                  {?MODULE, tcp_listener_started, []},
                  {?MODULE, tcp_listener_stopped, []},
                  OnConnect, Label]},
                transient, infinity, supervisor, [tcp_listener_sup]}),
    ok.

stop_tcp_listener(Host, Port) ->
    {ok, IPAddress} = inet:getaddr(Host, inet),
    Name = minority_misc:tcp_name(tcp_listener_sup, IPAddress, Port),
    ok = supervisor:terminate_child(minority_sup, Name),
    ok = supervisor:delete_child(minority_sup, Name),
    ok.

tcp_listener_started(IPAddress, Port) ->
    ok.
tcp_listener_stopped(IPAddress, Port) ->
    ok.
start_client(Sock) ->
    {ok, Child} = supervisor:start_child(tcp_client_sup, []),
    ok = controlling_process(Sock, Child),
    Child ! {go, Sock},
    Child.
%%工具方法
controlling_process(Sock, Pid) when is_port(Sock) ->
    gen_tcp:controlling_process(Sock, Pid).

check_tcp_listener_address(NamePrefix, Host, Port) ->
    IPAddress =
        case inet:getaddr(Host, inet) of
            {ok, IPAddress1} -> IPAddress1;
            {error, Reason} ->
                error_logger:error_msg("invalid host ~p - ~p~n",
                                       [Host, Reason]),
                throw({error, {invalid_host, Host, Reason}})
        end,
    if is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) -> ok;
       true -> error_logger:error_msg("invalid port ~p - not 0..65535~n",
                                      [Port]),
               throw({error, {invalid_port, Port}})
    end,
    Name = minority_misc:tcp_name(NamePrefix, IPAddress, Port),
    {IPAddress, Name}.
