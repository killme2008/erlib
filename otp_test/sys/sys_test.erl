-module(sys_test).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3]).

start_link() ->
    proc_lib:start_link(sys_test, init, [self()]).

alloc() ->
    sys_test ! {self(), alloc},
    receive
        {sys_test, Res} ->
            Res
    end.

channels()->
    lists:duplicate(10,x).

alloc([])->
    {none,[]};
alloc([H|Rest]) ->
    {H,Rest}.
free(Ch,Chs)->
    [Ch|Chs].

free(Ch) ->
    sys_test ! {free, Ch},
    ok.

init(Parent) ->
    register(sys_test, self()),
    Chs = channels(),
    Deb = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Chs, Parent, Deb).

loop(Chs, Parent, Deb) ->
    receive
        {From, alloc} ->
            Deb2 = sys:handle_debug(Deb, {sys_test, write_debug},
                                    sys_test, {in, alloc, From}),
            {Ch, Chs2} = alloc(Chs),
            From ! {sys_test, Ch},
            Deb3 = sys:handle_debug(Deb2, {sys_test, write_debug},
                                    sys_test, {out, {sys_test, Ch}, From}),
            loop(Chs2, Parent, Deb3);
        {free, Ch} ->
            Deb2 = sys:handle_debug(Deb, {sys_test, write_debug},
                                    sys_test, {in, {free, Ch}}),
            Chs2 = free(Ch, Chs),
            loop(Chs2, Parent, Deb2);

        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent,
                                  sys_test, Deb, Chs)
    end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, Parent, Deb, Chs) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).
