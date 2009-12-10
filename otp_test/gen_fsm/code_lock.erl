-module(code_lock).
-behaviour(gen_fsm).

-export([start_link/1,handle_event/3,terminate/3]).
-export([button/1,stop/0,lock/0]).
-export([init/1, locked/2, open/2]).

start_link(Code) ->
    gen_fsm:start_link({local, code_lock}, code_lock, Code, []).

button(Digit) ->
    gen_fsm:send_event(code_lock, {button, Digit}).
stop()->
    gen_fsm:send_all_state_event(code_lock,stop).

lock()->
    gen_fsm:send_all_state_event(code_lock,lock).

init(Code) ->
    {ok, locked, {[], Code},hibernate}.
locked({button, Digit}, {SoFar, Code}) ->
    Pattern=[Digit|SoFar],
    io:format("~p ~p ~n",[Pattern,Code]),
    case [Digit|SoFar] of
        Code ->
            do_unlock(),
            gen_fsm:start_timer(3000,lock),
            {next_state, open, {[], Code}};
        Incomplete when length(Incomplete)<length(Code) ->
            {next_state, locked, {Incomplete, Code}};
        _Wrong ->
            {next_state, locked, {[], Code}}
    end.

open({timeout,_Ref,lock}, State) ->
    do_lock(),
    {next_state, locked, State,hibernate}.
handle_event(lock,StateName,{_SoFar,Code})->
    {next_state,locked,{[],Code}};
handle_event(stop,_StateName,StateData)->
    {stop,normal,StateData}.
terminate(normal,_StateName,_StateData)->
    ok.

do_unlock()->
    io:format("unlock~n").
do_lock()->
    io:format("lock~n").
