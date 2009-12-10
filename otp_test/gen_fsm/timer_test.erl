-module(timer_test).
-behaviour(gen_fsm).
-export([start_link/0,test/0]).
-export([run/2,init/1,handle_event/2,terminate/3]).

start_link()->
    gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).
test()->
    gen_fsm:send_event(?MODULE,test).
init(_Args)->
    {ok,run,data}.
run({timeout,_Ref,_Msg},State)->
   io:format("timer timeout~n"),
   {next_state,run,State};
run(test,State)->
   %gen_fsm:start_timer(1000,dummy),
   {next_state,run,State,20000};
run(timeout,State) ->
   io:format("state timeout~n"),
   {next_state,run,State}.

handle_event(stop,State)->
   {stop,normal,State}.

terminate(stop,normal,_State)->
    ok.
    
