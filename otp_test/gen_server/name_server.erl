%%%-------------------------------------------------------------------
%%% File    : name_server.erl
%%% Author  :  <>
%%% Description : 
%%%
%%% Created : 28 Nov 2009 by  <>
%%%-------------------------------------------------------------------
-module(name_server).

-behaviour(gen_server).

%% API
-export([new/1]).
-export([get/2,put/3,delete/2,size/1,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%test
-export([call/2]).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
new(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

call(Msg,S)->
    S! {'$gen_call',{self(),erlang:monitor(process,S)},Msg},
    receive
       Any->
	    Any
   end.

get(Key,S)->
    gen_server:call(S,{find,Key}).
put(Key,Value,S)->
    gen_server:call(S,{store,Key,Value}).
delete(Key,S)->
    gen_server:call(S,{delete,Key}).
size(S)->
    gen_server:call(S,size).
stop(S)->
    gen_server:cast(S,stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({store,Key,Value},_From,Dict)->
   {reply,ok,dict:store(Key,Value,Dict)};
handle_call({find,Key},_From,Dict)->
   {reply,dict:find(Key,Dict),Dict};
handle_call(size,_From,Dict)->
   {reply,dict:size(Dict),Dict};
handle_call({delete,Key},_From,Dict)->
   {reply,ok,dict:erase(Key,Dict)};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop,State)->
   {stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------

