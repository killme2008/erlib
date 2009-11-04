%%%-------------------------------------------------------------------
%%% File    : analyse_node.erl
%%% Author  : dennis zhuang <killme2008@gmail.com>
%%% Description :
%%%
%%% Created :  4 Nov 2009 by dennis zhuang <killme2008@gmail.com>
%%%-------------------------------------------------------------------
-module(analyse_node).
-author("dennis zhuang").
-vsn('0.1').
-behaviour(gen_server).
%%%Trace log
-ifdef(debug).
-define(TRACE(X),io:format("DEBUG:~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(TRACE(X),true).
-endif.

%% API
-export([start/0,grep/2,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
%-define(SERVER,"log_analyse").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
grep(File,MsgId)->
    ?TRACE("cmd call: grep " ++ MsgId ++ " " ++ File),
    gen_server:call(?MODULE,{grep,File,MsgId}).
stop()->
    gen_server:call(?MODULE,{stop}).
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    {ok, #state{}}.

%%------------------------------x--------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({grep,File,MsgId}, _From, State) ->
    Cmd="grep " ++ MsgId ++ " " ++  File,
    ?TRACE(Cmd),
    Reply = os:cmd(Cmd),
    ?TRACE("Reply:" ++ Reply),
    {reply, Reply, State};
handle_call({stop},_,State)->
    {stop,normal,stopped,State};
handle_call(_,From,State) ->
    {reply,unknow_request,State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------


