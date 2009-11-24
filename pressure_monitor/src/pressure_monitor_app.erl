%%%----------------------------------------------------------------------
%%%
%%% @copyright 2009 pressure monitor
%%%
%%% @author boyan@taobao.com
%%% @doc monitor app and supervisor callback
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(pressure_monitor_app).
-author('boyan@taobao.com').
-vsn('0.1').
-include("debug.hrl").

-behaviour(application).
-behaviour(supervisor).

-export([start/0,stop/0]).
-export([start/2, stop/1]).
-export([init/1]).


%% @doc start the application from the erl shell
-spec start() -> 'ok' | {'error', any()}.
start() ->
    ensure_apps(),
    application:start(pressure_monitor).
%% @doc stop the application from the erl shell
stop()->
    application:stop(pressure_monitor).
%% @doc the application start callback
-spec start(Type :: atom(), Args :: any()) ->
    'ignore' | {'ok', pid()} | {'error', any()}.
start(_Type, Args) ->
    error_logger:info_msg("starting pressure monitor server app"),
    supervisor:start_link({local, pressure_monitor_sup}, ?MODULE, Args).
%% @doc the application  stop callback
stop(_State) ->
    ok.

%% @doc supervisor callback
init(_Args) ->
    Stragegy = {one_for_one, 10, 10},
    {ok, {Stragegy, [
                    {monitor_server, {monitor_server, start, _Args},
                permanent, brutal_kill, worker, [monitor_server]}
                     ]}}.

%%
%% internal API
%%

%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    ok.
