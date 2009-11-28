-module(minority_app).
-author("boyan (boyan@taobao.com").

-export([start/2,start/0]).

start()->
    start("localhost",8000).
start(Host,Port)->
   {ok, SupPid} = minority_sup:start_link(),
    ok = minority_networking:start(),
   ok = minority_networking:start_tcp_listener(Host, Port),
   {ok,SupPid}.
