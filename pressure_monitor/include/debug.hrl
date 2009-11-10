-ifdef(debug).
-define(DEBUG(X),io:format("DEBUG ~p:~p ~p ~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X),true).
-endif.
-define(ERROR(Msg,Why),io:format("ERROR ~p:~p ~p ~p ~n",[?MODULE,?LINE,Msg,Why])).
-define(INFO(X),io:format("INFO ~p:~p ~p ~n",[?MODULE,?LINE,X])).