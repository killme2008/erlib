-module(minority_reader).

-export([start_link/0]).
-export([init/1]).

-record(command,{opcode,data_id_len,data_num_id_len,body_len,data_id,data_num_id,body,persistence,notify,reserved}).
-record(v1, {sock, callback, recv_ref, connection_state,current_cmd}).

-define(HEADER_LEN,16).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self()])}.

init(Parent) ->
    Deb = sys:debug_options([]),
    receive
        {go, Sock} -> start_connection(Parent, Deb, Sock)
    end.

peername(Sock) ->   
    try
        {Address, Port} = inet_op(fun () -> minority_networking:peername(Sock) end),
        AddressS = inet_parse:ntoa(Address),
        {AddressS, Port}
    catch
        Ex -> app_log:error("error on TCP connection ~p:~p~n",
                               [self(), Ex]),
              app_log:info("closing TCP connection ~p", [self()]),
              exit(normal)
    end.

start_connection(Parent, Deb, ClientSock) ->
    process_flag(trap_exit, true),
    {PeerAddressS, PeerPort} = peername(ClientSock),
    try 
        app_log:info("starting TCP connection ~p from ~s:~p~n",
                        [self(), PeerAddressS, PeerPort]),
        %Ready to read 12-bytes header
        mainloop(Parent, Deb, switch_callback(
                                #v1{sock = ClientSock,
                                    callback = uninitialized_callback,
                                    recv_ref = none,
                                    connection_state = pre_init},
                                header, ?HEADER_LEN))
    catch
        Ex -> (if Ex == connection_closed_abruptly ->
                       fun app_log:warning/2;
                  true ->
                       fun app_log:error/2
               end)("exception on TCP connection ~p from ~s:~p~n~p~n",
                    [self(), PeerAddressS, PeerPort, Ex])
    after
        app_log:info("closing TCP connection ~p from ~s:~p~n",
                        [self(), PeerAddressS, PeerPort])
        
    end,
    done.

switch_callback(OldState, NewCallback, Length) ->
    Ref = inet_op(fun () -> minority_networking:async_recv(
                              OldState#v1.sock, Length, infinity) end),
    OldState#v1{callback = NewCallback,
                recv_ref = Ref}.

mainloop(Parent, Deb, State = #v1{sock= Sock, recv_ref = Ref}) ->
    receive
        {inet_async, Sock, Ref, {ok, Data}} ->
             {State1, Callback1, Length1} =
                handle_input(State#v1.callback, Data,
                             State#v1{recv_ref = none}),
            mainloop(Parent, Deb,
                     switch_callback(State1, Callback1, Length1));
        {inet_async, Sock, Ref, {error, closed}} ->
            if State#v1.connection_state =:= closed ->
                    State;
               true ->
                    throw(connection_closed_abruptly)
            end;
        {inet_async, Sock, Ref, {error, Reason}} ->
            throw({inet_error, Reason});
        {'EXIT', Parent, Reason} ->
            if State#v1.connection_state =:= running ->
                ok
            end,
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            mainloop(Parent, Deb, handle_dependent_exit(Pid, Reason, State));
        terminate_connection ->
            State;
        timeout ->
            throw({timeout, State#v1.connection_state});
        Other ->
            %% internal error -> something worth dying for
            exit({unexpected_message, Other})
    end.
%%Process header
handle_input(header,<<16#80:8,OpCode:8,DataIdLen:16,DataNumIdLen:16,Persistence:8,Notify:8,Reserved:32,TotalBodyLen:32>>,
                   State) ->
    Cmd=#command{opcode=OpCode,data_id_len=DataIdLen,data_num_id_len=DataNumIdLen,body_len=0,persistence=Persistence,
                 notify=Notify,reserved=Reserved},
    case TotalBodyLen of
         0 ->
             handle_msg(Cmd),
             {State#v1{current_cmd=none},header,?HEADER_LEN};
	 Value when Value >0 andalso DataIdLen >=0 andalso DataNumIdLen >= 0 ->
             BodyLen=Value-DataNumIdLen-DataIdLen,
             {State#v1{current_cmd=Cmd#command{body_len=BodyLen}},body,TotalBodyLen};   
	  _Any ->
             exit({error_command})
    end;

handle_input(header,Data,State)->
    exit({header,Data,State});
    
%%Process body
handle_input(body, Data, State=#v1{current_cmd=#command{data_id_len=DataIdLen,data_num_id_len=DataNumIdLen,body_len=BodyLen}} )-> 
    <<DataId:DataIdLen/binary,DataNumId:DataNumIdLen/binary,Body:BodyLen/binary>> = Data,
    Cmd=State#v1.current_cmd,
    {ok,Response}=handle_msg(Cmd#command{data_id=DataId,data_num_id=DataNumId,body=Body}),
    {State#v1{current_cmd=none},header,?HEADER_LEN};
handle_input(body,Data,State)->
   exit({header,Data,State});

handle_input(CallBack,Data,State) ->
    exit({CallBack,Data}).


handle_msg(#command{opcode=16#01,data_id= DataId,data_num_id= DataNumId,body=Body,persistence=Persistence,notify=Notify})->
    io:format("store ~p ~p value:~p persistence:~p,notify:~p ~n",[binary_to_list(DataId),binary_to_list(DataNumId),Body,Persistence,Notify]),
    {ok,response};
handle_msg(#command{opcode=16#00,data_id= DataId,data_num_id=DataNumId}) ->
    io:format("get command ~p ~p ~n",[DataId,DataNumId]),
    {ok,response};
handle_msg(#command{opcode=16#03}) ->
    io:format("heart beat~n"),
    {ok,response};
handle_msg(#command{opcode=16#04,data_id =DataId}) ->
    io:format("version ~p~n",[DataId]),
    {ok,response};
handle_msg(#command{opcode=16#05,data_id=DataId,data_num_id_len=DataNumIdLen,data_num_id=DataNumId}) ->
    io:format("delete command ~p ~p~n",[DataId,DataNumId]),
    {ok,response};
handle_msg(Cmd)->
    exit({unknow_command,Cmd}).
handle_dependent_exit(Pid, Reason, State)->
    ok.

tcp_send(Sock, Data) ->
    minority_misc:throw_on_error(inet_error,
                               fun () -> minority_networking:send(Sock, Data) end).
inet_op(F) -> minority_misc:throw_on_error(inet_error, F).
