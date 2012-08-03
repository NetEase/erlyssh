%%% -------------------------------------------------------------------
%%% Author  : CE
%%% Description :
%%%
%%% Created : 2009-1-2
%%% -------------------------------------------------------------------
-module(essh_agent).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
-export([init/1, connect/2, connected/2, connection_failed/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {server, primary, port}).
-define(SSH, "ssh ").
-define(MAGIC, "@[6769]@").
-define(ECHO, "echo @[6769]@").
-define(TIMEOUT, 60000).
-define(CMD_TIMEOUT, 300000).
%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([Server, Primary]) ->
    {ok, connect, #state{server = Server, primary = Primary}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
connect(_, State = #state{server = Server}) ->
    %io:format("INFO:before connecting ~p~n", [Server]),
    case open_ssh_port(Server, 0) of
        {ok, Port} ->
            State1 = State#state{port = Port},
            gen_server:call(essh, {res, Server, connected, ok}, infinity),
            {next_state, connected, State1};
        {failed, _Reason} ->
            gen_server:call(essh, {res, Server, connected, failed}, infinity),
            {next_state, connection_failed, State}
    end.

connected({cmd, C}, State = #state{server = Server, primary = Primary, port = Port}) ->
    send_cmd(Port, C),
    case receive_port_res(Primary, Port, []) of
        {ok, Res} ->
            gen_server:call(essh, {res, Server, responsed, Res}, infinity);
        {failed, Reason} ->
            gen_server:call(essh, {res, Server, failed, Reason}, infinity)
    end,
    {next_state, connected, State}.

connection_failed(restart, State) ->
    {next_state, connect, State};
connection_failed( _Cmd , State = #state{server = Server}) ->
    gen_server:call(essh, {res, Server, failed, inactive}),
    {next_state, connection_failed, State}.	

send_cmd(Port, C) ->
    %io:format("INFO:send command ~p~n", [C]),
    Port ! {self(), {command, lists:concat([C, "\n"])}},
    Port ! {self(), {command, "\n"}},
    Port ! {self(), {command, lists:concat(["echo", "\n"])}},
    Port ! {self(), {command, lists:concat([?ECHO, "\n"])}}.

receive_port_res(Primary, Port, RAcc) ->
    receive
        {Port, {data, {eol, ?MAGIC}}} ->            
            {ok, RAcc};
        {Port, {data, {eol, Line}}} ->
            if 
                %print primary server's response
                Primary =:= true ->
                    io:format("~s~n", [Line]);
            	true -> ok
            end,
            receive_port_res(Primary, Port, [Line|RAcc]);
    	{'EXIT', Port, Reason} ->
			{failed, Reason}
	after ?CMD_TIMEOUT ->
            {failed, cmd_timeout}
	end.

open_ssh_port(Server, Acc) ->
    if
        Acc < 3 ->
            Port = open_port({spawn, lists:concat([?SSH, Server])}, [{line, 4096}, stderr_to_stdout]),
            Port ! {self(), {command, lists:concat([?ECHO, "\n"])}},
            receive
                {Port, {data, {eol, ?MAGIC}}} ->
                    %io:format("INFO:~p connected ~n", [Server]),
                    {ok, Port};
                {'EXIT', Port, Reason} ->
                    io:format("ERROR:ssh port exit reason:~p~n", [Reason]),
                    {failed, Reason}		
            after	?TIMEOUT ->
                    io:format("ERROR:connecting to ~p timeout~n", [Server]),
                    Port ! {self(), close},
                    receive
                        {Port, closed} ->          
                            io:format("INFO:retry connecting to ~p after prev port closed~n", [Server]),
                            open_ssh_port(Server, Acc + 1)
                    after 10000 ->
                            io:format("INFO:retry connecting to ~p after prev port closing timeout~n", [Server]),
                            open_ssh_port(Server, Acc + 1)
                    end
            end;
        true ->
            {failed, "max connection retry reached"}
    end.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

