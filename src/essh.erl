%%% -------------------------------------------------------------------
%%% Author  : CE
%%% Description :
%%%
%%% Created : 2008-12-28
%%% -------------------------------------------------------------------
-module(essh).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(options, {con = 256, intv = 0}).
-record(state, {shell,pids = [], servers, cur = empty, opt = #options{}}).

%% ====================================================================
%% External functions
%% ====================================================================
%%
%% TODO: Add description of start/function_arity
%%
start(StartParas) -> 
    [SharedPath, FilePath | Command] = StartParas,
    StartCommand = get_start_command(Command, ""),    
    SharedLib = "readline_drv",
	%io:format("starting essh server~n"),    
    essh_scheduler:start(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [SharedPath, SharedLib, FilePath, StartCommand], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([SharedPath, SharedLib, FilePath, StartCommand]) -> 
    Servers = try get_server_lst(FilePath) of
					S -> S
			catch 
					_:X -> io:format("ERROR: can't open server list file~n"),halt()
			end,
    %io:format("INFO:servers = ~p~n", [Servers]),
    [Primary | _ ] = Servers,
    put(primary, Primary),
    %io:format("INFO:primary server = ~p~n", [Primary]),
   	Shell = case StartCommand of
                "" ->
                    spawn_link(fun() -> loop(SharedPath, SharedLib) end);
                _ ->
                    spawn_link(fun() -> non_interactive_loop(StartCommand) end)
            end,
    Pids = start_agents(Servers),
    call_ext(Pids, ""),
    {ok, #state{shell = Shell, pids = Pids, servers = Servers, cur = Servers}}.

get_start_command([C | CR], Cmd) ->
    get_start_command(CR, lists:concat([Cmd, C, " "]));
get_start_command([], Cmd) ->
  	Cmd.

get_server_lst(FilePath) ->
    {ok, IoDev} = file:open(FilePath, [read]),
    get_lst_from_file(IoDev, []).

get_lst_from_file(IoDev, L) ->
    case io:get_line(IoDev, "") of
        eof ->
            lists:reverse(L);
        Line ->            
            case get_server_line(Line) of
                undefined ->
                    get_lst_from_file(IoDev, L);
				CurLine ->
					get_lst_from_file(IoDev, [CurLine | L])
            end            
    end.         

get_server_line(Line) ->
    CurLine = string:strip(Line, both, $\n),
    if
		CurLine =:= "" ->
			undefined;
		true ->
			[ C0 | _] = CurLine,
			if
				C0 =:= $# ->
					undefined;
				true ->
					CurLine
			end
    end.

is_primary_server(S) ->
    P = get(primary),
    if 
        P =:= S ->
            true;
    	true ->
            false
    end.

start_agent_fsm(S) ->    
    IsP = is_primary_server(S),
    if
        IsP =:= true ->
            %io:format("start fsm of ~p as primary server~n", [S]),
            gen_fsm:start_link(essh_agent, [S, true], []);
        true ->
            gen_fsm:start_link(essh_agent, [S, false], [])
    end.
    
start_agents(Servers) ->
    Pids = [Pid || {ok, Pid}<- [start_agent_fsm(S) || S <- Servers]],
    %io:format("INFO:agents = ~p~n", [Pids]),
    Pids.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({shell_cmd, Cmd}, _From, State) ->
    %io:format("shell cmd received ~p~n", [Cmd]),    
	Cmd0 = string:strip(Cmd),
	process_input_cmd(State, Cmd0),		  
   	{reply, {cmd, continue}, State};

%%handle multipul ssh connection initialization
handle_call({res, Server, connected, Res}, _From, State)->
    StateN = get_processed_state(Server, Res, fun print_connect_info/2, {init, shell}, State),	
    %io:format("~p connected ok~n", [Server]),    
	{reply, ok, StateN};

%%handle command results
handle_call({res, Server, responsed, Res}, _From, State) ->
    gen_server:call(essh_scheduler, {continue}, infinity),
	%io:format("~p responsed: ~p~n", [Server, Res]),
    StateN = get_processed_state(Server, Res, fun print_cmd_res/2, done, State),			
	{reply, ok, StateN};
handle_call({res, Server, failed, Reason}, _From, State) ->
    gen_server:call(essh_scheduler, {continue}, infinity),
	io:format("~p failed: ~p~n", [Server, Reason]),
    StateN = get_processed_state(Server, Reason, fun print_cmd_res/2, done, State),	
	{reply, ok, StateN}.

%%-----------------------------------------------------------------
get_int_value(Value) ->
    Vl1 = string:strip(Value),
    try list_to_integer(Vl1) of
             Vl2 -> Vl2
         catch
			_ : X0 -> {error, X0}
		 end.

%%process shell commands
process_input_cmd(#state{shell = Shell}, "#con"++Value) ->
    Vl = get_int_value(Value),
    case Vl of 
        {error, Error} -> io:format("~p~n",[Error]);
        _ -> 
            gen_server:call(essh_scheduler, {opt, con, Vl}),
            gen_server:cast(essh, {opt, con, Vl})
    end,    
    Shell ! done;
process_input_cmd(#state{shell = Shell}, "#intv"++Value) ->
    Vl = get_int_value(Value),
    case Vl of 
        {error, Error} -> io:format("~p~n",[Error]);
        _ -> 
            gen_server:call(essh_scheduler, {opt, intv, Vl}),
            gen_server:cast(essh,  {opt, intv, Vl})
    end,    
    Shell ! done;
process_input_cmd(#state{shell = Shell}, "options;") ->
    gen_server:cast(essh, {opt, show}),
    Shell ! done;
process_input_cmd(#state{shell = Shell}, "exit") ->
    Shell ! done;
process_input_cmd(#state{shell = Shell},"") ->
    Shell ! done;
process_input_cmd(#state{shell = Shell},";") ->
    Shell ! done;
process_input_cmd(#state{shell = Shell},"!") ->
    Shell ! done;
process_input_cmd(_, "exit;") ->
    io:format("Thanks for using essh, bye.~n"),
	halt();		
process_input_cmd(#state{pids = Pids}, Cmd) ->
    io:format("--------------~s---------------~n", [get(primary)]),
  	call_ext(Pids, Cmd).		
%---------------------------------------------------------------------

get_processed_state(Server, Res, PrintMethod, DoneMessage,
	State = #state{shell = Shell, cur = CurServers, servers = Servers}) ->
	case process_cur_res(Server, CurServers, Res, PrintMethod) of
		[] ->
			clear_dict(),
			Shell ! DoneMessage,
			State#state{cur = Servers};
		Rest ->
			State#state{cur = Rest}
		end.

call_ext(Pids, Cmd) ->
    gen_server:call(essh_scheduler, {cmd, Pids, Cmd}).
   

clear_dict() ->
    PrimServer = get(primary),
    erase(),
    put(primary, PrimServer).

process_cur_res(Server, CurServers =  [Cur | RestServers], Res, PrintMethod) ->
    if 
        Server =/= Cur ->
            put(Server, {true, Res}),
            CurServers;            
    	true ->
            output_cmd_res(Cur, RestServers, Res, PrintMethod)
    end.	

output_cmd_res(Cur, Lst = [N | R], Res, PrintMethod) ->
    PrintMethod(Cur, Res),
    {NOk, NRes} = case V = get(N) of
                      {true, _} -> V;
                      _ -> {undefined, undefined}
                  end,
    if
        NOk =:= true ->
            output_cmd_res(N, R, NRes, PrintMethod);
    	true -> Lst
	end;
output_cmd_res(Cur, [], Res, PrintMethod) ->
    PrintMethod(Cur, Res),
	io:format("~n"),
    [].

do_print_cmd_res([]) ->	
    ok;
do_print_cmd_res([Line | Rest]) ->
    io:format("~s~n", [Line]),
    do_print_cmd_res(Rest).

print_cmd_res(S, Res) ->
    IsP = is_primary_server(S),
    if
        IsP =:= true ->
			put(last, Res),
            io:format("~n[primary server done]~n");
        true ->
            Last = get(last),
            if
                Res =:= Last ->
					put(identical, true),
                    io:format("->>");
                true ->
                    put(last, Res),
					Identical = get(identical),
					if
						Identical =:= true ->
							io:format("~n");
						true -> ok
					end,
					put(identical, false),
                    io:format("--------------~s---------------~n", [S]),
                    if
                        is_list(Res) ->
                            do_print_cmd_res(lists:reverse(Res));
                        true ->
                            io:format("~p~n", [Res])
                    end
            end
    end.

print_connect_info(Cur, Res) ->
    if
        Res =:= ok ->
            io:format("-----------------~s connected-------------------~n", [Cur]);
        true ->
            io:format("-----------------connect to ~s failed-------------------~n", [Cur])
    end.
    	
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%%handler for processing options setting
%% option <con> : limits of concurrent running process
handle_cast({opt, Opt, Value}, State = #state{opt = Options}) ->
    OptionsN = case Opt of
        con ->
            Options#options{con = Value};
        intv ->
            Options#options{intv = Value}               
    end,
    StateN = State#state{opt = OptionsN},
    {noreply, StateN}; 
handle_cast({opt, show}, State = #state{opt = Options}) ->
    io:format("~ninfo:options = ~p~n", [Options]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("essh received ~p~n", [Info]),
    {stop, "exit command received", State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
loop(SharedPath, SharedLib) ->
    receive 
        {init, shell} -> ok
    end,            
    %io:format("in loop ~p ~p~n", [SharedPath, SharedLib]),
    case erl_ddll:load_driver(SharedPath, SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> 
            io:format("Error:~p~n", [erl_ddll:format_error(ErrorDesc)]),
            exit(error)
    end,
    %io:format("driver loaded~n"),
    Port = open_port({spawn, SharedLib}, []),
	loop_cmd(Port).    

loop_cmd(Port) ->
    Port!{self(), {command, "start"}},
    receive
         {Port, {data, Data}} ->
            %io:format("received data ~p~n", [Data]),
            gen_server:call(essh, {shell_cmd, Data}),
            receive 
                done ->
                    ok
            end,
            loop_cmd(Port)
	end.

non_interactive_loop(Cmd) ->
    receive 
        {init, shell} -> ok
    end,
    gen_server:call(essh, {shell_cmd, Cmd}),
	receive
        done ->
            gen_server:call(essh, {shell_cmd, "exit;"})
    end.
            
