%%% -------------------------------------------------------------------
%%% Author  : CE
%%% Description :
%%%
%%% Created : 2009-1-18
%%% -------------------------------------------------------------------
-module(essh_scheduler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {con = 256, intv = 0, pids = [], suspend = 0, cmd}).

%% ====================================================================
%% External functions
%% ====================================================================
%%
%% TODO: Add description of start/function_arity
%%
start() -> 
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []),
	ok.


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
init([]) ->
    {ok, #state{}}.

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
handle_call({opt, con ,V}, _From, State) ->
    Reply = ok,
    StateN = if
        V >= 0 ->
           io:format("set concurrent execution limits = ~p~n", [V]),
           State#state{con = V};  		   
        true ->
            io:format("error:the value must above 0~n"),
            State
            end,
    {reply, Reply, StateN};
handle_call({opt, intv ,V}, _From, State) ->
    Reply = ok,
    StateN = if
        V >= 0 ->
   			 V1 = V * 1000,
             io:format("set execution interval = ~ps~n", [V]),
   			 State#state{intv = V1};
        true ->
             io:format("error:the value must above 0~n"),
             State
             end,  
    {reply, Reply, StateN};
handle_call({cmd, Pids , Cmd}, _From, State = #state{con = Con}) ->
    {Pids1, Pids2} = split_pids(Pids, Con),
    send_cmd(Pids1, Cmd, 0),
    StateN = State#state{pids = Pids2, cmd = Cmd, suspend = length(Pids1)},
    {reply, ok, StateN};
handle_call({continue}, _From,
             State = #state{con = Con, cmd = Cmd, intv = Intv, pids = Pids, suspend = 1}) ->
    case Pids of
        [] ->
            {reply, ok, State#state{suspend = 0}};
        _ -> 
             {Pids1, Pids2} = split_pids(Pids, Con),             
             send_cmd(Pids1, Cmd, Intv),
             {reply, ok, State#state{pids = Pids2, suspend = length(Pids1)}}
    end;
handle_call({continue}, _From,
             State = #state{suspend = Suspend}) when Suspend > 1->
    {reply, ok, State#state{suspend = Suspend - 1}}.    
    

split_pids(Pids, Con) ->
    if
        length(Pids) =< Con ->
            {Pids, []};
        true ->
            lists:split(Con, Pids)
    end.
    
send_cmd(Pids, Cmd, Intv) ->
    spawn(
      fun() -> 
              if 
                 Intv > 0 ->
                     receive 
                     after Intv ->
                             ok
                     end;
                 true -> ok
           	  end,
              [gen_fsm:send_event(Pid, {cmd, Cmd}) || Pid <- Pids]
       end).    
               
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

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

