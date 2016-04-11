-module(emonitor_srv2).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TABLE_NAME, emonitor_srv).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, show_all/0, add/1, add/2, del/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(st, { 
	zabbix,
	timeout = 5000,
	hostname,
	enable = false
	}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

show_all()->
    gen_server:call(?MODULE, show_all).

add(Type)->
    catch ets:insert(Type, [{make_ref(), 1}])
.

add(Type, Data)->
    catch ets:insert(Type, [{make_ref(), Data}])
.

del(Type)->
    catch ets:insert(Type, [{make_ref(), -1}])
.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    lager:debug("Started monitor client 2~n", []),
    self() ! init,
    {ok, #st{}}.

handle_call(show_all, _From, State) ->
    All = ets:match(?TABLE_NAME, {'$1', '$2', '$3', '$4'}),
    Result = [
    begin
        TableFullList = ets:match(TableName, {'$1', '$2'}),
        TableList = [begin ets:delete(TableName, X), Y end || [X, Y] <- TableFullList],  %% format {uuid, int()|list()|atom()|binary()}
        %%error_logger:error_report([{lists, TableList}, {full, TableFullList}]),
        case catch Function(TableList, Acc) of
        {save, Val, NewAcc} when is_list(Val)->
             ets:delete(?TABLE_NAME, TableName),
             catch ets:insert(?TABLE_NAME, [{TableName, Constructor, Function, NewAcc}]),
             Val
        ;
        {save, Val, NewAcc}->
             ets:delete(?TABLE_NAME, TableName),
             catch ets:insert(?TABLE_NAME, [{TableName, Constructor, Function, NewAcc}]),
             {TableName, Val}
        ;
        {done, Val} when is_list(Val)->
             Val
        ;
        {done, Val} ->
             {TableName, Val}
        ;
        _RRR->
             error_logger:info_report([other, _RRR]),
             {TableName, undefined}
        end % case Function(TableList)
    end || [TableName, Constructor, Function, Acc] <- All],
    {reply, Result, State}
;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%handle_cast({add, Type}, State) ->
%    catch ets:insert(Type, [{make_ref(), 1}]),
%    {noreply, State}
%;
%handle_cast({del, Type}, State) ->
%    catch ets:insert(Type, [{make_ref(), -1}]),
%    {noreply, State}
%;
handle_cast(Msg, State) ->
    lager:debug("Monitor client 2: strange cast message ~p~n", [Msg]),
    {noreply, State}
.

handle_info(init, State) ->
    erlang:send_after(State#st.timeout, self(), send_to_zabbix),

    SrcHost = case application:get_env(emonitor, src_host) of
    {ok, {name, SrcHost2}}->
         SrcHost2
    ;
    {ok, Methods} when is_list(Methods)->
         emonitor_utils:get_hostname(Methods)
    ;
    _ ->
         emonitor_utils:get_hostname([name, env, sname])
    end,

    case application:get_env(emonitor, enable) of
    {ok, true}->
        {ok, Enable} = application:get_env(emonitor, enable),
        Port = start_zabbix(),
        {noreply, State#st{zabbix = Port, hostname = SrcHost, enable=Enable}}
    ;
    _->
        {noreply, State}
    end
;
handle_info({Port, {exit_status, ExitCode}}, State) when is_port(Port)->
    lager:warning("ZabbixClent is down. Exit Code: ~p~n", [ExitCode]),
    {noreply, State#st{
        zabbix = undefined, 
        enable = true, 
        hostname = State#st.hostname
    }}
;
handle_info(send_to_zabbix, State) when (State#st.enable == false) ->
    erlang:send_after(State#st.timeout, self(), send_to_zabbix),

    {reply, Result, State} = handle_call(show_all, none, State),
    Result1 = [ R || {_, Y} = R <-lists:flatten(Result), Y /= undefined] ++ 
             [ {X,  0} || {X, undefined} <-lists:flatten(Result)],
    
    case application:get_env(emonitor, tty, false) of
    true->
        lager:warning("Result zabbix: ~p~n", [Result1])
    ;
    false->
        none
    end,

    {noreply, 
	#st{
            timeout = State#st.timeout,
            hostname = State#st.hostname
    }}
;
handle_info(send_to_zabbix, State)->
    Port = case State#st.zabbix of
       P when is_port(P)-> 
            P;
       undefined-> 
            start_zabbix() 
    end,

    {reply, Result, State} = handle_call(show_all, none, State),
    Result1 = [ R || {_, Y} = R <-lists:flatten(Result), Y /= undefined] ++ 
             [ {atom_to_list(X),  0} || {X, undefined} <-lists:flatten(Result)],
    case application:get_env(emonitor, tty, false) of
    true->
        lager:warning("Result zabbix: ~p~n", [Result1])
    ;
    false->
        none
    end,

    Send = fun(Key, Value)->  
       M = io_lib:format("~s ~s ~w\n", [State#st.hostname, Key, Value]),
       Port ! {self(), {command, M}} 
    end,

    [ catch Send(X,Y) || {X,Y} <- Result1],

		    %{ok, Files} = file:list_dir("/proc/" ++ os:getpid() ++ "/fd"),
		    %FDCount = length(Files),
		    %Send(State#st.'vm.proc.overload.mem.num'),        %% Это на будущее
		    %Send(State#st.'vm.proc.overload.mbox.num'),  %% Это на будущее
		    %Send((State#st.'vm.proc.max.num')#kv{val=erlang:system_info(process_count)}),
		    %Send((State#st.'vm.proc.cur.num')#kv{val=erlang:system_info(process_count)}),
		    %Send((State#st.'db.ets.max.num')#kv{val=length(ets:all())}),
		    %Send((State#st.'db.ets.cur.num')#kv{val=length(ets:all())}),
		    %Send((State#st.'file.cur.num')#kv{val=FDCount}),
		    %Send((State#st.'port.max.num')#kv{val=erlang:system_info(port_count)} ),
		    %Send((State#st.'port.cur.num')#kv{val=erlang:system_info(port_count)} ),
		    %Send((State#st.'vm.memory.total.num')#kv{val = erlang:memory(total) }),
		    %Send((State#st.'vm.memoty.binary.num')#kv{val = erlang:memory(binary)}),
		    %Send((State#st.'vm.memory.atom.num')#kv{val = erlang:memory(atom)}),
		    %Send((State#st.'vm.memory.ets.num')#kv{val = erlang:memory(ets)}),
		    %Send(State#st.'vm.exception.num'),


    %Тут нормализуем среднее время к одной секунде.
    %catch Send((State#st.'db.redis.median.time')#kv{val=trunc(( (State#st.'db.redis.median.time')#kv.val / (State#st.'db.redis.median.time')#kv.count ) / trunc(State#st.timeout/1000) )}),

    erlang:send_after(State#st.timeout, self(), send_to_zabbix),
    {noreply, 
	#st{zabbix = Port,
            timeout = State#st.timeout,
            enable = true,
            hostname = State#st.hostname
    }}
;

%%handle_info({Port, {data, Data}}, State) when is_port(Port)->
%%    lager:debug("Zabbix Client: [SAY] ~p~n", [Data]),
%%    {noreply, State}
%%;
handle_info(Info, State) ->
    lager:debug("Emonitor client 2: strange info message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% API Function
%% ------------------------------------------------------------------
start_zabbix()->
    %% https://www.zabbix.com/documentation/2.2/manpages/zabbix_sender
    {ok, ZHost} = application:get_env(emonitor, server),

    ZabbixBin = os:cmd("which zabbix_sender") -- "\n",
    Command = lists:flatten(io_lib:format("~s -z ~s -r -i -", [ZabbixBin, ZHost])),
    Port = open_port({spawn, Command}, [stderr_to_stdout, exit_status]),
    Port
.

