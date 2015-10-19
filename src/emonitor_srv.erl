-module(emonitor_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TABLE_NAME, emonitor_srv).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, create_sensor/3, start_sensor/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_sensor(Name, Constructor, Function) when is_atom(Name), is_function(Constructor), is_function(Function)->
    gen_server:call(?MODULE, {create_sensor, Name, Constructor, Function})
.

start_sensor(Name, Args)->
    [ [Constructor] | _] = ets:match(?TABLE_NAME, {Name, '$2', '_', '_'}),
    Constructor(Name, Args)
.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE_NAME, [named_table, public, {write_concurrency, true}]),
    lager:debug("Started monitor client~n", []),
    {ok, []}.

handle_call({create_sensor, Name, Constructor, Function}, _From, State) ->
    case catch ets:new(Name, [named_table, public, {write_concurrency, true}]) of
    Name ->
        catch ets:insert(?TABLE_NAME, [{Name, Constructor, Function, 0}]) %{ets_name, constuctor, fun, acc}
    ;
    _->
        none %alredy exist?
    end,
    {reply, ok, State}
;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

