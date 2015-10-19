-module(emonitor_utils).
-export([reg_error/0, time/1, start_time/0, stop_time/0, get_hostname/1, get_unix_timestamp/0]).
%% ------------------------------------------------------------------
%% API Function
%% ------------------------------------------------------------------

reg_error() ->
	gen_server:cast(?MODULE, {reg_error}).

get_unix_timestamp()->
  {A, B, C} = now(),
  (A*1000000000000+B*1000000+C) div 1000
.

time(StartTime)->
	stop_time() - StartTime
.

start_time()->
   get_unix_timestamp()
.

stop_time()->
   get_unix_timestamp()
.
%Функция для работы с хостнеймом
get_hostname([])->
    os:cmd("hostname -f") -- [10,13]
;
get_hostname([env|T])->
    case os:getenv("HOSTNAME") of
    false->
       get_hostname(T)
    ;
    Hostname->
       Hostname
    end
;
get_hostname([name|_])->
    [_Node, Hostname] = string:tokens(atom_to_list(node()), "@"),
    Hostname
;
get_hostname([sname|_])->
    [_Node, Hostname] = string:tokens(atom_to_list(node()), "@"),
    Hostname
;
get_hostname([_|T])->
    get_hostname(T)
.

