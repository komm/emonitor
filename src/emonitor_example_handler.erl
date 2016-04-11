-module(emonitor_example_handler).
-export([start/0, monitor/2]).

%% This function is a pid monitor.
%% Function not otp-tree style.
monitor(Type, Pid)->
	MonRef = erlang:monitor(process, Pid),
	emonitor_srv2:add(Type),
	receive
	{'DOWN', MonRef, process, Pid, _ExitState} ->
                error_logger:info_report([{process, 'DOWN'}]),
		emonitor_srv2:del(Type)
	end,
	erlang:demonitor(MonRef)
.

start()->
    emonitor_srv:create_sensor(redis,
                  fun(_,_)-> ok end, 
                  fun([], Acc)->
                      {save,[
                             {"erlconto.min.redis",   0},
                             {"erlconto.max.redis",   0},
                             {"erlconto.count.redis", 0},
                             {"erlconto.median.redis", 0}
                            ],
                      []} 
                  ;
                     (Data, Acc)->
                      {save,[
                             {"erlconto.min.redis", lists:min(Data)}, 
                             {"erlconto.max.redis", lists:max(Data)},
                             {"erlconto.count.redis", length(Data)},
                             {"erlconto.median.redis", lists:sum(Data) / length(Data)}
                            ],
                      []} 
                  end),
    emonitor_srv:start_sensor(redis, [])
.

