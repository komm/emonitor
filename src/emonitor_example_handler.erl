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
                             {"metric.min.redis",   0},
                             {"metric.max.redis",   0},
                             {"metric.count.redis", 0},
                             {"metric.median.redis", 0}
                            ],
                      []} 
                  ;
                     (Data, Acc)->
                      {save,[
                             {"metric.min.redis", lists:min(Data)}, 
                             {"metric.max.redis", lists:max(Data)},
                             {"metric.count.redis", length(Data)},
                             {"metric.median.redis", lists:sum(Data) / length(Data)}
                            ],
                      []} 
                  end),
    emonitor_srv:start_sensor(redis, [])
.
