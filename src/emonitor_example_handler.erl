-module(emonitor_example_handler).
-export([start/0, monitor/2]).

%% This function is a pid monitor.
%% Function not otp-tree style.
monitor(Type, Pid)->
	MonRef = erlang:monitor(process, Pid),
	emonitor_srv2:add(Type),
	receive
	{'DOWN', MonRef, process, Pid, _ExitState} ->
		emonitor_srv2:del(Type)
	end,
	erlang:demonitor(MonRef)  %% на всякий случай.
.

start()->
    [emonitor_srv:create_sensor(Sensor, 
                  fun(Type, Args)->
			spawn(?MODULE, monitor, [Type, Args])
                  end,
                  fun(List, Acc)-> 
                      Num = lists:sum(List),
                      {save, Num+Acc, Num+Acc}
                  end
    ) || Sensor <- ['dbredisconnects', 'dbmongoconnects'] ],

    emonitor_srv:start_sensor('dbredisconnects', self())
.
