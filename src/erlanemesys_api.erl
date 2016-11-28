-module(erlanemesys_api).

%-export([start_link/0, stop/0, add/1, add/2, get_active/0, run/1, sleep/1 ]).
%-export([del/1, register_host_pid/2, set_interval/2]).

%probe_mgr calls
-export([add/1, del/1, run/1, sleep/1, get_active/0, set_interval/2]).

%probe_stats calls
-export([report/1]).

add( ProbeName ) ->
	Host = ProbeName,
	probe_mgr:add( [ 
				{ name, ProbeName }, 
				{ type, ping }, 
				{ attrs , [ 
						{ fqdn_ip, Host }
					  ]
				}
			]  
			).
%	probe_mgr:add( Host ).

del( ProbeName ) ->
	probe_mgr:del( ProbeName ).

run( ProbeName ) ->
	probe_mgr:run( ProbeName ).

sleep( ProbeName ) ->
	probe_mgr:sleep( ProbeName ).

get_active() ->
	probe_mgr:get_active().

set_interval(ProbeName, Interval) ->
	probe_mgr:set_interval(ProbeName, Interval).

report( ProbeName ) ->
	probe_stats:report( ProbeName ).
