-module(erlanemesys_api).

%probe_mgr calls
-export([addping/1, addhttp/1, del/1, run/1, sleep/1, get_active/0, set_interval/2]).

%probe_stats calls
-export([report/1]).

addping( ProbeName ) ->
	Host = ProbeName,
	probe_mgr:add( [ 
				{ name, ProbeName ++ "-ping" }, 
				{ type, ping }, 
				{ attrs , [ 
						{ fqdn_ip, Host }
					  ]
				}
			]  
	).

addhttp( ProbeName ) ->
	Host = ProbeName,
	probe_mgr:add( [ 
				{ name, ProbeName ++ "-http" }, 
				{ type, http }, 
				{ attrs , [ 
						{ fqdn_ip, Host }
					  ]
				}
			]  
	).

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
