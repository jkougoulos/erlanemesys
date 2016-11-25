-module(probe_mgr).

-export([start_link/0, stop/0, add/1, add/2, get_active/0, run/1, sleep/1 ]).
-export([del/1, register_host_pid/2, set_interval/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-behaviour(gen_server).

%-record(pstate,
%                {
%		 status,
%		 interval,
%		 tries,
%		 timeout
%      }).
-record(pstate,
                {
		 status,
		 interval,
		 type,
		 attrs
      }).

-define(SERVER, ?MODULE).

-define(DEFINTERVAL, 30000 ).
-define(DEF_TRIES, 1 ).
-define(DEF_TIMEOUT, 2000 ).
-define(DEF_STATE_NEWPROBE, running ).

register_host_pid( Host, Pid ) ->
	gen_server:cast( ?MODULE, { register_host_pid, { Host, Pid } } ).

%add( Host ) ->
%	add( Host, running ).
add( Params ) ->
	add( Params, ?DEF_STATE_NEWPROBE ).

add( Params, Initial ) ->
	gen_server:call( ?MODULE, { add, { Params, Initial } } ).

set_interval( Host, Interval ) ->
	probe_worker:set_interval( Host, Interval ).

del( Host ) ->
	gen_server:call( ?MODULE, { del, Host } ).
	
run( Host ) ->
	gen_server:call( ?MODULE, { run, Host } ).
	
sleep( Host ) ->
	gen_server:call( ?MODULE, { sleep, Host } ).
	
get_active() ->
	gen_server:call( ?MODULE, get_active ).

start_link() ->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

handle_call( { add , { Params, Initial } } , _From, State ) ->
	{ name, ProbeName } = lists:keyfind( name, 1, Params ),
	{ type, ProbeType } = lists:keyfind( type, 1, Params ),
	{ attrs, Attrs } = lists:keyfind( attrs, 1, Params ),
	{ fqdn_ip, Host } = lists:keyfind( fqdn_ip, 1, Attrs ), 
	case probe_store:get_pstate( Host ) of 
		[] ->
			Interval = ?DEFINTERVAL,
%			Tries = ?DEFTRIES,
%			Timeout = ?DEF_TIMEOUT,
			NewPstate = #pstate{ 
						status = Initial, 
						type = ProbeType, 
					 	interval = Interval, 
						attrs = Attrs 
					   },
			ok = probe_store:set_pstate( ProbeName, NewPstate ),
			Result = probe_worker:new( Host, Interval, ?DEF_TRIES, ?DEF_TIMEOUT );
		_ -> 
			Result = nee_it_exist_in_config
	end,
	io:format("Got ~p for new host ~p~n", [Result,Host]),
	{ reply, Result , State };

handle_call( { del ,  Host } , _From, State ) ->
	Result = probe_worker:remove( Host ),
	ok = probe_store:del_pstate(Host),
	{ reply, Result , State };


handle_call( { run ,  Host } , _From, State ) ->
	case probe_worker:run( Host ) of
		ok ->
			[{Host,HostPstate}] = probe_store:get_pstate( Host ),
			NewPstate = HostPstate#pstate{ status = running },
			ok = probe_store:set_pstate( Host, NewPstate ),
			Result = ok;
		_ ->
			Result = nee_is_running_or_not_exist
	end,
	{ reply, Result , State };

handle_call( { sleep ,  Host } , _From, State ) ->
	case probe_worker:sleep( Host ) of
		ok ->
			[{Host,HostPstate}] = probe_store:get_pstate( Host ),
			NewPstate = HostPstate#pstate{ status = sleeping },
			ok = probe_store:set_pstate( Host, NewPstate ),
			Result = ok;
		_ ->
			Result = nee_is_sleeping_or_not_exist
	end,
	{ reply, Result , State };

handle_call( get_active , _From, State ) ->
	Result = probe_store:get_active(),
	{ reply, Result , State };

handle_call( _Msg, _From, State) ->
	{reply, { no_call_match_dear }, State }.


handle_cast( { register_host_pid , { Host, Pid }}, State ) ->
	[{Host, Pstate}] = probe_store:get_pstate( Host ),
	ok = probe_store:add_host_pid( Host, Pid ),
	#pstate{ status = WantStatus } = Pstate,
	case WantStatus of
		running ->
			ok = probe_worker:run( Host );
		_ ->
			ok
	end,
	{noreply, State} ;

handle_cast( _Msg, State ) ->
	{reply, { no_cast_match_dear }, State }.
 
handle_info( start_up, _State ) ->
	case probe_store:get_active() of
		[] ->
			Config = probe_store:get_config(),
			[ probe_worker:new( ProbeName, Interval, ?DEF_TRIES, ?DEF_TIMEOUT ) || 
				{ ProbeName, #pstate{ status = _Initial, type = Type,
						 interval = Interval, attrs = Attrs }} <- Config ];
%				{ Host, #pstate{ status = _Initial, interval = Interval,
%						 tries = Tries, timeout = Timeout }} <- Config ];
		_ ->
			we_restarted_good_luck
	end,
	{noreply, { running } };

handle_info( _Msg, State ) ->
	{reply, { no_info_match_dear }, State }.

init([]) ->
	self() ! start_up,
	{ok, { initializing } }.

code_change(_OldVsn, State, _Extra) ->
		io:format("Got code change! probe_mgr ~p~n",[self()]),
		{ok, State}.
 
terminate(_Reason, _State) ->
	ok.	

