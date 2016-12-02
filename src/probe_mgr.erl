-module(probe_mgr).

-export([start_link/0, stop/0, add/1, add/2, get_active/0, run/1, sleep/1 ]).
-export([del/1, register_probe_pid/2, set_interval/2]).

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
-define(DEF_STATE_NEWPROBE, running ).

register_probe_pid( ProbeName, Pid ) ->
	gen_server:cast( ?MODULE, { register_probe_pid, { ProbeName, Pid } } ).

add( Params ) ->
	add( Params, ?DEF_STATE_NEWPROBE ).

add( Params, Initial ) ->
	gen_server:call( ?MODULE, { add, { Params, Initial } } ).

set_interval( ProbeName, Interval ) ->
	probe_worker:set_interval( ProbeName, Interval ).

del( ProbeName ) ->
	gen_server:call( ?MODULE, { del, ProbeName } ).
	
run( ProbeName ) ->
	gen_server:call( ?MODULE, { run, ProbeName } ).
	
sleep( ProbeName ) ->
	gen_server:call( ?MODULE, { sleep, ProbeName } ).
	
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
	case lists:keyfind( interval, 1, Params ) of
			{ interval, Interval } -> 
				ok;
			false ->
				Interval = ?DEFINTERVAL
	end,
	ModuleToCalltxt = "probe_type_" ++ atom_to_list(ProbeType),
        ModuleToCall = list_to_atom(ModuleToCalltxt),
	{ ok, NewAttrs } = ModuleToCall:normalize_pstate_attrs( Attrs ),
	case probe_store:get_pstate( ProbeName ) of 
		[] ->
			NewPstate = #pstate{ 
						status = Initial, 
						type = ProbeType, 
					 	interval = Interval, 
						attrs = NewAttrs 
					   },
			ok = probe_store:set_pstate( ProbeName, NewPstate ),
			Result = probe_worker:new( ProbeName, NewPstate );
		_ -> 
			Result = nee_it_exist_in_config
	end,
	io:format("Got ~p for new probe ~p~n", [Result,ProbeName]),
	{ reply, Result , State };

handle_call( { del ,  ProbeName } , _From, State ) ->
	Result = probe_worker:remove( ProbeName ),
	ok = probe_store:del_pstate( ProbeName ),
	{ reply, Result , State };


handle_call( { run ,  ProbeName } , _From, State ) ->
	case probe_worker:run( ProbeName ) of
		ok ->
			[{ProbeName,ProbePstate}] = probe_store:get_pstate( ProbeName ),
			NewPstate = ProbePstate#pstate{ status = running },
			ok = probe_store:set_pstate( ProbeName, NewPstate ),
			Result = ok;
		_ ->
			Result = nee_is_running_or_not_exist
	end,
	{ reply, Result , State };

handle_call( { sleep ,  ProbeName } , _From, State ) ->
	case probe_worker:sleep( ProbeName ) of
		ok ->
			[{ProbeName,ProbePstate}] = probe_store:get_pstate( ProbeName ),
			NewPstate = ProbePstate#pstate{ status = sleeping },
			ok = probe_store:set_pstate( ProbeName, NewPstate ),
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


handle_cast( { register_probe_pid , { ProbeName, Pid }}, State ) ->
	[{ProbeName, Pstate}] = probe_store:get_pstate( ProbeName ),
	ok = probe_store:add_probe_pid( ProbeName, Pid ),
	#pstate{ status = WantStatus } = Pstate,
	case WantStatus of
		running ->
			ok = probe_worker:run( ProbeName );
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
			[ probe_worker:new( ProbeName, Pstate ) || { ProbeName, Pstate } <- Config ]; 
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

