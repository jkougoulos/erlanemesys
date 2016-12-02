-module(probe_worker).

-behavior( gen_server ).

-export([start_link/1, touch/1, touch/2, testdie/1, get_status/1, run/1]).
-export([remove/1, sleep/1, set_interval/2, new/2, scheduler/2, store_result/2 ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
		{
		 name,
		 type, 
		 attempts = 0, 
	 	 tref, 
		 fun_pid = undef,
		 status, 
		 interval,  
		 attrs
	}).

-record(pstate,
                {
                 status,
                 interval,
                 type,
                 attrs
      }).


scheduler( Pid, State ) ->
	#state{ interval = Interval } = State,
	random:seed( now() ),
	Delay = random:uniform( Interval ),
	timer:sleep( Delay ),
	Pid ! poke,
	{ ok, Tref } = timer:send_interval( Interval, Pid, poke ),
	set_running( Pid, Tref ).

set_running( Pid, Tref ) ->
	gen_server:cast( Pid, { set_running , Tref } ).

remove( ProbeName ) ->
	case probe_store:get_probe_pid( ProbeName ) of
		{ok, Pid } ->
			ok = supervisor:terminate_child( probe_sup, Pid ),
			probe_store:del_probe_pid( ProbeName );
		_ ->
			{ error, probe_does_not_exist }
	end.
		
new( ProbeName, Pstate ) -> 
	case probe_store:get_probe_pid( ProbeName ) of
		{ok, _Pid} ->
			{ error, probe_already_added };
		_ ->
			supervisor:start_child( probe_sup, [ { ProbeName, Pstate } ] )
	end.

store_result( Pid, { FromPid, Attrs, Result } ) ->
	gen_server:call( Pid, { store_result, FromPid, Attrs, Result }, 10000 ).

get_status( Pid ) ->
	gen_server:call( Pid, get_status, 10000 ).

run( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, run );

run( ProbeName ) ->
	case probe_store:get_probe_pid( ProbeName ) of
		{ok, Pid} ->
			run(Pid)
	end.

sleep( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, sleep );

sleep( ProbeName ) ->
	case probe_store:get_probe_pid( ProbeName ) of
		{ok, Pid} ->
			sleep(Pid)
	end.

set_interval( Pid, NewInterval ) when is_pid( Pid ) ->
	NewInt =
		case is_list(NewInterval) of
			true -> list_to_integer( NewInterval );
			_ -> NewInterval
		end,
       { ok, Status, _Attempts, _Attempts, _NewResults } = get_status( Pid ), 
       case Status of
               running ->
                             ok = sleep( Pid ),
			     gen_server:call( Pid, { set_interval, NewInt } ),
			     ok = run( Pid ),
			     ok;
                     _ ->
			     gen_server:call( Pid, { set_interval, NewInt } )
       end;
	
set_interval( ProbeName, NewInterval ) ->
	{ ok, Pid } = probe_store:get_probe_pid( ProbeName ),
	set_interval( Pid, NewInterval ).

touch( Pid ) ->
	gen_server:cast( Pid, touch ).

touch( Pid, Times ) ->
	touch( Pid, Times, [] ).

touch( _Pid, Times, Resultset ) when Times =< 0 ->
		Resultset;

touch( Pid, Times, Resultset ) when Times > 0 ->
	Result = touch( Pid ),
	touch( Pid, Times - 1, [Result| Resultset] ).

testdie( Pid ) ->
	gen_server:call( Pid, testdie ).

handle_call( testdie, _From, State ) ->
	Reply = 1 / 0,
	{reply, Reply, State};

handle_call( get_status , _From,  State ) ->
		#state{ 
				name = ProbeName,
				status = Status, 
				attempts = Attempts,
				attrs = _Attrs
		} = State,
		{ok, NewResults } = probe_store:get_results( ProbeName ),
%		{reply, { ok, Status, Sent, Rcvd, NewResults } , State};
		{reply, { ok, Status, Attempts, Attempts, NewResults } , State};

handle_call( { set_interval, NewInterval }, _From , State ) when NewInterval >= 100 ->
		NewState = State#state{ interval = NewInterval },
		{ reply, ok, NewState };

handle_call( run, _From,  State ) when State#state.status =:= sleeping ->
		spawn( ?MODULE, scheduler, [self(),State] ),
		{ reply, ok, State };
			
handle_call( run, _From,  State ) when State#state.status =:= initializing ->
		spawn( ?MODULE, scheduler, [self(),State] ),
		{ reply, ok, State };
			
handle_call( sleep, _From, State ) when State#state.status =:= running ->
		Tref = State#state.tref,
		{ ok, cancel } = timer:cancel( Tref ),
		{ reply, ok, State#state{ status = sleeping, tref = undefined } };

handle_call( { store_result, FromPid, NewAttrs, Result }, _From, State ) ->
		ProbeName = State#state.name,
		FunPid = State#state.fun_pid,
		FunPid = FromPid,
		probe_store:add_result( ProbeName, Result ),
		NewState = State#state{ fun_pid = undef, attrs = NewAttrs },
		{reply, ok, NewState};

handle_call( _, _From, State ) ->
		{ reply, probe_worker_call_here_unknown , State }.


handle_cast( touch , State ) when State#state.fun_pid =:= undef ->
		
		#state{ 
			name = ProbeName,
			type = Type,
			attempts = Attempts,
			attrs = Attrs
		} = State,
		NewAttempts = Attempts + 1,
		ModuleToCalltxt = "probe_type_" ++ atom_to_list(Type),
		ModuleToCall = list_to_atom(ModuleToCalltxt),
		Self = self(),
		FunPid = spawn_link( fun() ->
					{ ok, NewAttrs, Result } = apply( ModuleToCall, specific_touch,  [Attrs] ),
					ok = probe_worker:store_result( Self, { self(), NewAttrs, Result } )
				end
		),

		NewState = State#state{
					attempts = NewAttempts,
					fun_pid = FunPid
		},
		{noreply, NewState};

handle_cast( touch , State )  ->
%		io:format( "touch while another type specific probe ~p is running ~p~n", [State#state.fun_pid, State#state.name ] ),
		{noreply, State};

handle_cast( { set_running, Tref } , State ) ->
		NewState = State#state{ status = running, tref = Tref },
		{noreply, NewState} ; 

handle_cast(Msg, State) ->
		io:format( "Ignoring ~p~n", [Msg] ),
		{noreply, State}.

handle_info( poke, State ) ->
		touch( self() ),
		{noreply, State };

handle_info(_Info, State ) ->
		{noreply, State}.

start_link( Params ) ->
	gen_server:start_link(?MODULE, [ Params ], []).

terminate(_Reason, _State ) ->
	ok.

init( [ {ProbeName, Pstate} ] ) -> 
        #pstate{
                  status = _Initial,
                  type = Type,
                  interval = Interval,
                  attrs = Attrs
               }   = Pstate,

        ModuleToCalltxt = "probe_type_" ++ atom_to_list(Type),
        ModuleToCall = list_to_atom(ModuleToCalltxt),
        { ok, NewAttrs } = ModuleToCall:normalize_state_attrs( Attrs ),

	StartState = #state{ 
				name = ProbeName,
				type = Type,
				interval = Interval, 
				status = initializing,
				attrs = NewAttrs
	},
	probe_mgr:register_probe_pid( ProbeName, self() ),
	{ ok, StartState }.  

code_change(_OldVsn, State, _Extra) ->
				io:format("Got code change! ping_worker ~p~n",[self()]),
				{ok, State}.

