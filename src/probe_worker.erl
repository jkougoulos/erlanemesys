-module(probe_worker).

-behavior( gen_server ).

-export([start_link/1, touch/1, touch/2, testdie/1, get_status/1, run/1]).
-export([remove/1, sleep/1, set_interval/2, new/2, scheduler/2 ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
		{
		 host, 
		 type, 
		 attempts = 0, 
	 	 tref, 
		 status, 
		 interval,  
		 touched = 0, 
		 felt = 0
	}).

-record(pstate,
                {
                 status,
                 interval,
                 type,
                 attrs
      }).


-record(result, { timestamp, got } ).

-define(DEF_TIMEOUT, 2000 ).

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

remove( Dest ) ->
	case probe_store:get_host_pid( Dest ) of
		{ok, Pid } ->
			ok = supervisor:terminate_child( probe_sup, Pid ),
			probe_store:del_host_pid( Dest );
		_ ->
			{ error, host_does_not_exist }
	end.
		
%new( Dest, Interval, Tries, Timeout ) ->
new( ProbeName, Pstate ) -> 
	case probe_store:get_host_pid( ProbeName ) of
		{ok, _Pid} ->
			{ error, probe_already_added };
		_ ->
			supervisor:start_child( probe_sup, 
				[ { ProbeName, Pstate } ] )
	end.

get_status( Pid ) ->
	gen_server:call( Pid, get_status, 10000 ).

run( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, run );

run( Host ) ->
	case probe_store:get_host_pid( Host ) of
		{ok, Pid} ->
			run(Pid);
		_ ->
			{ error_pid_not_found }
	end.

sleep( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, sleep );

sleep( ProbeName ) ->
	case probe_store:get_host_pid( ProbeName ) of
		{ok, Pid} ->
			sleep(Pid);
		_ ->
			{ error_pid_not_found }
	end.

set_interval( Pid, NewInterval ) when is_pid( Pid ) ->
	NewInt =
		case is_list(NewInterval) of
			true -> list_to_integer( NewInterval);
			_ -> NewInterval
		end,
	gen_server:call( Pid, { set_interval, NewInt } );
	
set_interval( ProbeName, NewInterval ) ->
	{ ok, Pid } = probe_store:get_host_pid( ProbeName ),
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
				host = Host,
				status = Status, 
				touched = Sent, 
				felt = Rcvd 
			} = State,
		{ok, NewResults } = probe_store:get_results( Host ),
		{reply, { ok, Status, Sent, Rcvd, NewResults } , State};

handle_call( { set_interval, NewInterval }, _From , State ) when NewInterval >= 100 ->
		Status = State#state.status,
		NewState = 
		   case Status of
			running ->
				{ reply, ok, StateStopped } = handle_call( sleep, _From, State ),
				StateToRun = StateStopped#state{ interval = NewInterval },
				{reply, ok , StateAfterRun } = 
					handle_call( run, _From, StateToRun ),
				StateAfterRun;
			_ ->
				State#state{ interval = NewInterval }
		   end,
		{reply, ok,  NewState };

handle_call( run, _From,  State ) when State#state.status =:= sleeping ->
		spawn( ?MODULE, scheduler, [self(),State] ),
		{reply, ok, State };
			
handle_call( sleep, _From, State ) when State#state.status =:= running ->
		Tref = State#state.tref,
		{ ok, cancel } = timer:cancel( Tref ),
		{reply, ok,  State#state{ status = sleeping, tref = undefined } };

handle_call( _, _From, State ) ->
		{reply, probe_worker_call_here_unknown , State}.


handle_cast( touch , State ) ->
		
		#state{ 
			host = Host, 
			touched = Echo, 
			felt = Replies
						} = State,

		NewEcho = Echo + 1,
		Seq = NewEcho rem 65536,
		UTCNow = os:timestamp(),
		PingRes = 
  		  try gen_icmp:ping([Host], [{ timeout, ?DEF_TIMEOUT},{ sequence, Seq } ]) of
		    [PingOut] -> PingOut 
		  catch
			Exception:Reason -> 
				{ error, exception, {Exception, Reason} }

		  end,

	  	NewReplies =
		  case PingRes of
			{ ok, _, _,  _, {_, _, _, Delay}, _} ->
				Reply = { ok, { { sequence , Seq }, {delay, Delay} } },
				Replies + 1;
			{ error, timeout, _,  _ }  ->
				Reply = { error, timeout },
				Replies;
			{ error, unreach_net, _,  _, _, _, _} ->
				Reply = { error, unreach_net },
				Replies;
			{ error, exception, Exc } ->
				Reply = { error, exception, Exc },
				Replies;
			Other ->
				Reply = { error, unknown, Other },
				Replies
		  end,
		Result = #result{ timestamp = UTCNow, got = Reply },
		NewState = State#state{ 
					touched = NewEcho, 
					felt = NewReplies 
					},
		probe_store:add_result( Host, Result ),
		{noreply, NewState};

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
                                           } = Pstate,
        { fqdn_ip, Host } = lists:keyfind( fqdn_ip, 1, Attrs ),

			StartState = #state{ 
						host = Host, 
						type = Type,
						interval = Interval, 
						status = sleeping
						},
			probe_mgr:register_host_pid( ProbeName, self() ),
			{ ok, StartState }.  

code_change(_OldVsn, State, _Extra) ->
				io:format("Got code change! ping_worker ~p~n",[self()]),
				{ok, State}.

