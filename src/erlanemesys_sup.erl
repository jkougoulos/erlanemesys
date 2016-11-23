%%%-------------------------------------------------------------------
%% @doc erlanemesys top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlanemesys_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%init([]) ->
%    {ok, { {one_for_all, 0, 1}, []} }.
init( [] ) ->
        init({one_for_all, 3, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->
{ok,
  {
    {RestartStrategy, MaxRestart, MaxTime},
     [
       { probe_store, {probe_store, start_link, []}, permanent, 3000, worker, [probe_store]},
       { probe_mgr, {probe_mgr, start_link, []}, permanent, 3000, worker, [probe_mgr]},
       { probe_sup, {probe_sup, start_link, []}, permanent, infinity, supervisor, [probe_sup]}
     ]
  }
}.


%%====================================================================
%% Internal functions
%%====================================================================
