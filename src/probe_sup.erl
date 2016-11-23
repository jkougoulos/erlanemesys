-module(probe_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER,?MODULE).

start_link() ->
        supervisor:start_link({local,?SERVER}, ?MODULE, [] ).

init( [] ) ->
        init({simple_one_for_one, 3, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->

{ok, {{RestartStrategy, MaxRestart, MaxTime},
[
        {probe_worker,
                {probe_worker, start_link, []},
                permanent, 3000, worker, [probe_worker]}
]}}.

