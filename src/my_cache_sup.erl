-module(my_cache_sup).

-behavior(supervisor).

-export([start/0]).
-export([init/1]).

-spec start() -> {ok, pid()}.
start() ->
    supervisor:start({local, ?MODULE}, ?MODULE, []).

-spec init([])
        -> {ok, {{supervisor:strategy(), 10, 10}, [supervisor:child_spec()]}}.
init([]) ->
    Procs = [{cowboy_clock, {cowboy_clock, start_link, []},
        permanent, 5000, worker, [cowboy_clock]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.