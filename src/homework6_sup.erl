-module(homework6_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs =
        [#{id => homework6,
            start => {homework6, start_link, []},
            shutdown => brutal_kill}],
    {ok, {{one_for_one, 5, 5}, ChildSpecs}}.
