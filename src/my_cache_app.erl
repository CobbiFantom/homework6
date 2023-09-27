-module(my_cache_app).

-behavior(application).

-export([start/1]).
-export([stop/1]).

-spec start(_) -> {ok, pid()}.
start(_) ->
    my_cache_sup:start().

-spec stop(_) -> ok.
stop(_) ->
    ok.


