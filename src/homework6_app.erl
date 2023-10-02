-module(homework6_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_, _) ->
    homework6_sup:start_link().

-spec stop(_) -> ok.
stop(_) ->
    ok.