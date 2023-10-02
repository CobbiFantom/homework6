-module(homework6_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
    homework6_create_test, homework_insert_test,
    homework_lookup_test
    ].

init_per_suite(Config) ->
    ok = application:start(homework6),
    Config.

homework6_create_test(_Config) ->
    {new, ok} = homework6:create(new),
    already_exist = homework6:create(new).

homework_insert_test(_Config) ->
    {insert, ok} = homework6:insert(new, key, val),
    {insert, ok} = homework6:insert(new, key2, val2, 5).

homework_lookup_test(_Config) ->
    val = homework6:lookup(new, key),
    val2 = homework6:lookup(new, key2),
    timer:sleep(5000),
    undefined = homework6:lookup(new, key2).

end_per_suite(Config) ->
    ok = application:stop(homework6),
    Config.
