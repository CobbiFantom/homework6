-module(homework6).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create/1, test/1, insert/3, insert/4, lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(work_state, {cache}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

create(TableName) ->
    gen_server:call(?MODULE, {create, TableName}).

test(A) ->
    gen_server:call(?MODULE, {test, A}).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, Timeout) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value, Timeout}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

init([]) ->
    erlang:send_after(60000, self(), clear),
    {ok, #work_state{cache=[]}}.

handle_call({test, A},_From, State = #work_state{}) ->
    {reply, {ok, A}, State};


handle_call({create, TableName}, _From, State) ->
    case ets:info(TableName) of
        undefined ->
            Table = ets:new(TableName, [public, named_table]),
            {reply, {ok, Table}, State#work_state{cache = Table}};
        _ ->
            {reply, already_exist, State}
    end;

handle_call({insert, TableName, Key, Value}, _From, State = #work_state{}) ->
    case ets:info(TableName) of
        undefined ->
            {reply, undefined, State};
        _ ->
            ets:insert(TableName, {Key, Value}),
            {reply, {insert, ok}, State}
    end;

handle_call({insert, TableName, Key, Value, Timeout}, _From, State = #work_state{}) ->
    case ets:info(TableName) of
        undefined ->
            {reply, undefined, State};
        _ ->
            ets:insert(TableName, {Key, Value, erlang:system_time(seconds) + Timeout}),
            {reply, ok, State}
    end;

handle_call({lookup, TableName, Key}, _From, State) ->
    case ets:info(TableName) of
        undefined ->
            {reply, undefined, State};
        _ ->
            case ets:lookup(TableName, Key) of
                [] ->
                    {reply, undefined, State};
                [{_, Value, Timeout}] ->
                    case erlang:system_time(seconds) >= Timeout of
                        true ->
                            {reply, undefined, State};
                        false ->
                            {reply, Value, State}
                    end;
                [{Key, Value}] ->
                    {reply, Value, State}
            end
    end.

handle_cast(_Request, State = #work_state{}) ->
    {noreply, State}.


handle_info(clear, State) ->
    delete_obsolete(State#work_state.cache),
    erlang:send_after(60000, self(), clear),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    {shutdown,ok}.

code_change(_OldVsn, State = #work_state{}, _Extra) ->
    {ok, State}.

delete_obsolete(TableName) ->
    Now = erlang:system_time(seconds),
    Select = ets:fun2ms(fun({_, _, Timeout}) when Timeout < Now -> true end),
    try
        ets:select_delete(TableName,Select)
    of
        _ ->
            ok
    catch
        error:badarg ->
            {error, not_exists}
    end.