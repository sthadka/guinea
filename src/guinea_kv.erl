-module(guinea_kv).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([new/0]).
-export([get/1, set/2, set/1, del/1]).

-type table() :: term().

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec new() -> Table :: table().
new() ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]).

-spec set(Key::term(), Val::term()) -> true.
set(Key, Value) ->
    ets:insert(?MODULE, {Key, Value}).

-spec set([{Key::term(), Val::term()}]) -> true.
set(KeyValList) ->
    ets:insert(?MODULE, KeyValList).

-spec get(Key::term()) -> Val::term() | error.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            undefined;
        [{Key, Value}] ->
            Value
    end.

-spec del(Key::term())-> true.
del(Key) ->
    ets:delete(?MODULE, Key).
