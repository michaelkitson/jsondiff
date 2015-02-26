-module(jsondiff).

%% jsondiff: jsondiff library's entry point.

-export([diff/2, print/2, asJSON/2]).

%% API

asJSON(From, To) ->
    jsxn:encode([asMap(Change) || Change <- diff(From, To)]).

asMap({added, Path, NewValue}) ->
    #{
        type => added,
        path => Path,
        newValue => NewValue
    };

asMap({modified, Path, OldValue, NewValue}) ->
    #{
        type => modified,
        path => Path,
        newValue => NewValue,
        oldValue => OldValue
    };

asMap({removed, Path, OldValue}) ->
    #{
        type => removed,
        path => Path,
        newValue => OldValue
    }.

print(From, To) ->
    io:format("~p~n", [diff(From, To)]).

diff(From, To) ->
    diff(From, To, jsxn:is_json(From), jsxn:is_json(To)).

diff(From, To, true, true) ->
    lists:flatten(get_changes(jsxn:decode(From), jsxn:decode(To), [])).

%% Internals
get_changes(From, To, _Path) when From == To ->
    [];

%% Object Diffing
get_changes(From, To, Path) when is_map(From) and is_map(To) ->
    FromKeys = maps:keys(From),
    ToKeys   = maps:keys(To),

    AddedKeys    = [K || K <- ToKeys,   maps:is_key(K, From) == false],
    ConstantKeys = [K || K <- FromKeys, maps:is_key(K, To)],
    RemovedKeys  = [K || K <- FromKeys, maps:is_key(K, To)   == false],

    Added         = [{added, Path ++ [K], maps:get(K, To)} || K <- AddedKeys],
    Modifications = [get_changes(maps:get(K, From), maps:get(K, To), Path ++ [K]) || K <- ConstantKeys],
    Removed       = [{removed, Path ++ [K], maps:get(K, From)} || K <- RemovedKeys],

    Added ++ Modifications ++ Removed;

get_changes(From, To, Path) when is_list(From) and is_list(To) ->
    FromLength = length(From),
    ToLength   = length(To),

    if ToLength > FromLength ->
        AddedIndexes = lists:seq(FromLength + 1, ToLength),
        Added   = [{added, Path ++ [Index - 1], lists:nth(Index, To)} || Index <- AddedIndexes],
        Removed = [];
    FromLength > ToLength ->
        RemovedIndexes = lists:seq(ToLength + 1, FromLength),
        Added   = [],
        Removed = [{removed, Path ++ [Index - 1], lists:nth(Index, From)} || Index <- RemovedIndexes]
    end,
    ConstantIndexes = lists:seq(1, min(FromLength, ToLength)),
    Modifications = [get_changes(lists:nth(Index, From), lists:nth(Index, To), Path ++ [Index]) || Index <- ConstantIndexes],

    Added ++ Modifications ++ Removed;

get_changes(From, To, Path) ->
    [{modified, Path, From, To}].

%% End of Module.
