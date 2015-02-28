-module(jsondiff).

%% jsondiff: Detect changes between json documents.

-export([diff/2, as_json/2]).

%% API
as_json(From, To) ->
    jsxn:encode([as_map(Change) || Change <- diff(From, To)]).

diff(From, To) ->
    diff(From, To, jsxn:is_json(From), jsxn:is_json(To)).

diff(From, To, true, true) ->
    lists:sort(fun sort_changes/2, lists:flatten(get_changes(jsxn:decode(From), jsxn:decode(To), []))).

%% Internals

%% Handle identical arguments
get_changes(From, To, _Path) when From == To ->
    [];

%% Object Diffing
get_changes(From, To, Path) when is_map(From) andalso is_map(To) ->
    FromKeys = maps:keys(From),
    ToKeys   = maps:keys(To),

    AddedKeys    = [K || K <- ToKeys,   maps:is_key(K, From) == false],
    ConstantKeys = [K || K <- FromKeys, maps:is_key(K, To)],
    RemovedKeys  = [K || K <- FromKeys, maps:is_key(K, To)   == false],

    Added         = [{added, Path ++ [K], maps:get(K, To)} || K <- AddedKeys],
    Modifications = [get_changes(maps:get(K, From), maps:get(K, To), Path ++ [K]) || K <- ConstantKeys],
    Removed       = [{removed, Path ++ [K], maps:get(K, From)} || K <- RemovedKeys],

    Added ++ Modifications ++ Removed;

%% List diffing - preserves order
get_changes(From, To, Path) when is_list(From) andalso is_list(To) ->
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
    Modifications = [get_changes(lists:nth(Index, From), lists:nth(Index, To), Path ++ [Index - 1]) || Index <- ConstantIndexes],

    Added ++ Modifications ++ Removed;

%% Other nonequal json terms.
get_changes(From, To, Path) ->
    [{modified, Path, From, To}].

%% Result conversions
as_map({added, Path, NewValue}) ->
    #{
        type => added,
        path => Path,
        newValue => NewValue
    };

as_map({modified, Path, OldValue, NewValue}) ->
    #{
        type => modified,
        path => Path,
        newValue => NewValue,
        oldValue => OldValue
    };

as_map({removed, Path, OldValue}) ->
    #{
        type => removed,
        path => Path,
        newValue => OldValue
    }.

%% Result Sorting
sort_changes({added, LeftPath, _LeftNewValue}, {added, RightPath, _RightNewValue}) ->
    LeftPath < RightPath;
sort_changes({added, _LeftPath, _LeftNewValue}, _Right) ->
    true;
sort_changes(_Left, {added, _RightPath, _RightNewValue}) ->
    false;
sort_changes({modified, LeftPath, _LeftOldValue, _LeftNewValue}, {modified, RightPath, _RightOldValue, _RightNewValue}) ->
    LeftPath < RightPath;
sort_changes({modified, _LeftPath, _LeftOldValue, _LeftNewValue}, _Right) ->
    true;
sort_changes(_Left, {modified, _RightPath, _RightOldValue, _RightNewValue}) ->
    false;
sort_changes({removed, LeftPath, _LeftOldValue}, {removed, RightPath, _RightOldValue}) ->
    LeftPath < RightPath.

%% End of Module.
