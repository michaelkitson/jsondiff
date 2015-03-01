-module(jsondiff_tests).

-include_lib("eunit/include/eunit.hrl").

simple_equal_array_test() ->
    One = <<"[1]">>,
    Two = <<"[1]">>,
    Expected = [],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

simple_equal_object_test() ->
    One = <<"{\"a\":1}">>,
    Two = <<"{\"a\":1}">>,
    Expected = [],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

complex_equal_array_test() ->
    One = <<"[\"asdfasdf\",1,2,3,[false,45]]">>,
    Two = <<"[\"asdfasdf\",1,2,3,[false,45]]">>,
    Expected = [],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

subobject_test() ->
    One = <<"{\"b\":2,\"c\":{},\"d\":4}">>,
    Two = <<"{\"a\":1,\"c\":{\"a\":3},\"d\":5}">>,
    Expected = [
        {added, [<<"a">>], 1},
        {added, [<<"c">>, <<"a">>], 3},
        {modified, [<<"d">>], 4, 5},
        {removed, [<<"b">>], 2}
    ],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

simple_array_added_test() ->
    One = <<"[1]">>,
    Two = <<"[1,2,3]">>,
    Expected = [
        {added, [1], 2},
        {added, [2], 3}
    ],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

simple_array_removed_test() ->
    One = <<"[1,2,3]">>,
    Two = <<"[1]">>,
    Expected = [
        {removed, [1], 2},
        {removed, [2], 3}
    ],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

array_modification_test() ->
    One = <<"[1,{\"a\":2,\"b\":3,\"c\":0},4]">>,
    Two = <<"[1,{\"a\":5,\"b\":3},6,7]">>,
    Expected = [
        {added, [3], 7},
        {modified, [1,<<"a">>], 2, 5},
        {modified, [2], 4, 6},
        {removed, [1,<<"c">>], 0}
    ],
    Actual = jsondiff:diff(One, Two),
    ?assertEqual(Expected, Actual).

subobject_json_test() ->
    One = <<"{\"b\":2,\"c\":{},\"d\":4}">>,
    Two = <<"{\"a\":1,\"c\":{\"a\":3},\"d\":5}">>,
    Expected = jsxn:encode([
        #{ type => added,    path => [<<"a">>],          newValue => 1},
        #{ type => added,    path => [<<"c">>, <<"a">>], newValue => 3},
        #{ type => modified, path => [<<"d">>],          oldValue => 4, newValue => 5},
        #{ type => removed,  path => [<<"b">>],          newValue => 2}
    ]),
    Actual = jsondiff:as_json(One, Two),
    ?assertEqual(Expected, Actual).
