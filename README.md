# jsondiff
An erlang JSON diffing library.

[![Build Status](https://travis-ci.org/michaelkitson/jsondiff.svg)](https://travis-ci.org/michaelkitson/jsondiff)

## Examples

Diffing `[1,{"a":2,"b":3,"c":0},4]` and `[1,{"a":5,"b":3},6,7]`

```erlang
jsondiff:diff(<<"[1,{\"a\":2,\"b\":3,\"c\":0},4]">>, <<"[1,{\"a\":5,\"b\":3},6,7]">>).
```

```erlang
[{added,[3],7},
 {modified,[1,<<"a">>],2,5},
 {modified,[2],4,6},
 {removed,[1,<<"c">>],0}]
```

```erlang
io:format("~s~n",jsx:prettify(jsondiff:as_json(<<"[1,{\"a\":2,\"b\":3,\"c\":0},4]">>, <<"[1,{\"a\":5,\"b\":3},6,7]">>))).
```

```json
[
  {
    "newValue": 7,
    "path": [
      3
    ],
    "type": "added"
  },
  {
    "newValue": 5,
    "oldValue": 2,
    "path": [
      1,
      "a"
    ],
    "type": "modified"
  },
  {
    "newValue": 6,
    "oldValue": 4,
    "path": [
      2
    ],
    "type": "modified"
  },
  {
    "newValue": 0,
    "path": [
      1,
      "c"
    ],
    "type": "removed"
  }
]
```
