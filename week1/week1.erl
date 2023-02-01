-module(week1).
-include_lib("eunit/include/eunit.hrl").
-export([hello_world/0, get_hello/0]).

hello_world() ->
    io:format("Hello PTR\n").

get_hello() ->
    "Hello PTR".

unit_test_() -> 
    ?_test(?assert(get_hello() =:= "Hello PTR")).
    