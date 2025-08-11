-module(simple).
-export([
    answer/0,
    greeting/0,
    status/0,
    pi/0,
    truth/0,
    nothing/0
]).

answer() ->
    42.

greeting() ->
    "Hello, World!".

status() ->
    ok.

pi() ->
    3.14158999999999988262e+00.

truth() ->
    true.

nothing() ->
    nil.

