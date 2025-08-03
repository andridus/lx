-module(record_complex).
-export([record_complex/0]).

-record(point, {x = nil :: integer(), y = nil :: integer()}).-record(person, {name = nil :: binary(), age = nil :: integer(), active = nil :: boolean()}).-spec record_complex() -> {#point{}, #person{}}.
record_complex() ->
    POINT_1 = #point{x = 10, y = 20},
    PERSON_2 = #person{name = <<"Maria"/utf8>>, age = 25, active = true},
    {POINT_1, PERSON_2}.
