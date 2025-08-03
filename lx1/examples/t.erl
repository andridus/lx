-module(t).
-export([simple_record/0]).

-record(person, {name = nil :: binary(), age = nil :: integer()}).-spec simple_record() -> #person{}.
simple_record() ->
    PERSON_1 = #person{name = <<"João"/utf8>>, age = 30},
    PERSON_1.
