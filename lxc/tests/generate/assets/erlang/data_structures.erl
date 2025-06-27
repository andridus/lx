-module(data_structures).

-export([test_data_structures/0]).

-record(person, {name, age}).

test_data_structures() ->
    % Lists
    Numbers = [1, 2, 3, 4, 5],
    Mixed = [1, "hello", ok, true],
    Cons = [1 | [2, 3, 4]],

    % Tuples
    Point = {10, 20},
    User = {"john", 30, active},

    % Maps
    Config = #{debug => true, timeout => 5000},
    Mixed_keys = #{"name" => "alice", age => 25},

    % Records
    Person = #person{name = "bob", age = 35},

    {Numbers, Mixed, Cons, Point, User, Config, Mixed_keys, Person}.