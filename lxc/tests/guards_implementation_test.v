module main

fn test_with_expression_with_guards() {
	lx_code := '
record User {
  id :: integer,
  active :: boolean
}

def check_user_status(user) do
	with User{id: id, active: true} when id > 0 <- user do
		"active_user"
	else
		"inactive_user"
	end
end'
	expected := '-module(test).
-export([check_user_status/1]).

-record(user, {id, active}).
-spec check_user_status(any()) -> binary().
check_user_status(User) ->
case User of
    #user{id = Id, active = true} when Id > 0 ->
        <<"active_user"/utf8>>;
    Other ->
        <<"inactive_user"/utf8>>
end.

'
	assert generates_erlang(lx_code) == expected
}

// fn test_with_expression_multiple_guards() {
// 	lx_code := '
// def check_tuple(tuple) do
// 	with {x, y} when x > 0 and y < 10 <- tuple do
// 		"valid_tuple"
// 	else
// 		"invalid_tuple"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_tuple/1]).

// -spec check_tuple(any()) -> string().
// check_tuple(Tuple) ->
// case Tuple of
//     {X, Y} when X > 0 andalso Y < 10 ->
//         "valid_tuple";
//     Other ->
//         "invalid_tuple"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_with_expression_map_guards() {
// 	lx_code := '
// def check_person(person) do
// 	with %{age: age} when age >= 18 <- person do
// 		"adult"
// 	else
// 		"minor"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_person/1]).

// -spec check_person(any()) -> string().
// check_person(Person) ->
// case Person of
//     #{age => Age} when Age >= 18 ->
//         "adult";
//     Other ->
//         "minor"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_with_expression_list_guards() {
// 	lx_code := '
// def check_list(list) do
// 	with [head | tail] when head > 0 <- list do
// 		"positive_head"
// 	else
// 		"non_positive_head"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_list/1]).

// -spec check_list(any()) -> string().
// check_list(List) ->
// case List of
//     [Head | Tail] when Head > 0 ->
//         "positive_head";
//     Other ->
//         "non_positive_head"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_match_expression_with_guards() {
// 	lx_code := '
// record User {
//   id :: integer,
//   active :: boolean
// }

// def check_user_status(user) do
// 	match User{id: id, active: true} when id > 0 <- user
// 	"active_user"
// end'
// 	expected := '-module(main).
// -export([check_user_status/1]).

// -record(user, {id, active}).
// -spec check_user_status(any()) -> string().
// check_user_status(User) ->
// case User of
//     #user{id = Id, active = true} when Id > 0 ->
//         "active_user";
//     Other ->
//         Other
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_match_expression_simple_guards() {
// 	lx_code := '
// def check_number(num) do
// 	match x when x > 0 <- num
// 	"positive"
// end'
// 	expected := '-module(main).
// -export([check_number/1]).

// -spec check_number(any()) -> string().
// check_number(Num) ->
// case Num of
//     X when X > 0 ->
//         "positive";
//     Other ->
//         Other
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_equality_operators() {
// 	lx_code := '
// def check_values(a, b) do
// 	with x when x == 5 <- a,
// 		 y when y != 10 <- b do
// 		"conditions_met"
// 	else
// 		"conditions_failed"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_values/2]).

// -spec check_values(any(), any()) -> string().
// check_values(A, B) ->
// case A of
//     X when X =:= 5 ->
//         case B of
//             Y when Y =/= 10 ->
//                 "conditions_met";
//             Other ->
//                 "conditions_failed"
//         end;
//     Other ->
//         "conditions_failed"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_comparison_operators() {
// 	lx_code := '
// def check_range(a, b, c, d) do
// 	with x when x > 5 <- a,
// 		 y when y < 10 <- b,
// 		 z when z >= 15 <- c,
// 		 w when w <= 20 <- d do
// 		"all_in_range"
// 	else
// 		"out_of_range"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_range/4]).

// -spec check_range(any(), any(), any(), any()) -> string().
// check_range(A, B, C, D) ->
// case A of
//     X when X > 5 ->
//         case B of
//             Y when Y < 10 ->
//                 case C of
//                     Z when Z >= 15 ->
//                         case D of
//                             W when W =< 20 ->
//                                 "all_in_range";
//                             Other ->
//                                 "out_of_range"
//                         end;
//                     Other ->
//                         "out_of_range"
//                 end;
//             Other ->
//                 "out_of_range"
//         end;
//     Other ->
//         "out_of_range"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_boolean_literals() {
// 	lx_code := '
// def check_always(val) do
// 	with x when true <- val do
// 		"always_matches"
// 	else
// 		"never_reached"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_always/1]).

// -spec check_always(any()) -> string().
// check_always(Val) ->
// case Val of
//     X ->
//         "always_matches";
//     Other ->
//         "never_reached"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_false_literal() {
// 	lx_code := '
// def check_never(val) do
// 	with x when false <- val do
// 		"never_matches"
// 	else
// 		"always_reached"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_never/1]).

// -spec check_never(any()) -> string().
// check_never(Val) ->
// case Val of
//     X when false ->
//         "never_matches";
//     Other ->
//         "always_reached"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_variable_scoping() {
// 	lx_code := '
// def check_sum(tuple) do
// 	with {x, y} when x + y > 10 <- tuple do
// 		"sum_greater_than_10"
// 	else
// 		"sum_less_or_equal_10"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_sum/1]).

// -spec check_sum(any()) -> string().
// check_sum(Tuple) ->
// case Tuple of
//     {X, Y} when X + Y > 10 ->
//         "sum_greater_than_10";
//     Other ->
//         "sum_less_or_equal_10"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_nested_patterns() {
// 	lx_code := '
// record Profile {
//   age :: integer
// }

// record User {
//   profile :: Profile
// }

// def check_adult_user(user) do
// 	with User{profile: Profile{age: age}} when age >= 18 <- user do
// 		"adult_user"
// 	else
// 		"minor_user"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_adult_user/1]).

// -record(profile, {age}).
// -record(user, {profile}).
// -spec check_adult_user(any()) -> string().
// check_adult_user(User) ->
// case User of
//     #user{profile = #profile{age = Age}} when Age >= 18 ->
//         "adult_user";
//     Other ->
//         "minor_user"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }

// fn test_guards_with_all_operators() {
// 	lx_code := '
// def check_all_conditions(a, b, c, d, e, f) do
// 	with x when x > 5 <- a,
// 		 y when y < 10 <- b,
// 		 z when z >= 15 <- c,
// 		 w when w <= 20 <- d,
// 		 v when v == 25 <- e,
// 		 u when u != 30 <- f do
// 		"all_conditions_met"
// 	else
// 		"conditions_failed"
// 	end
// end'
// 	expected := '-module(main).
// -export([check_all_conditions/6]).

// -spec check_all_conditions(any(), any(), any(), any(), any(), any()) -> string().
// check_all_conditions(A, B, C, D, E, F) ->
// case A of
//     X when X > 5 ->
//         case B of
//             Y when Y < 10 ->
//                 case C of
//                     Z when Z >= 15 ->
//                         case D of
//                             W when W =< 20 ->
//                                 case E of
//                                     V when V =:= 25 ->
//                                         case F of
//                                             U when U =/= 30 ->
//                                                 "all_conditions_met";
//                                             Other ->
//                                                 "conditions_failed"
//                                         end;
//                                     Other ->
//                                         "conditions_failed"
//                                 end;
//                             Other ->
//                                 "conditions_failed"
//                         end;
//                     Other ->
//                         "conditions_failed"
//                 end;
//             Other ->
//                 "conditions_failed"
//         end;
//     Other ->
//         "conditions_failed"
// end.

// '
// 	assert generates_erlang(lx_code) == expected
// }
