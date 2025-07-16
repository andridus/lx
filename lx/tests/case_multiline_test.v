module main

// fn test_case_multiline_simple() {
// 	lx_code := '
// def simple_case() do
//   case 1 do
//     1 ->
//       x = 10
//       20
//     2 -> 0
//     _ -> -1
//   end
// end'
// 	expected := '-module(main).
// -export([simple_case/0]).

// -spec simple_case() -> integer().
// simple_case() ->
// case 1 of
//     1 -> X_aaaa = 10,
//     20;
//     2 -> 0;
//     _ -> -1
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_case_multiline_complex() {
// 	lx_code := '
// def complex_case(x) do
//   case x do
//     1 ->
//       a = 10
//       b = 20
//       30
//     2 ->
//       result = 42
//       result
//     _ ->
//       default_val = -1
//       default_val
//   end
// end'
// 	expected := '-module(main).
// -export([complex_case/1]).

// -spec complex_case(any()) -> integer().
// complex_case(X) ->
// case X of
//     1 -> A_aaaa = 10,
//     B_baaa = 20,
//     30;
//     2 -> Result_caaa = 42,
//     Result_caaa;
//     _ -> Default_val_daaa = -1,
//     Default_val_daaa
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_case_multiline_with_tuples() {
// 	lx_code := '
// def tuple_case(x) do
//   case x do
//     {1, 2} ->
//       first = 1
//       second = 2
//       {first, second}
//     {a, b} ->
//       {a, b}
//     _ ->
//       zero = 0
//       {zero, zero}
//   end
// end'
// 	expected := '-module(main).
// -export([tuple_case/1]).

// -spec tuple_case(any()) -> {any(), any()}.
// tuple_case(X) ->
// case X of
//     {1, 2} -> First_aaaa = 1,
//     Second_baaa = 2,
//     {First_aaaa, Second_baaa};
//     {A, B} -> {A, B};
//     _ -> Zero_caaa = 0,
//     {Zero_caaa, Zero_caaa}
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_case_multiline_with_atoms() {
// 	lx_code := '
// def atom_case(status) do
//   case status do
//     :ok ->
//       message = "Success"
//       code = 200
//       {message, code}
//     :error ->
//       message = "Error"
//       code = 500
//       {message, code}
//     _ ->
//       message = "Unknown"
//       code = 0
//       {message, code}
//   end
// end'
// 	expected := '-module(main).
// -export([atom_case/1]).

// -spec atom_case(any()) -> {any(), any()}.
// atom_case(Status) ->
// case Status of
//     ok -> Message_aaaa = "Success",
//     Code_baaa = 200,
//     {Message_aaaa, Code_baaa};
//     error -> Message_caaa = "Error",
//     Code_daaa = 500,
//     {Message_caaa, Code_daaa};
//     _ -> Message_eaaa = "Unknown",
//     Code_faaa = 0,
//     {Message_eaaa, Code_faaa}
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

fn test_case_multiline_with_lists() {
	lx_code := '
def list_case(lst) do
  case lst do
    [] ->
      empty_msg = "Empty list"
      empty_msg
     [head | tail] ->
       first = head
       rest_count = 42
       {first, rest_count}
    _ ->
      unknown = "Unknown"
      unknown
  end
end'
	expected := '-module(test).
-export([list_case/1]).

-spec list_case(any()) -> binary() | {any(), integer()}.
list_case(Lst) ->
case Lst of
    [] -> Empty_msg_aaaa = <<"Empty list"/utf8>>,
    Empty_msg_aaaa;
    [Head | Tail] -> First_baaa = Head,
    Rest_count_caaa = 42,
    {First_baaa, Rest_count_caaa};
    _ -> Unknown_daaa = <<"Unknown"/utf8>>,
    Unknown_daaa
end.

'
	assert generates_erlang(lx_code) == expected
}

// fn test_case_single_line_still_works() {
// 	lx_code := '
// def single_line_case(x) do
//   case x do
//     1 -> :one
//     2 -> :two
//     _ -> :other
//   end
// end'
// 	expected := '-module(main).
// -export([single_line_case/1]).

// -spec single_line_case(any()) -> atom().
// single_line_case(X) ->
// case X of
//     1 -> one;
//     2 -> two;
//     _ -> other
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }
