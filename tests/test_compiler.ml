open Alcotest
open Compiler.Ast

(* Helper function to check if string contains substring *)
let string_contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec search i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else search (i + 1)
  in
  search 0

let test_compile_function () =
  let func = { name = "num"; params = []; body = Literal (LInt 42) } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "-module(generated)"; "num() ->"; "42." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_compile_function_with_params () =
  let func = { name = "add"; params = [ "x"; "y" ]; body = Var "x" } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "add(X, Y) ->"; "X." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_compile_let_expression () =
  let let_expr = Let ("x", Literal (LInt 42), Var "x") in
  let func = { name = "num"; params = []; body = let_expr } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "num() ->"; "(X = 42, X)." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_capitalize_var () =
  check string "capitalize empty" "" (Compiler.capitalize_var "");
  check string "capitalize single" "X" (Compiler.capitalize_var "x");
  check string "capitalize word" "Hello" (Compiler.capitalize_var "hello");
  check string "capitalize already caps" "Hello"
    (Compiler.capitalize_var "Hello")

let test_empty_program () =
  let program = { items = [] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "-module(generated)"; "-compile(export_all)" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for nil compilation *)
let test_compile_nil () =
  let func = { name = "test_nil"; params = []; body = Literal LNil } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "test_nil() ->"; "nil." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for empty function body compilation *)
let test_compile_empty_function () =
  let func = { name = "empty"; params = []; body = Literal LNil } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "empty() ->"; "nil." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for tuple compilation *)
let test_compile_tuples () =
  let pair_tuple = Tuple [ Literal (LAtom "ok"); Literal (LInt 42) ] in
  let triple_tuple =
    Tuple [ Literal (LAtom "ok"); Literal (LInt 42); Literal (LString "test") ]
  in
  let empty_tuple = Tuple [] in

  let func1 = { name = "pair"; params = []; body = pair_tuple } in
  let func2 = { name = "triple"; params = []; body = triple_tuple } in
  let func3 = { name = "empty_tuple"; params = []; body = empty_tuple } in

  let program =
    { items = [ Function func1; Function func2; Function func3 ] }
  in
  let result = Compiler.compile_to_string program in

  let expected_parts =
    [
      "pair() ->";
      "{ok, 42}.";
      "triple() ->";
      "{ok, 42, \"test\"}.";
      "empty_tuple() ->";
      "{}.";
    ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for if-then without else compilation *)
let test_compile_if_then () =
  let if_expr = If (Literal (LBool true), Literal (LInt 42), None) in
  let func = { name = "test_if"; params = []; body = if_expr } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts =
    [ "test_if() ->"; "case true of true -> 42; _ -> nil end." ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for if-then-else compilation *)
let test_compile_if_then_else () =
  let if_expr =
    If (Literal (LBool true), Literal (LInt 42), Some (Literal (LInt 0)))
  in
  let func = { name = "test_if_else"; params = []; body = if_expr } in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts =
    [ "test_if_else() ->"; "case true of true -> 42; _ -> 0 end." ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let tests =
  [
    ("compile function", `Quick, test_compile_function);
    ("compile function with params", `Quick, test_compile_function_with_params);
    ("compile let expression", `Quick, test_compile_let_expression);
    ("capitalize variable names", `Quick, test_capitalize_var);
    ("empty program", `Quick, test_empty_program);
    ("compile nil", `Quick, test_compile_nil);
    ("compile empty function", `Quick, test_compile_empty_function);
    ("compile tuples", `Quick, test_compile_tuples);
    ("compile if-then", `Quick, test_compile_if_then);
    ("compile if-then-else", `Quick, test_compile_if_then_else);
  ]
