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

let tests =
  [
    ("compile function", `Quick, test_compile_function);
    ("compile function with params", `Quick, test_compile_function_with_params);
    ("compile let expression", `Quick, test_compile_let_expression);
    ("capitalize variable names", `Quick, test_capitalize_var);
    ("empty program", `Quick, test_empty_program);
  ]
