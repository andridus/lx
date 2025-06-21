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

(* Helper function to check if string matches regex pattern *)
let string_matches_pattern s pattern =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

let test_compile_function () =
  let func = make_single_clause_function "num" [] (Literal (LInt 42)) in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "-module(generated)"; "num() ->"; "42." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_compile_function_with_params () =
  let func = make_single_clause_function "add" [ "x"; "y" ] (Var "x") in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts =
    [ "add(X_[a-z0-9]+, Y_[a-z0-9]+) ->"; "X_[a-z0-9]+\\." ]
  in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
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
  let func = make_single_clause_function "test_nil" [] (Literal LNil) in
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
  let func = make_single_clause_function "empty" [] (Literal LNil) in
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

  let func1 = make_single_clause_function "pair" [] pair_tuple in
  let func2 = make_single_clause_function "triple" [] triple_tuple in
  let func3 = make_single_clause_function "empty_tuple" [] empty_tuple in

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
  let func = make_single_clause_function "test_if" [] if_expr in
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
  let func = make_single_clause_function "test_if_else" [] if_expr in
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
    ("capitalize variable names", `Quick, test_capitalize_var);
    ("empty program", `Quick, test_empty_program);
    ("compile nil", `Quick, test_compile_nil);
    ("compile empty function", `Quick, test_compile_empty_function);
    ("compile tuples", `Quick, test_compile_tuples);
    ("compile if-then", `Quick, test_compile_if_then);
    ("compile if-then-else", `Quick, test_compile_if_then_else);
  ]
