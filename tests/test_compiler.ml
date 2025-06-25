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
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "-module(generated)"; "num() ->"; "42." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_compile_function_with_params () =
  let func =
    make_single_clause_function "add" [ "x"; "y" ]
      (BinOp (Var "x", "+", Var "y"))
  in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [ "add(X_[a-z0-9]+, Y_[a-z0-9]+) ->"; "X_[a-z0-9]+ \\+ Y_[a-z0-9]+\\." ]
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
  let program = { deps = None; items = [] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "-module(generated)" ] in
  let unexpected_parts = [ "-compile(export_all)" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts;
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("should not contain: " ^ part) false contains)
    unexpected_parts

(* Test for nil compilation *)
let test_compile_nil () =
  let func = make_single_clause_function "test_nil" [] (Literal LNil) in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "test_nil() ->"; "nil." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for empty function body compilation *)
let test_compile_empty_function () =
  let func = make_single_clause_function "empty" [] (Literal LNil) in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
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
    { deps = None; items = [ Function func1; Function func2; Function func3 ] }
  in
  let result = Compiler.compile_to_string_for_tests program in

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

(* Test for if without else compilation *)
let test_compile_if_then () =
  let if_expr = If (Literal (LBool true), Literal (LInt 42), None) in
  let func = make_single_clause_function "test_if" [] if_expr in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [ "test_if() ->"; "case true of true -> 42; _ -> nil end." ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for if-else compilation *)
let test_compile_if_then_else () =
  let if_expr =
    If (Literal (LBool true), Literal (LInt 42), Some (SimpleElse (Literal (LInt 0))))
  in
  let func = make_single_clause_function "test_if_else" [] if_expr in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [ "test_if_else() ->"; "case true of true -> 42; _ -> 0 end." ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

(* Test for comparison operators compilation *)
let test_compile_comparison_operators () =
  let eq_expr = BinOp (Var "x", "==", Literal (LInt 10)) in
  let neq_expr = BinOp (Var "y", "!=", Literal (LInt 5)) in
  let lt_expr = BinOp (Var "a", "<", Var "b") in
  let gt_expr = BinOp (Var "c", ">", Var "d") in
  let leq_expr = BinOp (Var "e", "<=", Literal (LInt 100)) in
  let geq_expr = BinOp (Var "f", ">=", Literal (LInt 0)) in

  let func1 = make_single_clause_function "test_eq" [ "x" ] eq_expr in
  let func2 = make_single_clause_function "test_neq" [ "y" ] neq_expr in
  let func3 = make_single_clause_function "test_lt" [ "a"; "b" ] lt_expr in
  let func4 = make_single_clause_function "test_gt" [ "c"; "d" ] gt_expr in
  let func5 = make_single_clause_function "test_leq" [ "e" ] leq_expr in
  let func6 = make_single_clause_function "test_geq" [ "f" ] geq_expr in

  let program =
    {
      deps = None;
      items =
        [
          Function func1;
          Function func2;
          Function func3;
          Function func4;
          Function func5;
          Function func6;
        ];
    }
  in
  let result = Compiler.compile_to_string_for_tests program in

  let expected_parts =
    [
      "X_[a-z0-9]+ == 10";
      "Y_[a-z0-9]+ /= 5";
      (* Erlang uses /= for != *)
      "A_[a-z0-9]+ < B_[a-z0-9]+";
      "C_[a-z0-9]+ > D_[a-z0-9]+";
      "E_[a-z0-9]+ =< 100";
      (* Erlang uses =< for <= *)
      "F_[a-z0-9]+ >= 0";
    ]
  in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
      check bool ("contains pattern: " ^ part) true contains)
    expected_parts

(* Test for if condition with comparison operators *)
let test_compile_if_with_comparison () =
  let condition = BinOp (Var "x", "==", Literal (LInt 1)) in
  let if_expr =
    If (condition, Literal (LAtom "ok"), Some (SimpleElse (Literal (LAtom "error"))))
  in
  let func = make_single_clause_function "test" [ "x" ] if_expr in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  let expected_parts =
    [
      "test(X_[a-z0-9]+) ->";
      "case X_[a-z0-9]+ == 1 of true -> ok; _ -> error end.";
    ]
  in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
      check bool ("contains pattern: " ^ part) true contains)
    expected_parts

(* Test for complex comparison expressions *)
let test_compile_complex_comparisons () =
  (* For now, we'll test a simpler case since 'and' operator isn't implemented yet *)
  let simple_expr = BinOp (Var "x", ">=", Literal (LInt 0)) in
  let func = make_single_clause_function "is_positive" [ "x" ] simple_expr in
  let program = { deps = None; items = [ Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  let expected_parts = [ "is_positive(X_[a-z0-9]+) ->"; "X_[a-z0-9]+ >= 0" ] in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
      check bool ("contains pattern: " ^ part) true contains)
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
    ("compile if", `Quick, test_compile_if_then);
    ("compile if-else", `Quick, test_compile_if_then_else);
    ("compile comparison operators", `Quick, test_compile_comparison_operators);
    ("compile if with comparison", `Quick, test_compile_if_with_comparison);
    ("compile complex comparisons", `Quick, test_compile_complex_comparisons);
  ]
