open Alcotest
open Compiler.Ast

let string_contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec search i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else search (i + 1)
  in
  search 0

(* Test strict logical operators (and, or, not) *)
let test_strict_logical_operators () =
  let test_cases = [
    ("fun test(a, b) { a and b }", ["and"]);
    ("fun test(a, b) { a or b }", ["or"]);
    ("fun test(a) { not a }", ["not"]);
  ] in

  List.iter (fun (input, expected_parts) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    List.iter (fun part ->
      let contains = string_contains_substring result part in
      check bool ("strict operator: " ^ input ^ " contains: " ^ part) true contains
    ) expected_parts
  ) test_cases

(* Test short-circuit logical operators (andalso, orelse) *)
let test_short_circuit_operators () =
  let test_cases = [
    ("fun test(a, b) { a andalso b }", ["andalso"]);
    ("fun test(a, b) { a orelse b }", ["orelse"]);
  ] in

  List.iter (fun (input, expected_parts) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    List.iter (fun part ->
      let contains = string_contains_substring result part in
      check bool ("short-circuit operator: " ^ input ^ " contains: " ^ part) true contains
    ) expected_parts
  ) test_cases

(* Test operator precedence *)
let test_logical_precedence () =
  let input = "fun test(a, b, c) { a orelse b andalso c }" in
  let program = Compiler.parse_string input in
  match program.items with
  | [Function { clauses = [{ body = BinOp (_, "orelse", BinOp (_, "andalso", _)); _ }]; _ }] ->
      check bool "orelse has lower precedence than andalso" true true
  | _ ->
      Alcotest.fail "Expected orelse with lower precedence than andalso"

(* Test complex logical expressions *)
let test_complex_logical_expressions () =
  let test_cases = [
    ("fun test(a, b, c) { a and b or c }", ["and"; "or"]);
    ("fun test(a, b, c) { a andalso b orelse c }", ["andalso"; "orelse"]);
    ("fun test(a, b) { not (a and b) }", ["not"; "and"]);
    ("fun test(a, b, c, d) { (a or b) and (c or d) }", ["or"; "and"]);
  ] in

  List.iter (fun (input, expected_parts) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    List.iter (fun part ->
      let contains = string_contains_substring result part in
      check bool ("complex expression: " ^ input ^ " contains: " ^ part) true contains
    ) expected_parts
  ) test_cases

(* Test logical operators with comparisons *)
let test_logical_with_comparisons () =
  let test_cases = [
    ("fun test(x, y) { x > 0 and y < 10 }", [">"; "and"; "<"]);
    ("fun test(x, y) { x == 0 or y != 5 }", ["=="; "or"; "/="]);
    ("fun test(x) { not (x >= 100) }", ["not"; ">="]);
    ("fun test(x, y) { x > 0 andalso y < 10 }", [">"; "andalso"; "<"]);
  ] in

  List.iter (fun (input, expected_parts) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    List.iter (fun part ->
      let contains = string_contains_substring result part in
      check bool ("logical with comparison: " ^ input ^ " contains: " ^ part) true contains
    ) expected_parts
  ) test_cases

(* Test logical operators in if expressions *)
let test_logical_in_if () =
  let input = "fun test(x, y) { if x > 0 and y < 10 { :valid } else { :invalid } }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_and = string_contains_substring result "and" in
  let contains_case = string_contains_substring result "case" in
  check bool "if with logical contains 'and'" true contains_and;
  check bool "if compiles to case" true contains_case

(* Test logical operators in guards *)
let test_logical_in_guards () =
  let test_cases = [
    ("fun test(x, y) when x > 0 and y < 10 { :valid }", [" when "; ", "]);
    ("fun test(x, y) when x == 0 or y != 5 { :special }", [" when "; "; "]);
    ("fun test(x) when not is_atom(x) { :not_atom }", [" when "; "not"]);
  ] in

  List.iter (fun (input, expected_parts) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    List.iter (fun part ->
      let contains = string_contains_substring result part in
      check bool ("guard with logical: " ^ input ^ " contains: " ^ part) true contains
    ) expected_parts
  ) test_cases

(* Test AST structure for logical operators *)
let test_logical_ast () =
  let program = Compiler.parse_string "fun test(a, b) { a and b }" in
  match program.items with
  | [Function { clauses = [{ body = BinOp (Var "a", "and", Var "b"); _ }]; _ }] ->
      check bool "and operator parsed correctly" true true
  | _ ->
      Alcotest.fail "Expected BinOp with 'and' operator"

let test_unary_ast () =
  let program = Compiler.parse_string "fun test(a) { not a }" in
  match program.items with
  | [Function { clauses = [{ body = UnaryOp ("not", Var "a"); _ }]; _ }] ->
      check bool "not operator parsed correctly" true true
  | _ ->
      Alcotest.fail "Expected UnaryOp with 'not' operator"

let tests = [
  ("strict logical operators", `Quick, test_strict_logical_operators);
  ("short-circuit operators", `Quick, test_short_circuit_operators);
  ("logical precedence", `Quick, test_logical_precedence);
  ("complex logical expressions", `Quick, test_complex_logical_expressions);
  ("logical with comparisons", `Quick, test_logical_with_comparisons);
  ("logical in if", `Quick, test_logical_in_if);
  ("logical in guards", `Quick, test_logical_in_guards);
  ("logical AST", `Quick, test_logical_ast);
  ("unary AST", `Quick, test_unary_ast);
]