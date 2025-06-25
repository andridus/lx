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
  let test_cases =
    [
      ("def test(a, b) do a and b end", [ "and" ]);
      ("def test(a, b) do a or b end", [ "or" ]);
      ("def test(a) do not a end", [ "not" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("strict operator: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test short-circuit logical operators (andalso, orelse) *)
let test_short_circuit_operators () =
  let test_cases =
    [
      ("def test(a, b) do a andalso b end", [ "andalso" ]);
      ("def test(a, b) do a orelse b end", [ "orelse" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("short-circuit operator: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test operator precedence *)
let test_logical_precedence () =
  let input = "def test(a, b, c) do a orelse b andalso c end" in
  let program = Compiler.parse_string input in
  match program.items with
  | [
   Function
     {
       clauses = [ { body = BinOp (_, "orelse", BinOp (_, "andalso", _)); _ } ];
       _;
     };
  ] ->
      check bool "orelse has lower precedence than andalso" true true
  | _ -> Alcotest.fail "Expected orelse with lower precedence than andalso"

(* Test complex logical expressions *)
let test_complex_logical_expressions () =
  let test_cases =
    [
      ("def test(a, b, c) do a and b or c end", [ "and"; "or" ]);
      ("def test(a, b, c) do a andalso b orelse c end", [ "andalso"; "orelse" ]);
      ("def test(a, b) do not (a and b) end", [ "not"; "and" ]);
      ("def test(a, b, c, d) do (a or b) and (c or d) end", [ "or"; "and" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("complex expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test logical operators with comparisons *)
let test_logical_with_comparisons () =
  let test_cases =
    [
      ("def test(x, y) do x > 0 and y < 10 end", [ ">"; "and"; "<" ]);
      ("def test(x, y) do x == 0 or y != 5 end", [ "=="; "or"; "/=" ]);
      ("def test(x) do not (x >= 100) end", [ "not"; ">=" ]);
      ("def test(x, y) do x > 0 andalso y < 10 end", [ ">"; "andalso"; "<" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("logical with comparison: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test logical operators in if expressions *)
let test_logical_in_if () =
  let input =
    "def test(x, y) do if x > 0 and y < 10 do :valid else :invalid end end"
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in
  let contains_and = string_contains_substring result "and" in
  let contains_case = string_contains_substring result "case" in
  check bool "if with logical contains 'and'" true contains_and;
  check bool "if compiles to case" true contains_case

(* Test logical operators in guards *)
let test_logical_in_guards () =
  let test_cases =
    [
      ("def test(x, y) when x > 0 and y < 10 do :valid end", [ " when "; ", " ]);
      ( "def test(x, y) when x == 0 or y != 5 do :special end",
        [ " when "; "; " ] );
      ("def test(x) when not is_atom(x) do :not_atom end", [ " when "; "not" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("guard with logical: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test AST structure for logical operators *)
let test_logical_ast () =
  let program = Compiler.parse_string "def test(a, b) do a and b end" in
  match program.items with
  | [
   Function { clauses = [ { body = BinOp (Var "a", "and", Var "b"); _ } ]; _ };
  ] ->
      check bool "and operator parsed correctly" true true
  | _ -> Alcotest.fail "Expected BinOp with 'and' operator"

let test_unary_ast () =
  let program = Compiler.parse_string "def test(a) do not a end" in
  match program.items with
  | [ Function { clauses = [ { body = UnaryOp ("not", Var "a"); _ } ]; _ } ] ->
      check bool "not operator parsed correctly" true true
  | _ -> Alcotest.fail "Expected UnaryOp with 'not' operator"

let tests =
  [
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
