open Compiler.Ast
open Alcotest

let test_parse_simple_function () =
  let program = Compiler.parse_string "fun num() { 42 }" in
  match program.items with
  | [Function { name = "num"; params = []; body = Literal (LInt 42) }] -> ()
  | _ -> fail "Expected simple function definition"

let test_parse_function_with_params () =
  let program = Compiler.parse_string "fun add(x, y) { x }" in
  match program.items with
  | [Function { name = "add"; params = ["x"; "y"]; body = Var "x" }] -> ()
  | _ -> fail "Expected function with parameters"

let test_parse_multiple_functions () =
  let program = Compiler.parse_string "fun first() { 1 } fun second() { 2 }" in
  match program.items with
  | [Function { name = "first"; params = []; body = Literal (LInt 1) };
     Function { name = "second"; params = []; body = Literal (LInt 2) }] -> ()
  | _ -> fail "Expected two function definitions"

let test_parse_empty_program () =
  let program = Compiler.parse_string "" in
  match program.items with
  | [] -> ()
  | _ -> fail "Expected empty program"

let test_parse_literals () =
  let test_cases = [
    ("fun num() { 42 }", fun items ->
      match items with
      | [Function { body = Literal (LInt 42); _ }] -> true
      | _ -> false);
    ("fun str() { \"hello\" }", fun items ->
      match items with
      | [Function { body = Literal (LString "hello"); _ }] -> true
      | _ -> false);
    ("fun bool() { true }", fun items ->
      match items with
      | [Function { body = Literal (LBool true); _ }] -> true
      | _ -> false);
    ("fun atom() { :atom }", fun items ->
      match items with
      | [Function { body = Literal (LAtom "atom"); _ }] -> true
      | _ -> false);
  ] in

  List.iter (fun (input, check_fn) ->
    let program = Compiler.parse_string input in
    if not (check_fn program.items) then
      fail ("Failed to parse: " ^ input)
  ) test_cases

let tests = [
  ("parse simple function", `Quick, test_parse_simple_function);
  ("parse function with parameters", `Quick, test_parse_function_with_params);
  ("parse multiple functions", `Quick, test_parse_multiple_functions);
  ("parse empty program", `Quick, test_parse_empty_program);
  ("parse various literals", `Quick, test_parse_literals);
]