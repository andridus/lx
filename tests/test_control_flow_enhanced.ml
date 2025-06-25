open Alcotest
open Compiler.Ast
open Compiler.Parser
open Compiler.Lexer
open Compiler.Typechecker

(* Helper function to parse expressions *)
let parse_expr_helper input =
  let lexbuf = Lexing.from_string input in
  try
    let parsed = main read lexbuf in
    match parsed.items with
    | [ Function func ] -> (
        match func.clauses with
        | [ clause ] -> clause.body
        | _ -> failwith "Expected single clause")
    | _ -> failwith "Expected single function"
  with _ -> failwith ("Failed to parse: " ^ input)

(* Helper function to parse and compile *)
let parse_and_compile input =
  let lexbuf = Lexing.from_string input in
  try
    let parsed = main read lexbuf in
    let compiled = Compiler.compile_to_string_for_tests parsed in
    compiled
  with _ -> failwith ("Failed to parse/compile: " ^ input)

(* Helper function to check substring *)
let contains_substring s ~substring =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(* Test if expressions with else *)
let test_if_else () =
  let input =
    {|
    defp test(x) do
      if x == 1 do
        "one"
      else
        "other"
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | If
      ( BinOp (Var "x", "==", Literal (LInt 1)),
        Literal (LString "one"),
        Some (SimpleElse (Literal (LString "other"))) ) ->
      ()
  | _ -> Alcotest.fail "Expected if-else expression"

(* Test if expressions without else/case *)
let test_if_no_else () =
  let input =
    {|
    defp test(x) do
      if x == 1 do
        "one"
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | If (BinOp (Var "x", "==", Literal (LInt 1)), Literal (LString "one"), None)
    ->
      ()
  | _ -> Alcotest.fail "Expected if without else"

(* Test with expressions with else *)
let test_with_else () =
  let input =
    {|
    defp test() do
      with .{:ok, value} <= get_result() do
        value
      else
        "failed"
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | With (steps, Var "value", Some (SimpleElse (Literal (LString "failed")))) ->
      check int "with steps count" 1 (List.length steps)
  | _ -> Alcotest.fail "Expected with-else expression"

(* Test with expressions with case *)
let test_with_case () =
  let input =
    {|
    defp test() do
      with .{:ok, value} <= get_result() do
        value
      case
        .{:error, reason} -> reason
        _ -> "unknown"
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | With (steps, Var "value", Some (ClauseElse clauses)) ->
      check int "with steps count" 1 (List.length steps);
      check int "case clauses count" 2 (List.length clauses)
  | _ -> Alcotest.fail "Expected with-case expression"

(* Test with expressions without else/case *)
let test_with_no_else () =
  let input =
    {|
    defp test() do
      with .{:ok, value} <= get_result() do
        value
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | With (steps, Var "value", None) ->
      check int "with steps count" 1 (List.length steps)
  | _ -> Alcotest.fail "Expected with without else"

(* Test multiple with steps *)
let test_with_multiple_steps () =
  let input =
    {|
    defp test() do
      with .{:ok, user} <= get_user(),
           .{:ok, role} <= get_role(user) do
        .{user, role}
      else
        .{:error, "failed"}
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | With (steps, Tuple [ Var "user"; Var "role" ], Some (SimpleElse _)) ->
      check int "with steps count" 2 (List.length steps)
  | _ -> Alcotest.fail "Expected with multiple steps"

(* Test with guards in case *)
let test_with_case_guards () =
  let input =
    {|
    defp test() do
      with .{:ok, value} <= get_result() do
        value
      case
        .{:error, reason} when reason == :timeout -> "timeout"
        .{:error, _} -> "other error"
        _ -> "unknown"
      end
    end
  |}
  in
  let expr = parse_expr_helper input in
  match expr with
  | With (_, _, Some (ClauseElse clauses)) -> (
      check int "case clauses count" 3 (List.length clauses);
      (* Check that first clause has a guard *)
      match List.hd clauses with
      | _, Some _, _ -> ()
      | _ -> Alcotest.fail "Expected guard in first clause")
  | _ -> Alcotest.fail "Expected with-case with guards"

(* Test if compilation to Erlang *)
let test_if_compilation () =
  let input =
    {|
    def test_if_else(x) do
      if x == 1 do
        "one"
      else
        "other"
      end
    end
  |}
  in
  let compiled = parse_and_compile input in
  check bool "contains if-else pattern" true
    (contains_substring compiled ~substring:"case")

(* Test with compilation to Erlang *)
let test_with_compilation () =
  let input =
    {|
    def get_result() do .{:ok, "test"} end

    def test_with_else() do
      with .{:ok, value} <= get_result() do
        value
      else
        "failed"
      end
    end

    def test_with_case() do
      with .{:ok, value} <= get_result() do
        value
      case
        .{:error, _} -> "error"
        _ -> "unknown"
      end
    end
  |}
  in
  let compiled = parse_and_compile input in
  check bool "contains with pattern" true
    (contains_substring compiled ~substring:"case get_result()");
  check bool "contains nested case" true
    (contains_substring compiled ~substring:"case true of")

(* Test type checking for if expressions *)
let test_if_type_checking () =
  let input =
    {|
    defp test_if_else(x) do
      if x == 1 do
        "one"
      else
        "other"
      end
    end
  |}
  in
  let lexbuf = Lexing.from_string input in
  let parsed = main read lexbuf in
  try
    let _ = type_check_program parsed in
    () (* Should succeed *)
  with _ -> Alcotest.fail "Type checking should succeed for if-else"

(* Test type checking for with expressions *)
let test_with_type_checking () =
  let input =
    {|
    defp get_result() do .{:ok, "test"} end

    defp test_with_else() do
      with .{:ok, value} <= get_result() do
        value
      else
        "failed"
      end
    end
  |}
  in
  let lexbuf = Lexing.from_string input in
  let parsed = main read lexbuf in
  try
    let _ = type_check_program parsed in
    () (* Should succeed *)
  with _ -> Alcotest.fail "Type checking should succeed for with-else"

(* Test type error for mismatched branch types *)
let test_type_error_mismatched_branches () =
  let input =
    {|
    defp test_bad_types(x) do
      if x == 1 do
        "string"
      else
        42
      end
    end
  |}
  in
  let lexbuf = Lexing.from_string input in
  let parsed = main read lexbuf in
  try
    let _ = type_check_program parsed in
    Alcotest.fail "Should have type error for mismatched branch types"
  with _ -> () (* Should fail with type error *)

(* Test suite *)
let tests =
  [
    ("if with else", `Quick, test_if_else);
    ("if without else", `Quick, test_if_no_else);
    ("with with else", `Quick, test_with_else);
    ("with with case", `Quick, test_with_case);
    ("with without else", `Quick, test_with_no_else);
    ("with multiple steps", `Quick, test_with_multiple_steps);
    ("with case guards", `Quick, test_with_case_guards);
    ("if compilation", `Quick, test_if_compilation);
    ("with compilation", `Quick, test_with_compilation);
    ("if type checking", `Quick, test_if_type_checking);
    ("with type checking", `Quick, test_with_type_checking);
    ( "type error mismatched branches",
      `Quick,
      test_type_error_mismatched_branches );
  ]
