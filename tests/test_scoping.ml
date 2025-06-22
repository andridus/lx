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

(* Test that variables in different scopes get different names *)
let test_variable_scoping () =
  let block1 = Block [ Assign ("x", Literal (LInt 100), None); Var "x" ] in
  let block2 = Block [ Assign ("x", Literal (LInt 200), None); Var "x" ] in
  let sequence_expr =
    Sequence
      [ Assign ("result1", block1, None); Assign ("result2", block2, None) ]
  in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should contain unique variable names for x in different scopes *)
  let has_x_abc = string_contains_substring result "X_" in
  let has_result_def = string_contains_substring result "Result" in
  check bool "contains scoped variables" true (has_x_abc && has_result_def)

(* Test that ignored variables are compiled as underscore *)
let test_ignored_variables () =
  let sequence_expr =
    Sequence
      [
        Assign ("_result", Literal (LInt 42), None);
        Assign ("_debug", Literal (LString "test"), None);
        Literal (LAtom "ok");
      ]
  in
  let func =
    {
      name = "test";
      clauses = [ { params = []; body = sequence_expr; position = None } ];
      visibility = Private;
      position = None;
    }
  in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  (* Should not contain variable assignments for ignored variables *)
  let no_result_assignment = not (string_contains_substring result "Result") in
  let no_debug_assignment = not (string_contains_substring result "Debug") in
  check bool "ignored variables not assigned" true
    (no_result_assignment && no_debug_assignment)

let tests =
  [
    ("variable scoping", `Quick, test_variable_scoping);
    ("ignored variables", `Quick, test_ignored_variables);
  ]
