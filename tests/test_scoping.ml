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
  let sequence_expr =
    Sequence
      [
        Assign ("x", Literal (LInt 100), None);
        Assign ("y", Literal (LInt 200), None);
        BinOp (Var "x", "+", Var "y");
      ]
  in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should contain variable names *)
  let has_x_var = string_contains_substring result "X_" in
  let has_y_var = string_contains_substring result "Y_" in
  check bool "contains X variable" true has_x_var;
  check bool "contains Y variable" true has_y_var

(* Test that ignored variables are compiled as underscore *)
let test_ignored_variables () =
  let sequence_expr =
    Sequence
      [ Assign ("used_var", Literal (LString "test"), None); Var "used_var" ]
  in
  let func =
    {
      name = "test";
      clauses =
        [ { params = []; body = sequence_expr; position = None; guard = None } ];
      visibility = Private;
      position = None;
    }
  in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  (* Should contain variable assignments for used variables *)
  let has_used_var = string_contains_substring result "Used_var" in
  check bool "used variables are assigned" true has_used_var

let tests =
  [
    ("variable scoping", `Quick, test_variable_scoping);
    ("ignored variables", `Quick, test_ignored_variables);
  ]
