(* Tests for the Type Checker *)
open Alcotest
open Compiler.Ast
open Compiler.Typechecker

let test_basic_types () =
  let program = {
    items = [
      Function {
        name = "test_int";
        params = [];
        body = Literal (LInt 42);
      };
      Function {
        name = "test_string";
        params = [];
        body = Literal (LString "hello");
      };
      Function {
        name = "test_bool";
        params = [];
        body = Literal (LBool true);
      }
    ]
  } in

  try
    let _ = type_check_program program in
    ()
  with
  | TypeError error ->
      fail ("Basic types test failed: " ^ string_of_type_error error)

let test_function_types () =
  let program = {
    items = [
      Function {
        name = "add";
        params = ["x"; "y"];
        body = Var "x"; (* Simplified to avoid undefined function *)
      }
    ]
  } in

  try
    let env = type_check_program program in
    match List.assoc_opt "add" env with
    | Some (TFun (TVar _, TFun (TVar _, TVar _))) ->
        ()
    | _ ->
        fail "Function types test failed: unexpected type"
  with
  | TypeError error ->
      fail ("Function types test failed: " ^ string_of_type_error error)

let test_let_expressions () =
  let program = {
    items = [
      Function {
        name = "test_let";
        params = [];
        body = Let ("x", Literal (LInt 10), Var "x");
      }
    ]
  } in

  try
    let _ = type_check_program program in
    ()
  with
  | TypeError error ->
      fail ("Let expressions test failed: " ^ string_of_type_error error)

let test_type_error () =
  let program = {
    items = [
      Function {
        name = "type_error";
        params = [];
        body = Var "nonexistent_var";
      }
    ]
  } in

  try
    let _ = type_check_program program in
    fail "Type error test failed: should have thrown error"
  with
  | TypeError (UnboundVariable _) ->
      ()
  | TypeError error ->
      fail ("Type error test failed: wrong error type: " ^ string_of_type_error error)

let tests = [
  ("basic types", `Quick, test_basic_types);
  ("function types", `Quick, test_function_types);
  ("let expressions", `Quick, test_let_expressions);
  ("type error", `Quick, test_type_error);
]