(* Tests for the Type Checker *)
open Alcotest
open Compiler.Ast
open Compiler.Typechecker

let test_basic_types () =
  let program =
    {
      items =
        [
          Function { name = "test_int"; params = []; body = Literal (LInt 42) };
          Function
            {
              name = "test_string";
              params = [];
              body = Literal (LString "hello");
            };
          Function
            { name = "test_bool"; params = []; body = Literal (LBool true) };
        ];
    }
  in

  try
    let _ = type_check_program program in
    ()
  with TypeError error ->
    fail ("Basic types test failed: " ^ string_of_type_error error)

let test_function_types () =
  let program =
    {
      items =
        [
          Function
            {
              name = "add";
              params = [ "x"; "y" ];
              body = Var "x";
              (* Simplified to avoid undefined function *)
            };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "add" env with
    | Some (TFun (TVar _, TFun (TVar _, TVar _))) -> ()
    | _ -> fail "Function types test failed: unexpected type"
  with TypeError error ->
    fail ("Function types test failed: " ^ string_of_type_error error)

let test_let_expressions () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_let";
              params = [];
              body = Let ("x", Literal (LInt 10), Var "x");
            };
        ];
    }
  in

  try
    let _ = type_check_program program in
    ()
  with TypeError error ->
    fail ("Let expressions test failed: " ^ string_of_type_error error)

let test_type_error () =
  let program =
    {
      items =
        [
          Function
            { name = "type_error"; params = []; body = Var "nonexistent_var" };
        ];
    }
  in

  try
    let _ = type_check_program program in
    fail "Type error test failed: should have thrown error"
  with
  | TypeError (UnboundVariable _) -> ()
  | TypeError error ->
      fail
        ("Type error test failed: wrong error type: "
       ^ string_of_type_error error)

(* Test for nil type *)
let test_nil_type () =
  let program =
    {
      items =
        [ Function { name = "test_nil"; params = []; body = Literal LNil } ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_nil" env with
    | Some TNil -> ()
    | Some t ->
        fail ("Nil type test failed: expected TNil, got " ^ string_of_type t)
    | None -> fail "Nil type test failed: function not found"
  with TypeError error ->
    fail ("Nil type test failed: " ^ string_of_type_error error)

(* Test for integer type (renamed from int) *)
let test_integer_type () =
  let program =
    {
      items =
        [
          Function
            { name = "test_integer"; params = []; body = Literal (LInt 42) };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_integer" env with
    | Some TInteger -> ()
    | Some t ->
        fail
          ("Integer type test failed: expected TInteger, got "
         ^ string_of_type t)
    | None -> fail "Integer type test failed: function not found"
  with TypeError error ->
    fail ("Integer type test failed: " ^ string_of_type_error error)

(* Test for optional types with if-then without else *)
let test_optional_types () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_optional";
              params = [];
              body = If (Literal (LBool true), Literal (LInt 42), None);
            };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_optional" env with
    | Some (TOption TInteger) -> ()
    | Some t ->
        fail
          ("Optional type test failed: expected TOption TInteger, got "
         ^ string_of_type t)
    | None -> fail "Optional type test failed: function not found"
  with TypeError error ->
    fail ("Optional type test failed: " ^ string_of_type_error error)

(* Test for if-then-else with consistent types *)
let test_if_then_else_consistent () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_if_else";
              params = [];
              body =
                If
                  ( Literal (LBool true),
                    Literal (LInt 10),
                    Some (Literal (LInt 20)) );
            };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_if_else" env with
    | Some TInteger -> ()
    | Some t ->
        fail
          ("If-then-else test failed: expected TInteger, got "
         ^ string_of_type t)
    | None -> fail "If-then-else test failed: function not found"
  with TypeError error ->
    fail ("If-then-else test failed: " ^ string_of_type_error error)

(* Test for empty function body *)
let test_empty_function_body () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_empty";
              params = [];
              body = Literal LNil;
              (* Empty body should be parsed as nil *)
            };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_empty" env with
    | Some TNil -> ()
    | Some t ->
        fail
          ("Empty function test failed: expected TNil, got " ^ string_of_type t)
    | None -> fail "Empty function test failed: function not found"
  with TypeError error ->
    fail ("Empty function test failed: " ^ string_of_type_error error)

(* Test for tuple types *)
let test_tuple_types () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_tuple";
              params = [];
              body = Tuple [ Literal (LAtom "ok"); Literal (LInt 42) ];
            };
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_tuple" env with
    | Some (TTuple [ TAtom; TInteger ]) -> ()
    | Some t ->
        fail
          ("Tuple type test failed: expected TTuple [TAtom; TInteger], got "
         ^ string_of_type t)
    | None -> fail "Tuple type test failed: function not found"
  with TypeError error ->
    fail ("Tuple type test failed: " ^ string_of_type_error error)

let tests =
  [
    ("basic types", `Quick, test_basic_types);
    ("function types", `Quick, test_function_types);
    ("let expressions", `Quick, test_let_expressions);
    ("type error", `Quick, test_type_error);
    ("nil type", `Quick, test_nil_type);
    ("integer type", `Quick, test_integer_type);
    ("optional types", `Quick, test_optional_types);
    ("if-then-else consistent", `Quick, test_if_then_else_consistent);
    ("empty function body", `Quick, test_empty_function_body);
    ("tuple types", `Quick, test_tuple_types);
  ]
