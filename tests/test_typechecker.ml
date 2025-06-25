(* Tests for the Type Checker *)
open Alcotest
open Compiler.Ast
open Compiler.Typechecker

let test_basic_types () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_int" [] (Literal (LInt 42)));
          Function
            (make_single_clause_function "test_string" []
               (Literal (LString "hello")));
          Function
            (make_single_clause_function "test_bool" [] (Literal (LBool true)));
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
      deps = None;
      items =
        [ Function (make_single_clause_function "add" [ "x"; "y" ] (Var "x")) ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "add" env with
    | Some (TFun (TVar _, TFun (TVar _, TVar _))) -> ()
    | _ -> fail "Function types test failed: unexpected type"
  with TypeError error ->
    fail ("Function types test failed: " ^ string_of_type_error error)

let test_type_error () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "type_error" [] (Var "nonexistent_var"));
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
      deps = None;
      items =
        [ Function (make_single_clause_function "test_nil" [] (Literal LNil)) ];
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
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_integer" [] (Literal (LInt 42)));
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
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_optional" []
               (If (Literal (LBool true), Literal (LInt 42), None)));
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

(* Test for if-else with consistent types *)
let test_if_then_else_consistent () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_if_else" []
               (If
                  ( Literal (LBool true),
                    Literal (LInt 10),
                    Some (SimpleElse (Literal (LInt 20))) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_if_else" env with
    | Some TInteger -> ()
    | Some t ->
        fail ("If-else test failed: expected TInteger, got " ^ string_of_type t)
    | None -> fail "If-else test failed: function not found"
  with TypeError error ->
    fail ("If-else test failed: " ^ string_of_type_error error)

(* Test for empty function body *)
let test_empty_function_body () =
  let program =
    {
      deps = None;
      items =
        [
          Function (make_single_clause_function "test_empty" [] (Literal LNil));
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
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_tuple" []
               (Tuple [ Literal (LAtom "ok"); Literal (LInt 42) ]));
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

(* Test for if with case clauses *)
let test_if_with_case () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_if_case" []
               (If
                  ( Literal (LBool true),
                    Literal (LString "yes"),
                    Some
                      (ClauseElse
                         [
                           (PVar "x", None, Literal (LString "no"));
                           (PWildcard, None, Literal (LString "default"));
                         ]) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_if_case" env with
    | Some TString -> ()
    | Some t ->
        fail ("If-case test failed: expected TString, got " ^ string_of_type t)
    | None -> fail "If-case test failed: function not found"
  with TypeError error ->
    fail ("If-case test failed: " ^ string_of_type_error error)

(* Test for with expressions with else *)
let test_with_else () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_result" []
               (Tuple [ Literal (LAtom "ok"); Literal (LString "test") ]));
          Function
            (make_single_clause_function "test_with_else" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "value" ],
                        App (Var "get_result", []) );
                    ],
                    Var "value",
                    Some (SimpleElse (Literal (LString "failed"))) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_with_else" env with
    | Some TString -> ()
    | Some t ->
        fail ("With-else test failed: expected TString, got " ^ string_of_type t)
    | None -> fail "With-else test failed: function not found"
  with TypeError error ->
    fail ("With-else test failed: " ^ string_of_type_error error)

(* Test for with expressions with case *)
let test_with_case () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_result" []
               (Tuple [ Literal (LAtom "ok"); Literal (LString "test") ]));
          Function
            (make_single_clause_function "test_with_case" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "value" ],
                        App (Var "get_result", []) );
                    ],
                    Var "value",
                    Some
                      (ClauseElse
                         [
                           ( PTuple [ PAtom "error"; PVar "reason" ],
                             None,
                             Var "reason" );
                           (PWildcard, None, Literal (LString "unknown"));
                         ]) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_with_case" env with
    | Some TString -> ()
    | Some t ->
        fail ("With-case test failed: expected TString, got " ^ string_of_type t)
    | None -> fail "With-case test failed: function not found"
  with TypeError error ->
    fail ("With-case test failed: " ^ string_of_type_error error)

(* Test for with expressions with guards *)
let test_with_case_guards () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_result" []
               (Tuple [ Literal (LAtom "error"); Literal (LString "timeout") ]));
          Function
            (make_single_clause_function "test_with_guards" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "value" ],
                        App (Var "get_result", []) );
                    ],
                    Var "value",
                    Some
                      (ClauseElse
                         [
                           ( PTuple [ PAtom "error"; PVar "reason" ],
                             Some
                               (GuardBinOp
                                  ( GuardAtomValue (GuardVar "reason"),
                                    "==",
                                    GuardAtomValue
                                      (GuardLiteral (LString "timeout")) )),
                             Literal (LString "timeout") );
                           (PWildcard, None, Literal (LString "other"));
                         ]) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_with_guards" env with
    | Some TString -> ()
    | Some t ->
        fail
          ("With-guards test failed: expected TString, got " ^ string_of_type t)
    | None -> fail "With-guards test failed: function not found"
  with TypeError error ->
    fail ("With-guards test failed: " ^ string_of_type_error error)

(* Test for multiple with steps *)
let test_with_multiple_steps () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_user" []
               (Tuple [ Literal (LAtom "ok"); Literal (LString "john") ]));
          Function
            (make_single_clause_function "get_role" [ "user" ]
               (Tuple [ Literal (LAtom "ok"); Literal (LString "admin") ]));
          Function
            (make_single_clause_function "test_with_multiple" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "user" ],
                        App (Var "get_user", []) );
                      ( PTuple [ PAtom "ok"; PVar "role" ],
                        App (Var "get_role", [ Var "user" ]) );
                    ],
                    Tuple [ Var "user"; Var "role" ],
                    Some
                      (SimpleElse
                         (Tuple
                            [
                              Literal (LString "error");
                              Literal (LString "failed");
                            ])) )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_with_multiple" env with
    | Some (TTuple [ TString; TString ]) -> ()
    | Some t ->
        fail
          ("With-multiple test failed: expected TTuple [TString; TString], got "
         ^ string_of_type t)
    | None -> fail "With-multiple test failed: function not found"
  with TypeError error ->
    fail ("With-multiple test failed: " ^ string_of_type_error error)

(* Test type error for mismatched branch types in if-case *)
let test_if_case_type_mismatch () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_mismatch" []
               (If
                  ( Literal (LBool true),
                    Literal (LString "string"),
                    Some
                      (ClauseElse
                         [
                           (PVar "x", None, Literal (LInt 42));
                           (PWildcard, None, Literal (LString "default"));
                         ]) )));
        ];
    }
  in

  try
    let _ = type_check_program program in
    fail "Type mismatch test failed: should have thrown error"
  with TypeError _ -> () (* Expected to fail *)

(* Test type error for mismatched branch types in with-case *)
let test_with_case_type_mismatch () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_result" []
               (Tuple [ Literal (LAtom "ok"); Literal (LString "test") ]));
          Function
            (make_single_clause_function "test_with_mismatch" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "value" ],
                        App (Var "get_result", []) );
                    ],
                    Var "value",
                    Some
                      (ClauseElse
                         [
                           ( PTuple [ PAtom "error"; PVar "reason" ],
                             None,
                             Literal (LInt 42) );
                           (PWildcard, None, Literal (LString "unknown"));
                         ]) )));
        ];
    }
  in

  try
    let _ = type_check_program program in
    fail "With type mismatch test failed: should have thrown error"
  with TypeError _ -> () (* Expected to fail *)

(* Test if-else with optional result *)
let test_if_else_optional () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "test_optional" []
               (If (Literal (LBool true), Literal (LString "some"), None)));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_optional" env with
    | Some (TOption TString) -> ()
    | Some t ->
        fail
          ("Optional if test failed: expected TOption TString, got "
         ^ string_of_type t)
    | None -> fail "Optional if test failed: function not found"
  with TypeError error ->
    fail ("Optional if test failed: " ^ string_of_type_error error)

(* Test with-else with optional result *)
let test_with_optional () =
  let program =
    {
      deps = None;
      items =
        [
          Function
            (make_single_clause_function "get_result" []
               (Tuple [ Literal (LAtom "ok"); Literal (LString "test") ]));
          Function
            (make_single_clause_function "test_with_optional" []
               (With
                  ( [
                      ( PTuple [ PAtom "ok"; PVar "value" ],
                        App (Var "get_result", []) );
                    ],
                    Var "value",
                    None )));
        ];
    }
  in

  try
    let env = type_check_program program in
    match List.assoc_opt "test_with_optional" env with
    | Some (TOption TString) -> ()
    | Some t ->
        fail
          ("Optional with test failed: expected TOption TString, got "
         ^ string_of_type t)
    | None -> fail "Optional with test failed: function not found"
  with TypeError error ->
    fail ("Optional with test failed: " ^ string_of_type_error error)

let tests =
  [
    ("basic types", `Quick, test_basic_types);
    ("function types", `Quick, test_function_types);
    ("type error", `Quick, test_type_error);
    ("nil type", `Quick, test_nil_type);
    ("integer type", `Quick, test_integer_type);
    ("optional types", `Quick, test_optional_types);
    ("if-else consistent", `Quick, test_if_then_else_consistent);
    ("empty function body", `Quick, test_empty_function_body);
    ("tuple types", `Quick, test_tuple_types);
    ("if with case", `Quick, test_if_with_case);
    ("with else", `Quick, test_with_else);
    ("with case", `Quick, test_with_case);
    ("with case guards", `Quick, test_with_case_guards);
    ("with multiple steps", `Quick, test_with_multiple_steps);
    ("if case type mismatch", `Quick, test_if_case_type_mismatch);
    ("with case type mismatch", `Quick, test_with_case_type_mismatch);
    ("if else optional", `Quick, test_if_else_optional);
    ("with optional", `Quick, test_with_optional);
  ]
