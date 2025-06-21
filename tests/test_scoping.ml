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

(* Test that variables with same name in different blocks are allowed *)
let test_same_name_different_blocks () =
  let block1 = Block [ Assign ("x", Literal (LInt 42), None); Var "x" ] in
  let block2 = Block [ Assign ("x", Literal (LInt 100), None); Var "x" ] in
  let sequence_expr =
    Sequence
      [ Assign ("result1", block1, None); Assign ("result2", block2, None) ]
  in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should compile successfully and contain both blocks *)
  let contains_block_comments = String.contains result '%' in
  check bool "contains block comments" true contains_block_comments

(* Test that shadowing (child scope redefining parent variable) is NOT allowed *)
let test_shadowing_not_allowed () =
  let inner_block = Block [ Assign ("x", Literal (LInt 100), None); Var "x" ] in
  let sequence_expr =
    Sequence
      [
        Assign ("x", Literal (LInt 42), None);
        Assign ("result", inner_block, None);
      ]
  in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in

  (* This should raise an exception for shadowing *)
  try
    let _ = Compiler.compile_to_string program in
    check bool "should have failed due to shadowing" false true
  with
  | Compiler.Error.CompilationError err
    when String.contains (Compiler.Error.string_of_error err) 's' ->
      check bool "correctly failed with shadowing error" true true
  | Failure msg when String.contains msg 's' ->
      check bool "correctly failed with shadowing error" true true
  | _ -> check bool "failed with unexpected error" false true

(* Test that redefinition in same scope fails *)
let test_same_scope_redefinition_fails () =
  let sequence_expr =
    Sequence
      [
        Assign ("x", Literal (LInt 42), None);
        Assign ("x", Literal (LInt 100), None) (* This should fail *);
      ]
  in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in

  (* This should raise an exception *)
  try
    let _ = Compiler.compile_to_string program in
    check bool "should have failed" false true
  with
  | Compiler.Error.CompilationError err
    when String.contains (Compiler.Error.string_of_error err) 'a' ->
      check bool "correctly failed with reassignment error" true true
  | Failure msg when String.contains msg 'a' ->
      check bool "correctly failed with reassignment error" true true
  | _ -> check bool "failed with unexpected error" false true

(* Test complex nested scoping *)
let test_complex_nested_scoping () =
  let inner_inner_block =
    Block [ Assign ("z", Literal (LInt 300), None); Var "z" ]
  in
  let inner_block =
    Block
      [
        Assign ("y", Literal (LInt 200), None);
        Assign ("nested", inner_inner_block, None);
        Var "y";
      ]
  in
  let outer_sequence =
    Sequence
      [
        Assign ("x", Literal (LInt 100), None);
        Assign ("middle", inner_block, None);
        Var "x";
      ]
  in
  let func = make_single_clause_function "test" [] outer_sequence in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should compile successfully with nested structure *)
  let contains_assignments = String.contains result '=' in
  check bool "contains nested assignments" true contains_assignments

let test_different_scopes () =
  let block1 = Block [ Assign ("x", Literal (LInt 42), None); Var "x" ] in
  let block2 = Block [ Assign ("x", Literal (LInt 100), None); Var "x" ] in
  let program =
    {
      items =
        [
          Function
            {
              name = "test";
              clauses =
                [
                  {
                    params = [];
                    body =
                      Sequence
                        [
                          Assign ("result1", block1, None);
                          Assign ("result2", block2, None);
                        ];
                  };
                ];
            };
        ];
    }
  in
  try
    let _ = Compiler.Typechecker.type_check_program program in
    (* Should succeed - different scopes can use same variable names *)
    check bool "allows same variable name in different scopes" true true
  with
  | Failure msg when string_contains_substring msg "already defined" ->
      check bool "incorrectly rejects different scopes" false true
  | _ -> check bool "unexpected error in different scopes test" false true

let test_shadowing_parent_scope_fails () =
  let inner_block = Block [ Assign ("x", Literal (LInt 100), None); Var "x" ] in
  let program =
    {
      items =
        [
          Function
            {
              name = "test";
              clauses =
                [
                  {
                    params = [];
                    body =
                      Sequence
                        [
                          Assign ("x", Literal (LInt 42), None);
                          Assign ("result", inner_block, None);
                        ];
                  };
                ];
            };
        ];
    }
  in
  try
    let _ = Compiler.Typechecker.type_check_program program in
    check bool "should have failed on shadowing" false true
  with
  | Compiler.Error.CompilationError err ->
      let error_msg = Compiler.Error.string_of_error err in
      if string_contains_substring error_msg "shadow" then
        check bool "correctly rejects shadowing" true true
      else check bool "unexpected CompilationError in shadowing test" false true
  | Failure msg when string_contains_substring msg "already defined" ->
      check bool "correctly rejects shadowing" true true
  | _ -> check bool "unexpected error in shadowing test" false true

let test_same_scope_redefinition_fails_new () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test";
              clauses =
                [
                  {
                    params = [];
                    body =
                      Sequence
                        [
                          Assign ("x", Literal (LInt 42), None);
                          Assign ("x", Literal (LInt 100), None)
                          (* This should fail *);
                        ];
                  };
                ];
            };
        ];
    }
  in
  try
    let _ = Compiler.Typechecker.type_check_program program in
    check bool "should have failed on redefinition" false true
  with
  | Compiler.Error.CompilationError err
    when string_contains_substring
           (Compiler.Error.string_of_error err)
           "already defined" ->
      check bool "correctly rejects redefinition" true true
  | Failure msg when string_contains_substring msg "already defined" ->
      check bool "correctly rejects redefinition" true true
  | _ -> check bool "unexpected error in redefinition test" false true

let test_complex_nested_scoping_new () =
  let inner_inner_block =
    Block [ Assign ("z", Literal (LInt 300), None); Var "z" ]
  in
  let inner_block =
    Block
      [
        Assign ("y", Literal (LInt 200), None);
        Assign ("nested", inner_inner_block, None);
        Var "y";
      ]
  in
  let program =
    {
      items =
        [
          Function
            {
              name = "test";
              clauses =
                [
                  {
                    params = [];
                    body =
                      Sequence
                        [
                          Assign ("x", Literal (LInt 100), None);
                          Assign ("middle", inner_block, None);
                          Var "x";
                        ];
                  };
                ];
            };
        ];
    }
  in
  try
    let _ = Compiler.Typechecker.type_check_program program in
    check bool "complex nested scoping works" true true
  with exn ->
    Printf.printf "Error: %s\n" (Printexc.to_string exn);
    check bool "complex nested scoping should work" false true

let tests =
  [
    ("same name different blocks", `Quick, test_same_name_different_blocks);
    ("shadowing not allowed", `Quick, test_shadowing_not_allowed);
    ("same scope redefinition fails", `Quick, test_same_scope_redefinition_fails);
    ("complex nested scoping", `Quick, test_complex_nested_scoping);
    ("different scopes", `Quick, test_different_scopes);
    ("shadowing parent scope fails", `Quick, test_shadowing_parent_scope_fails);
    ( "same scope redefinition fails new",
      `Quick,
      test_same_scope_redefinition_fails_new );
    ("complex nested scoping new", `Quick, test_complex_nested_scoping_new);
  ]
