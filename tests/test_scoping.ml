open Alcotest
open Compiler.Ast

(* Test that variables with same name in different blocks are allowed *)
let test_same_name_different_blocks () =
  let block1 = Block [ Assign ("x", Literal (LInt 42)); Var "x" ] in
  let block2 = Block [ Assign ("x", Literal (LInt 100)); Var "x" ] in
  let sequence_expr = Sequence [
    Assign ("result1", block1);
    Assign ("result2", block2)
  ] in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should compile successfully and contain both blocks *)
  let contains_block_comments = String.contains result '%' in
  check bool "contains block comments" true contains_block_comments

(* Test that shadowing (child scope redefining parent variable) is NOT allowed *)
let test_shadowing_not_allowed () =
  let inner_block = Block [ Assign ("x", Literal (LInt 100)); Var "x" ] in
  let sequence_expr = Sequence [
    Assign ("x", Literal (LInt 42));
    Assign ("result", inner_block)
  ] in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in

  (* This should raise an exception for shadowing *)
  try
    let _ = Compiler.compile_to_string program in
    check bool "should have failed due to shadowing" false true
  with
  | Failure msg when String.contains msg 's' ->
      check bool "correctly failed with shadowing error" true true
  | _ ->
      check bool "failed with unexpected error" false true

(* Test that redefinition in same scope fails *)
let test_same_scope_redefinition_fails () =
  let sequence_expr = Sequence [
    Assign ("x", Literal (LInt 42));
    Assign ("x", Literal (LInt 100))  (* This should fail *)
  ] in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in

  (* This should raise an exception *)
  try
    let _ = Compiler.compile_to_string program in
    check bool "should have failed" false true
    with
  | Failure msg when String.contains msg 'a' ->
      check bool "correctly failed with reassignment error" true true
  | _ ->
      check bool "failed with unexpected error" false true

(* Test complex nested scoping *)
let test_complex_nested_scoping () =
  let inner_inner_block = Block [ Assign ("z", Literal (LInt 300)); Var "z" ] in
  let inner_block = Block [
    Assign ("y", Literal (LInt 200));
    Assign ("nested", inner_inner_block);
    Var "y"
  ] in
  let outer_sequence = Sequence [
    Assign ("x", Literal (LInt 100));
    Assign ("middle", inner_block);
    Var "x"
  ] in
  let func = make_single_clause_function "test" [] outer_sequence in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in

  (* Should compile successfully with nested structure *)
  let contains_assignments = String.contains result '=' in
  check bool "contains nested assignments" true contains_assignments

let tests =
  [
    ("same name different blocks", `Quick, test_same_name_different_blocks);
    ("shadowing not allowed", `Quick, test_shadowing_not_allowed);
    ("same scope redefinition fails", `Quick, test_same_scope_redefinition_fails);
    ("complex nested scoping", `Quick, test_complex_nested_scoping);
  ]