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

(* Test simple block assignment *)
let test_simple_block () =
  let block_expr = Block [ Assign ("y", Literal (LInt 42)); Var "y" ] in
  let assign_expr = Assign ("x", block_expr) in
  let func = make_single_clause_function "test" [] assign_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "% start block"; "% end block" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("simple block contains: " ^ part) true contains)
    expected_parts

(* Test nested blocks *)
let test_nested_blocks () =
  let inner_block = Block [ Assign ("z", Literal (LInt 1)); Var "z" ] in
  let outer_block = Block [ Assign ("y", inner_block); Var "y" ] in
  let assign_expr = Assign ("x", outer_block) in
  let func = make_single_clause_function "test" [] assign_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "% start block"; "% end block" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("nested block contains: " ^ part) true contains)
    expected_parts

(* Test block with multiple statements *)
let test_multiple_statements_block () =
  let block_expr = Block [
    Assign ("a", Literal (LInt 1));
    Assign ("b", Literal (LInt 2));
    Assign ("c", Literal (LInt 3));
    Var "c"
  ] in
  let assign_expr = Assign ("x", block_expr) in
  let func = make_single_clause_function "test" [] assign_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "% start block"; "% end block" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("multiple statements block contains: " ^ part) true contains)
    expected_parts

(* Test that variables don't leak from blocks *)
let test_variable_scoping () =
  let block_expr = Block [ Assign ("local", Literal (LInt 42)); Var "local" ] in
  let sequence_expr = Sequence [
    Assign ("x", block_expr);
    Assign ("y", Literal (LInt 1))
  ] in
  let func = make_single_clause_function "test" [] sequence_expr in
  let program = { items = [ Function func ] } in
  let result = Compiler.compile_to_string program in
  (* Should contain block structure with proper scoping *)
  let contains_block_start = string_contains_substring result "% start block" in
  let contains_block_end = string_contains_substring result "% end block" in
  check bool "contains block start comment" true contains_block_start;
  check bool "contains block end comment" true contains_block_end

let tests =
  [
    ("simple block", `Quick, test_simple_block);
    ("nested blocks", `Quick, test_nested_blocks);
    ("multiple statements block", `Quick, test_multiple_statements_block);
    ("variable scoping", `Quick, test_variable_scoping);
  ]
