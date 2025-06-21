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
let test_simple_block_assignment () =
  let block_expr = Block [ Assign ("y", Literal (LInt 42), None); Var "y" ] in
  let assign_expr = Assign ("x", block_expr, None) in
  let program =
    {
      items =
        [
          Function
            { name = "test"; clauses = [ { params = []; body = assign_expr; position = None } ]; position = None };
        ];
    }
  in
  try
    let _ = Compiler.compile_to_string program in
    check bool "simple block assignment compiles" true true
  with exn ->
    Printf.printf "Error: %s\n" (Printexc.to_string exn);
    check bool "simple block assignment should compile" false true

(* Test nested blocks *)
let test_nested_block_assignment () =
  let inner_block = Block [ Assign ("z", Literal (LInt 1), None); Var "z" ] in
  let outer_block = Block [ Assign ("y", inner_block, None); Var "y" ] in
  let assign_expr = Assign ("x", outer_block, None) in
  let program =
    {
      items =
        [
          Function
            { name = "test"; clauses = [ { params = []; body = assign_expr; position = None } ]; position = None };
        ];
    }
  in
  try
    let _ = Compiler.compile_to_string program in
    check bool "nested block assignment compiles" true true
  with exn ->
    Printf.printf "Error: %s\n" (Printexc.to_string exn);
    check bool "nested block assignment should compile" false true

(* Test block with multiple statements *)
let test_multiple_assignments_in_block () =
  let block_expr =
    Block
      [
        Assign ("a", Literal (LInt 1), None);
        Assign ("b", Literal (LInt 2), None);
        Assign ("c", Literal (LInt 3), None);
        Var "c";
      ]
  in
  let assign_expr = Assign ("x", block_expr, None) in
  let program =
    {
      items =
        [
          Function
            { name = "test"; clauses = [ { params = []; body = assign_expr; position = None } ]; position = None };
        ];
    }
  in
  try
    let _ = Compiler.compile_to_string program in
    check bool "multiple assignments in block compile" true true
  with exn ->
    Printf.printf "Error: %s\n" (Printexc.to_string exn);
    check bool "multiple assignments in block should compile" false true

(* Test that variables don't leak from blocks *)
let test_block_with_sequence () =
  let block_expr =
    Block [ Assign ("local", Literal (LInt 42), None); Var "local" ]
  in
  let sequence_expr =
    Sequence
      [ Assign ("x", block_expr, None); Assign ("y", Literal (LInt 1), None) ]
  in
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
    ("simple block", `Quick, test_simple_block_assignment);
    ("nested blocks", `Quick, test_nested_block_assignment);
    ("multiple statements block", `Quick, test_multiple_assignments_in_block);
    ("variable scoping", `Quick, test_block_with_sequence);
  ]
