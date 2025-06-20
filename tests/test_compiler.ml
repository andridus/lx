open Alcotest
open Compiler.Ast

let test_compile_int () =
  let program = Expr (Literal (LInt 42)) in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> 42." in
  check string "compile integer" expected result

let test_compile_var () =
  let program = Expr (Var "x") in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> X." in
  check string "compile variable" expected result

let test_compile_let () =
  let program = Expr (Let ("x", Literal (LInt 42))) in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> X = 42." in
  check string "compile let expression" expected result

let test_compile_string () =
  let program = Expr (Literal (LString "hello")) in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> \"hello\"." in
  check string "compile string literal" expected result

let test_capitalize_var () =
  check string "capitalize empty" "" (Compiler.capitalize_var "");
  check string "capitalize single" "X" (Compiler.capitalize_var "x");
  check string "capitalize word" "Hello" (Compiler.capitalize_var "hello");
  check string "capitalize already caps" "Hello" (Compiler.capitalize_var "Hello")

let test_end_to_end () =
  let input = "let x = 42 in x" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> X = 42." in
  check string "end-to-end compilation" expected result

let tests = [
  ("compile integer literal", `Quick, test_compile_int);
  ("compile variable", `Quick, test_compile_var);
  ("compile let expression", `Quick, test_compile_let);
  ("compile string literal", `Quick, test_compile_string);
  ("capitalize variable names", `Quick, test_capitalize_var);
  ("end-to-end compilation", `Quick, test_end_to_end);
]