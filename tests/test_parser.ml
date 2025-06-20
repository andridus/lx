open Compiler.Ast

let test_parse_int () =
  let program = Compiler.parse_string "42" in
  match program with
  | Expr (Literal (LInt 42)) -> ()
  | _ -> Alcotest.fail "Expected Expr (Literal (LInt 42))"

let test_parse_var () =
  let program = Compiler.parse_string "x" in
  match program with
  | Expr (Var "x") -> ()
  | _ -> Alcotest.fail "Expected Expr (Var \"x\")"

let test_parse_let_simple () =
  let program = Compiler.parse_string "let x = 42 in x" in
  match program with
  | Expr (Let ("x", Literal (LInt 42))) -> ()
  | _ -> Alcotest.fail "Expected Let expression with x = 42"

let test_parse_nested_let () =
  let program = Compiler.parse_string "let x = 42 in let y = x in y" in
  match program with
  | Expr (Let ("x", Literal (LInt 42))) -> ()
  | _ -> Alcotest.fail "Expected nested Let expression"

let test_parse_complex_expr () =
  let program = Compiler.parse_string "let answer = 42 in answer" in
  match program with
  | Expr (Let ("answer", Literal (LInt 42))) -> ()
  | _ -> Alcotest.fail "Expected Let expression with answer = 42"

let tests = [
  ("parse integer literal", `Quick, test_parse_int);
  ("parse variable", `Quick, test_parse_var);
  ("parse simple let expression", `Quick, test_parse_let_simple);
  ("parse nested let expression", `Quick, test_parse_nested_let);
  ("parse complex expression", `Quick, test_parse_complex_expr);
]