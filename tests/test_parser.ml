open Alcotest
open Compiler.Ast

let test_simple_function () =
  let result = Compiler.parse_string "fun num() { 42 }" in
  match result.items with
  | [
   Function
     {
       name = "num";
       clauses = [ { params = []; body = Literal (LInt 42); position = _ } ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected single function with correct structure"

let test_function_with_params () =
  let result = Compiler.parse_string "fun add(x, y) { x }" in
  match result.items with
  | [
   Function
     {
       name = "add";
       clauses =
         [ { params = [ PVar "x"; PVar "y" ]; body = Var "x"; position = _ } ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected function with parameters"

let test_multiple_functions () =
  let result = Compiler.parse_string "fun first() { 1 } fun second() { 2 }" in
  match result.items with
  | [
   Function
     {
       name = "first";
       clauses = [ { params = []; body = Literal (LInt 1); position = _ } ];
       visibility = Private;
       position = _;
     };
   Function
     {
       name = "second";
       clauses = [ { params = []; body = Literal (LInt 2); position = _ } ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected two functions"

let test_public_function () =
  let result = Compiler.parse_string "pub fun hello() { \"world\" }" in
  match result.items with
  | [
   Function
     {
       name = "hello";
       clauses =
         [ { params = []; body = Literal (LString "world"); position = _ } ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected single public function"

let tests =
  [
    ("simple function", `Quick, test_simple_function);
    ("function with params", `Quick, test_function_with_params);
    ("multiple functions", `Quick, test_multiple_functions);
    ("public function", `Quick, test_public_function);
  ]
