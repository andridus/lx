open Alcotest
open Compiler.Ast

let test_simple_function () =
  let result = Compiler.parse_string "fun num() { 42 }" in
  match result.items with
  | [
   Function
     {
       name = "num";
               clauses = [ { params = []; body = Literal (LInt 42); position = _; guard = _ } ];
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
                   [ { params = [ PVar "x"; PVar "y" ]; body = Var "x"; position = _; guard = _ } ];
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
       clauses = [ { params = []; body = Literal (LInt 1); position = _; guard = _ } ];
       visibility = Private;
       position = _;
     };
   Function
     {
       name = "second";
       clauses = [ { params = []; body = Literal (LInt 2); position = _; guard = _ } ];
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
         [ { params = []; body = Literal (LString "world"); position = _; guard = _ } ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected single public function"

(* Test parsing of comparison operators *)
let test_comparison_operators () =
  let test_cases =
    [
      ("x == 1", "==");
      ("y != 5", "!=");
      ("a < b", "<");
      ("c > d", ">");
      ("e <= 10", "<=");
      ("f >= 0", ">=");
    ]
  in
  List.iter
    (fun (expr_str, op) ->
      let code = "fun test() { " ^ expr_str ^ " }" in
      let result = Compiler.parse_string code in
      match result.items with
      | [
       Function
         {
           name = "test";
           clauses =
             [ { params = []; body = BinOp (_, actual_op, _); position = _; guard = _ } ];
           visibility = Private;
           position = _;
         };
      ] ->
          check string ("operator " ^ op) op actual_op
      | _ -> fail ("Failed to parse comparison operator: " ^ op))
    test_cases

(* Test parsing of if condition with comparison *)
let test_if_with_comparison () =
  let result =
    Compiler.parse_string "fun test() { if x == 1 { :ok } else { :error } }"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             params = [];
             body =
               If
                 ( BinOp (Var "x", "==", Literal (LInt 1)),
                   Literal (LAtom "ok"),
                   Some (Literal (LAtom "error")) );
             position = _;
             guard = _;
           };
         ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected if-else with comparison condition"

(* Test parsing of complex comparison expressions *)
let test_complex_comparison_parsing () =
  let result = Compiler.parse_string "fun test(x, y) { x >= y }" in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             params = [ PVar "x"; PVar "y" ];
             body = BinOp (Var "x", ">=", Var "y");
             position = _;
             guard = _;
           };
         ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected function with comparison between parameters"

(* Test parsing precedence of comparison operators *)
let test_comparison_precedence () =
  let result = Compiler.parse_string "fun test() { x + 1 == y * 2 }" in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             params = [];
             body =
               BinOp
                 ( BinOp (Var "x", "+", Literal (LInt 1)),
                   "==",
                   BinOp (Var "y", "*", Literal (LInt 2)) );
             position = _;
             guard = _;
           };
         ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected correct precedence: arithmetic before comparison"

let tests =
  [
    ("simple function", `Quick, test_simple_function);
    ("function with params", `Quick, test_function_with_params);
    ("multiple functions", `Quick, test_multiple_functions);
    ("public function", `Quick, test_public_function);
    ("comparison operators", `Quick, test_comparison_operators);
    ("if with comparison", `Quick, test_if_with_comparison);
    ("complex comparison parsing", `Quick, test_complex_comparison_parsing);
    ("comparison precedence", `Quick, test_comparison_precedence);
  ]
