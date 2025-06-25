open Alcotest
open Compiler.Ast

let test_simple_function () =
  let result = Compiler.parse_string "pub fun num() do 42 end" in
  match result.items with
  | [
   Function
     {
       name = "num";
       clauses =
         [ { params = []; body = Literal (LInt 42); position = _; guard = _ } ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected single function with correct structure"

let test_function_with_params () =
  let result = Compiler.parse_string "pub fun add(x, y) do x end" in
  match result.items with
  | [
   Function
     {
       name = "add";
       clauses =
         [
           {
             params = [ PVar "x"; PVar "y" ];
             body = Var "x";
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected function with parameters"

let test_multiple_functions () =
  let result =
    Compiler.parse_string "pub fun first() do 1 end fun second() do 2 end"
  in
  match result.items with
  | [
   Function
     {
       name = "first";
       clauses =
         [ { params = []; body = Literal (LInt 1); position = _; guard = _ } ];
       visibility = Public;
       position = _;
     };
   Function
     {
       name = "second";
       clauses =
         [ { params = []; body = Literal (LInt 2); position = _; guard = _ } ];
       visibility = Private;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected two functions"

let test_public_function () =
  let result = Compiler.parse_string "pub fun hello() do \"world\" end" in
  match result.items with
  | [
   Function
     {
       name = "hello";
       clauses =
         [
           {
             params = [];
             body = Literal (LString "world");
             position = _;
             guard = _;
           };
         ];
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
      let code = "pub fun test() do " ^ expr_str ^ " end" in
      let result = Compiler.parse_string code in
      match result.items with
      | [
       Function
         {
           name = "test";
           clauses =
             [
               {
                 params = [];
                 body = BinOp (_, actual_op, _);
                 position = _;
                 guard = _;
               };
             ];
           visibility = Public;
           position = _;
         };
      ] ->
          check string ("operator " ^ op) op actual_op
      | _ -> fail ("Failed to parse comparison operator: " ^ op))
    test_cases

(* Test parsing of if condition with comparison *)
let test_if_with_comparison () =
  let result =
    Compiler.parse_string
      "pub fun test() do if x == 1 do :ok else :error end end"
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
                   Some (SimpleElse (Literal (LAtom "error"))) );
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected if-else with comparison condition"

(* Test parsing of complex comparison expressions *)
let test_complex_comparison_parsing () =
  let result = Compiler.parse_string "pub fun test(x, y) do x >= y end" in
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
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected function with comparison between parameters"

(* Test parsing precedence of comparison operators *)
let test_comparison_precedence () =
  let result = Compiler.parse_string "pub fun test() do x + 1 == y * 2 end" in
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
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected correct precedence: arithmetic before comparison"

(* Test parsing of with with else *)
let test_with_with_else () =
  let result =
    Compiler.parse_string
      "pub fun test() do with .{:ok, value} <= get_result() do value else \
       \"failed\" end end"
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
               With
                 ( steps,
                   Var "value",
                   Some (SimpleElse (Literal (LString "failed"))) );
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      check int "with steps count" 1 (List.length steps)
  | _ -> fail "Expected with-else expression"

(* Test parsing of with with case *)
let test_with_with_case () =
  let result =
    Compiler.parse_string
      {|
      pub fun test() do
        with .{:ok, value} <= get_result() do
          value
        case
          .{:error, _} -> "error"
          _ -> "unknown"
        end
      end
    |}
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
             body = With (steps, Var "value", Some (ClauseElse clauses));
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      check int "with steps count" 1 (List.length steps);
      check int "case clauses count" 2 (List.length clauses)
  | _ -> fail "Expected with-case expression"

(* Test parsing of multiple with steps *)
let test_with_multiple_steps () =
  let result =
    Compiler.parse_string
      "pub fun test() do with .{:ok, user} <= get_user(), .{:ok, role} <= \
       get_role(user) do .{user, role} else .{:error, \"failed\"} end end"
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
               With
                 (steps, Tuple [ Var "user"; Var "role" ], Some (SimpleElse _));
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      check int "with steps count" 2 (List.length steps)
  | _ -> fail "Expected with multiple steps"

(* Test parsing of with case with guards *)
let test_with_case_with_guards () =
  let result =
    Compiler.parse_string
      {|
      pub fun test() do
        with .{:ok, value} <= get_result() do
          value
        case
          .{:error, reason} when reason == :timeout -> "timeout"
          _ -> "other"
        end
      end
    |}
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
             body = With (_, Var "value", Some (ClauseElse clauses));
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] -> (
      check int "case clauses count" 2 (List.length clauses);
      (* Check that first clause has a guard *)
      match List.hd clauses with
      | _, Some _, _ -> ()
      | _ -> fail "Expected guard in first clause")
  | _ -> fail "Expected with-case with guards"

(* Test parsing of if without else *)
let test_if_without_else () =
  let result =
    Compiler.parse_string "pub fun test(x) do if x == 1 do \"one\" end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             params = [ PVar "x" ];
             body =
               If
                 ( BinOp (Var "x", "==", Literal (LInt 1)),
                   Literal (LString "one"),
                   None );
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected if without else"

(* Test parsing of with without else *)
let test_with_without_else () =
  let result =
    Compiler.parse_string
      "pub fun test() do with .{:ok, value} <= get_result() do value end end"
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
             body = With (steps, Var "value", None);
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      check int "with steps count" 1 (List.length steps)
  | _ -> fail "Expected with without else"

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
    ("with with else", `Quick, test_with_with_else);
    ("with with case", `Quick, test_with_with_case);
    ("with multiple steps", `Quick, test_with_multiple_steps);
    ("with case with guards", `Quick, test_with_case_with_guards);
    ("if without else", `Quick, test_if_without_else);
    ("with without else", `Quick, test_with_without_else);
  ]
