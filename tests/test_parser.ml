open Compiler.Ast
open Alcotest

let test_parse_simple_function () =
  let program = Compiler.parse_string "fun num() { 42 }" in
  match program.items with
  | [
   Function
     { name = "num"; clauses = [ { params = []; body = Literal (LInt 42); position = _ } ]; position = _ };
  ] ->
      ()
  | _ -> fail "Expected simple function definition"

let test_parse_function_with_params () =
  let program = Compiler.parse_string "fun add(x, y) { x }" in
  match program.items with
  | [
   Function
           {
        name = "add";
        clauses = [ { params = [ PVar "x"; PVar "y" ]; body = Var "x"; position = _ } ];
        position = _;
      };
  ] ->
      ()
  | _ -> fail "Expected function with parameters"

let test_parse_multiple_functions () =
  let program = Compiler.parse_string "fun first() { 1 } fun second() { 2 }" in
  match program.items with
  | [
   Function
     { name = "first"; clauses = [ { params = []; body = Literal (LInt 1); position = _ } ]; position = _ };
   Function
     { name = "second"; clauses = [ { params = []; body = Literal (LInt 2); position = _ } ]; position = _ };
  ] ->
      ()
  | _ -> fail "Expected two function definitions"

let test_parse_empty_program () =
  let program = Compiler.parse_string "" in
  match program.items with [] -> () | _ -> fail "Expected empty program"

let test_parse_literals () =
  let test_cases =
    [
      ( "fun num() { 42 }",
        fun items ->
          match items with
          | [ Function { clauses = [ { body = Literal (LInt 42); _ } ]; _ } ] ->
              true
          | _ -> false );
      ( "fun str() { \"hello\" }",
        fun items ->
          match items with
          | [
           Function { clauses = [ { body = Literal (LString "hello"); _ } ]; _ };
          ] ->
              true
          | _ -> false );
      ( "fun bool() { true }",
        fun items ->
          match items with
          | [ Function { clauses = [ { body = Literal (LBool true); _ } ]; _ } ]
            ->
              true
          | _ -> false );
      ( "fun atom() { :atom }",
        fun items ->
          match items with
          | [
           Function { clauses = [ { body = Literal (LAtom "atom"); _ } ]; _ };
          ] ->
              true
          | _ -> false );
    ]
  in

  List.iter
    (fun (input, check_fn) ->
      let program = Compiler.parse_string input in
      if not (check_fn program.items) then fail ("Failed to parse: " ^ input))
    test_cases

(* Test for nil literal *)
let test_parse_nil () =
  let program = Compiler.parse_string "fun test_nil() { nil }" in
  match program.items with
  | [ Function { clauses = [ { body = Literal LNil; _ } ]; _ } ] -> ()
  | _ -> fail "Expected nil literal"

(* Test for empty function body *)
let test_parse_empty_function () =
  let program = Compiler.parse_string "fun empty() { }" in
  match program.items with
  | [ Function { clauses = [ { body = Literal LNil; _ } ]; _ } ] -> ()
  | _ -> fail "Expected empty function to parse as nil"

(* Test for tuple parsing *)
let test_parse_tuples () =
  let test_cases =
    [
      ( "fun pair() { (:ok, 42) }",
        fun items ->
          match items with
          | [
           Function
             {
               clauses =
                 [
                   {
                     body = Tuple [ Literal (LAtom "ok"); Literal (LInt 42) ];
                     _;
                   };
                 ];
               _;
             };
          ] ->
              true
          | _ -> false );
      ( "fun triple() { (:ok, 42, \"test\") }",
        fun items ->
          match items with
          | [
           Function
             {
               clauses =
                 [
                   {
                     body =
                       Tuple
                         [
                           Literal (LAtom "ok");
                           Literal (LInt 42);
                           Literal (LString "test");
                         ];
                     _;
                   };
                 ];
               _;
             };
          ] ->
              true
          | _ -> false );
      ( "fun empty_tuple() { () }",
        fun items ->
          match items with
          | [ Function { clauses = [ { body = Tuple []; _ } ]; _ } ] -> true
          | _ -> false );
    ]
  in

  List.iter
    (fun (input, check_fn) ->
      let program = Compiler.parse_string input in
      if not (check_fn program.items) then
        fail ("Failed to parse tuple: " ^ input))
    test_cases

(* Test for if-then without else *)
let test_parse_if_then () =
  let program = Compiler.parse_string "fun check() { if true then 42 }" in
  match program.items with
  | [
   Function
     {
       clauses =
         [ { body = If (Literal (LBool true), Literal (LInt 42), None); _ } ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected if-then without else"

(* Test for if-then-else *)
let test_parse_if_then_else () =
  let program =
    Compiler.parse_string "fun check() { if true then 42 else 0 }"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [
           {
             body =
               If
                 ( Literal (LBool true),
                   Literal (LInt 42),
                   Some (Literal (LInt 0)) );
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected if-then-else"

let tests =
  [
    ("parse simple function", `Quick, test_parse_simple_function);
    ("parse function with parameters", `Quick, test_parse_function_with_params);
    ("parse multiple functions", `Quick, test_parse_multiple_functions);
    ("parse empty program", `Quick, test_parse_empty_program);
    ("parse various literals", `Quick, test_parse_literals);
    ("parse nil literal", `Quick, test_parse_nil);
    ("parse empty function", `Quick, test_parse_empty_function);
    ("parse tuples", `Quick, test_parse_tuples);
    ("parse if-then", `Quick, test_parse_if_then);
    ("parse if-then-else", `Quick, test_parse_if_then_else);
  ]
