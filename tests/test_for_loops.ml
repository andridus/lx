open Alcotest
open Compiler.Ast

(* Test simple for loop parsing *)
let test_simple_for_loop () =
  let result =
    Compiler.parse_string "def test() do for x in [1, 2, 3] do x * 2 end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses = [ { body = For (PVar "x", None, _, _, None); _ } ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected simple for loop to parse correctly"

(* Test for loop with guard *)
let test_for_loop_with_guard () =
  let result =
    Compiler.parse_string
      "def test() do for x in [1, 2, 3] when x > 1 do x end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses = [ { body = For (PVar "x", None, _, _, Some _); _ } ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected for loop with guard to parse correctly"

(* Test for loop with map pattern *)
let test_for_loop_map_pattern () =
  let result =
    Compiler.parse_string
      "def test() do for %{name: name} in users do name end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             body =
               For
                 ( PMap [ AtomKeyPattern ("name", PVar "name") ],
                   None,
                   _,
                   _,
                   None );
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected for loop with map pattern to parse correctly"

(* Test for loop with named variable *)
let test_for_loop_with_named_variable () =
  let result =
    Compiler.parse_string
      "def test() do for %{name: name1} = account in accounts do account end \
       end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             body =
               For
                 ( PMap [ AtomKeyPattern ("name", PVar "name1") ],
                   Some "account",
                   _,
                   _,
                   None );
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected for loop with named variable to parse correctly"

(* Test for loop with tuple pattern *)
let test_for_loop_tuple_pattern () =
  let result =
    Compiler.parse_string
      "def test() do for {key, value} in pairs do key end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             body = For (PTuple [ PVar "key"; PVar "value" ], None, _, _, None);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected for loop with tuple pattern to parse correctly"

(* Test for loop with list pattern *)
let test_for_loop_list_pattern () =
  let result =
    Compiler.parse_string
      "def test() do for [head | tail] in lists do head end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses =
         [
           {
             body = For (PCons (PVar "head", PVar "tail"), None, _, _, None);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected for loop with list pattern to parse correctly"

(* Test for loop compilation - just check that it compiles without errors *)
let test_for_loop_compilation () =
  let result =
    Compiler.parse_string
      "def test() do for x in [1, 2, 3] when x > 1 do x * 2 end end"
  in
  try
    let _compiled = Compiler.compile_to_string result in
    () (* Success if no exception *)
  with _ -> fail "Expected for loop to compile to Erlang without errors"

(* Test complex for loop with map pattern and guard *)
let test_complex_for_loop () =
  let result =
    Compiler.parse_string
      "def test() do for %{name: name, age: age} in users when age >= 18 do \
       name end end"
  in
  match result.items with
  | [
   Function
     {
       name = "test";
       clauses = [ { body = For (PMap _, None, _, _, Some _); _ } ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected complex for loop to parse correctly"

let tests =
  [
    test_case "simple_for_loop" `Quick test_simple_for_loop;
    test_case "for_loop_with_guard" `Quick test_for_loop_with_guard;
    test_case "for_loop_map_pattern" `Quick test_for_loop_map_pattern;
    test_case "for_loop_with_named_variable" `Quick
      test_for_loop_with_named_variable;
    test_case "for_loop_tuple_pattern" `Quick test_for_loop_tuple_pattern;
    test_case "for_loop_list_pattern" `Quick test_for_loop_list_pattern;
    test_case "for_loop_compilation" `Quick test_for_loop_compilation;
    test_case "complex_for_loop" `Quick test_complex_for_loop;
  ]
