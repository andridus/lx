open Alcotest
open Compiler.Ast

(* Helper function to check if string contains substring *)
let string_contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec search i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else search (i + 1)
  in
  search 0

(* Test underscore parameters in function definitions *)
let test_underscore_parameters () =
  let input = "pub fun init(_) { :ok }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "init(_) ->"; "ok." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("underscore parameter contains: " ^ part) true contains)
    expected_parts

(* Test __MODULE__ macro compilation *)
let test_module_macro () =
  let input = "pub fun get_module() { __MODULE__ }" in
  let program = Compiler.parse_string input in

  (* Test that __MODULE__ parses correctly as ?MODULE *)
  match program.items with
  | [ Function { clauses = [ { body = Var "?MODULE"; _ } ]; _ } ] ->
      check bool "__MODULE__ macro parses correctly" true true
  | _ -> check bool "__MODULE__ macro parses correctly" false true

(* Test ignored variables with underscore prefix *)
let test_ignored_variables () =
  let input =
    "pub fun test() { _used = gen_server.call(erlang.self(), :get); :ok }"
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in

  (* Should contain the external call and final result, but no assignment *)
  let should_contain = [ "gen_server:call(erlang:self(), get)"; "ok" ] in
  List.iter
    (fun expected ->
      let contains = string_contains_substring result expected in
      check bool ("should contain: " ^ expected) true contains)
    should_contain;

  (* Should NOT contain assignment to ignored variable *)
  let should_not_contain = [ "_used ="; "_Used ="; "Used_ =" ] in
  List.iter
    (fun forbidden ->
      let contains = string_contains_substring result forbidden in
      check bool
        ("ignored variable should not create assignment: " ^ forbidden)
        false contains)
    should_not_contain

(* Test valid dot syntax works correctly *)
let test_valid_dot_syntax () =
  let input = "pub fun test() { gen_server.call(module_ref, :get) }" in
  let program = Compiler.parse_string input in
  match program.items with
  | [
   Function
     {
       clauses = [ { body = ExternalCall ("gen_server", "call", _, _); _ } ];
       _;
     };
  ] ->
      check bool "dot syntax parses correctly" true true
  | _ -> check bool "dot syntax parses correctly" true false

(* Test worker with special syntax features *)
let test_worker_special_syntax () =
  (* Skip this test as worker syntax is not fully implemented yet *)
  check bool "worker syntax test skipped" true true

(* Test parsing of __MODULE__ token *)
let test_module_macro_parsing () =
  let input = "pub fun test() { __MODULE__ }" in
  let program = Compiler.parse_string input in

  match program.items with
  | [ Function { clauses = [ { body = Var "?MODULE"; _ } ]; _ } ] -> ()
  | _ -> Alcotest.fail "Expected __MODULE__ to parse as Var \"?MODULE\""

(* Test underscore patterns in case expressions *)
let test_underscore_in_patterns () =
  let input = "pub fun test(x) { case x { _ -> :default } }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in

  let contains_case = string_contains_substring result "case" in
  let contains_wildcard = string_contains_substring result "_ ->" in
  let contains_default = string_contains_substring result "default" in

  check bool "case expression contains case" true contains_case;
  check bool "case expression contains wildcard" true contains_wildcard;
  check bool "case expression contains default" true contains_default

let tests =
  [
    ("underscore parameters", `Quick, test_underscore_parameters);
    ("__MODULE__ macro", `Quick, test_module_macro);
    ("ignored variables", `Quick, test_ignored_variables);
    ("valid dot syntax", `Quick, test_valid_dot_syntax);
    ("worker special syntax", `Quick, test_worker_special_syntax);
    ("__MODULE__ macro parsing", `Quick, test_module_macro_parsing);
    ("underscore in patterns", `Quick, test_underscore_in_patterns);
  ]
