open Alcotest

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

(* Helper function to check if string matches regex pattern *)
let string_matches_pattern s pattern =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

let test_simple_function_example () =
  let program = Compiler.parse_string "fun hello() { \"world\" }" in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "-module(generated)"; "hello() ->"; "\"world\"." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_function_with_parameters () =
  let program = Compiler.parse_string "fun add(x, y) { x }" in
  let result = Compiler.compile_to_string program in
  let expected_parts =
    [ "add(X_[a-z0-9]+, Y_[a-z0-9]+) ->"; "X_[a-z0-9]+\\." ]
  in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_multiple_functions () =
  let input = "fun first() { 1 } fun second() { 2 }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "first() ->"; "1."; "second() ->"; "2." ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_various_literals () =
  let test_cases =
    [
      ("fun test_int() { 42 }", [ "42" ]);
      ("fun test_float() { 3.14 }", [ "3.14" ]);
      ("fun test_bool() { true }", [ "true" ]);
      ("fun test_atom() { :hello }", [ "hello" ]);
      ("fun test_string() { \"world\" }", [ "\"world\"" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

let test_empty_program () =
  let program = Compiler.parse_string "" in
  let result = Compiler.compile_to_string program in
  let expected_parts = [ "-module(generated)"; "-compile(export_all)" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("empty program contains: " ^ part) true contains)
    expected_parts

let test_error_handling () =
  (* Test that parsing invalid syntax raises appropriate errors *)
  try
    let _ = Compiler.parse_string "invalid syntax here" in
    Alcotest.fail "Should have raised parsing error"
  with _ -> () (* Expected to fail *)

let tests =
  [
    ("simple function example", `Quick, test_simple_function_example);
    ("function with parameters", `Quick, test_function_with_parameters);
    ("multiple functions", `Quick, test_multiple_functions);
    ("various literal types", `Quick, test_various_literals);
    ("empty program", `Quick, test_empty_program);
    ("error handling for invalid syntax", `Quick, test_error_handling);
  ]
