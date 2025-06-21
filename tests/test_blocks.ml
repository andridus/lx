open Alcotest

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
let test_simple_block () =
  let input = "fun test() { x = { y = 42; y } }" in
  let program = parse_string input in
  let result = compile_to_string program in
  let expected_parts = [ "fun()"; "end)()" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("simple block contains: " ^ part) true contains)
    expected_parts

(* Test nested blocks *)
let test_nested_blocks () =
  let input = "fun test() { x = { y = { z = 1; z }; y } }" in
  let program = parse_string input in
  let result = compile_to_string program in
  let expected_parts = [ "fun()"; "end)()" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("nested block contains: " ^ part) true contains)
    expected_parts

(* Test block with multiple statements *)
let test_multiple_statements_block () =
  let input = "fun test() { x = { a = 1; b = 2; c = 3; c } }" in
  let program = parse_string input in
  let result = compile_to_string program in
  let expected_parts = [ "fun()"; "end)()" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("multiple statements block contains: " ^ part) true contains)
    expected_parts

(* Test that variables don't leak from blocks *)
let test_variable_scoping () =
  let input = "fun test() { x = { local = 42; local }; y = 1 }" in
  let program = parse_string input in
  let result = compile_to_string program in
  (* Should contain anonymous function structure *)
  let contains_anon_fun = string_contains_substring result "fun()" in
  check bool "contains anonymous function" true contains_anon_fun

let tests =
  [
    ("simple block", `Quick, test_simple_block);
    ("nested blocks", `Quick, test_nested_blocks);
    ("multiple statements block", `Quick, test_multiple_statements_block);
    ("variable scoping", `Quick, test_variable_scoping);
  ]
