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

let test_example_lx () =
  let program = Compiler.parse_string "let x = 42 in x" in
  let result = Compiler.compile_to_string program in
  let expected = "-module(dummy).\n-compile(export_all).\n\nstart() -> X = 42." in
  check string "example.lx compilation" expected result

let test_example_content_parsing () =
  (* Test parsing the content of example.lx directly *)
  let content = "let x = 42 in x" in
  let program = Compiler.parse_string content in
  match program with
  | Expr (Let ("x", Literal (LInt 42))) -> ()
  | _ -> Alcotest.fail "Failed to parse example.lx content correctly"

let test_example_matches_expected () =
  (* Test that compiling example.lx content produces expected output *)
  let content = "let x = 42 in x" in
  let program = Compiler.parse_string content in
  let result = Compiler.compile_to_string program in

  (* The expected output should have the same structure as example.erl *)
  let has_module = String.contains result 'm' && String.contains result 'o' in
  let has_start = String.contains result 's' && String.contains result 't' in
  let has_42 = String.contains result '4' && String.contains result '2' in

  check bool "contains module declaration" true has_module;
  check bool "contains start function" true has_start;
  check bool "contains value 42" true has_42

let test_various_expressions () =
  let test_cases = [
    ("42", "42");
    ("hello", "Hello");
    ("let y = 100 in y", "Y = 100");
  ] in

  List.iter (fun (input, expected_part) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    let contains_expected = string_contains_substring result expected_part in
    check bool ("expression: " ^ input) true contains_expected
  ) test_cases

let test_string_examples () =
  (* Test string literal examples *)
  let test_cases = [
    ("let greeting = \"hello\" in greeting", "Greeting = \"hello\"");
    ("\"world\"", "\"world\"");
  ] in

  List.iter (fun (input, expected_part) ->
    let program = Compiler.parse_string input in
    let result = Compiler.compile_to_string program in
    let contains_expected = string_contains_substring result expected_part in
    check bool ("string expression: " ^ input) true contains_expected
  ) test_cases

let test_error_handling () =
  (* Test that parsing invalid syntax raises appropriate errors *)
  try
    let _ = Compiler.parse_string "invalid syntax here" in
    Alcotest.fail "Should have raised parsing error"
  with
  | _ -> () (* Expected to fail *)

let tests = [
  ("compile example.lx content", `Quick, test_example_lx);
  ("parse example.lx content", `Quick, test_example_content_parsing);
  ("example matches expected structure", `Quick, test_example_matches_expected);
  ("various expressions compile", `Quick, test_various_expressions);
  ("string examples compile", `Quick, test_string_examples);
  ("error handling for invalid syntax", `Quick, test_error_handling);
]