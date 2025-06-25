open Alcotest
open Compiler.Error

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

let test_reserved_word_error () =
  (* Test that 'test' is now correctly tokenized as IDENT token (not reserved) *)
  let lexbuf = Lexing.from_string "test" in
  Compiler.Lexer.set_filename (Some "test.lx");
  let token = Compiler.Lexer.read lexbuf in
  (* Should return IDENT token, not TEST *)
  check bool "Should tokenize 'test' as IDENT token" true
    (token = Compiler.Parser.IDENT "test")

let test_unterminated_string_error () =
  (* Test unterminated string detection *)
  let test_fn () =
    let lexbuf = Lexing.from_string "\"unterminated string" in
    Compiler.Lexer.set_filename (Some "test.lx");
    ignore (Compiler.Lexer.read lexbuf);
    ignore (Compiler.Lexer.read lexbuf)
    (* EOF will trigger the error *)
  in
  try
    test_fn ();
    fail "Should have raised error"
  with CompilationError _ -> () (* Expected *)

let test_unexpected_character_error () =
  (* Test unexpected character detection *)
  let test_fn () =
    let lexbuf = Lexing.from_string "@" in
    Compiler.Lexer.set_filename (Some "test.lx");
    ignore (Compiler.Lexer.read lexbuf)
  in
  try
    test_fn ();
    fail "Should have raised error"
  with CompilationError _ -> () (* Expected *)

let test_parse_error_with_position () =
  (* Test that parse errors include position information *)
  try
    ignore
      (Compiler.parse_string ~filename:(Some "test.lx")
         "pub fun test() do if true end");
    fail "Expected parse error"
  with
  | CompilationError error ->
      check int "Should report line 1" 1 error.position.line;
      check (option string) "Should include filename" (Some "test.lx")
        error.position.filename
  | _ -> fail "Expected CompilationError"

let test_correct_test_usage () =
  (* Test that 'test' can be used correctly in test blocks *)
  let code =
    {|
    pub fun hello() do "world" end
    describe "Tests" do
      test "hello test" do
        hello()
      end
    end
    test "standalone test" do
      42
    end
  |}
  in
  let program = Compiler.parse_string code in
  check int "Should parse 3 items" 3 (List.length program.items)

let test_function_call_not_capitalized () =
  let source = "pub fun test(x, _y) do x end" in
  let program = Compiler.parse_string source in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "test(X_"; ", _) ->" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_variables_still_capitalized () =
  (* Test that variables are still properly capitalized *)
  let code =
    {|
    pub fun test_vars(param) do
      x = param
      x
    end
  |}
  in
  let program = Compiler.parse_string code in
  let erlang_code = Compiler.compile_to_string_for_tests program in
  check bool "Parameters should be capitalized" true
    (String.contains erlang_code 'P');
  check bool "Variables should be capitalized" true
    (String.contains erlang_code 'X')

let tests =
  [
    test_case "reserved_word_error" `Quick test_reserved_word_error;
    test_case "unterminated_string_error" `Quick test_unterminated_string_error;
    test_case "unexpected_character_error" `Quick
      test_unexpected_character_error;
    test_case "parse_error_with_position" `Quick test_parse_error_with_position;
    test_case "correct_test_usage" `Quick test_correct_test_usage;
    test_case "function_call_not_capitalized" `Quick
      test_function_call_not_capitalized;
    test_case "variables_still_capitalized" `Quick
      test_variables_still_capitalized;
  ]
