open Alcotest

let () =
  run "LX-Lang Compiler Tests" [
    ("Lexer Tests", Test_lexer.tests);
    ("AST Tests", Test_ast_types.tests);
    ("Parser Tests", Test_parser.tests);
    ("Compiler Tests", Test_compiler.tests);
    ("Example-based Tests", Test_examples.tests);
    ("OTP Validator Tests", Test_otp_validator.tests);
    ("Typechecker Tests", Test_typechecker.tests);
  ]