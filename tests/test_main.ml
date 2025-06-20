open Alcotest

let () =
  run "LX-Lang Compiler Tests" [
    ("Lexer Tests", Test_lexer.tests);
    ("Parser Tests", Test_parser.tests);
    ("Compiler Tests", Test_compiler.tests);
    ("Example-based Tests", Test_examples.tests);
  ]