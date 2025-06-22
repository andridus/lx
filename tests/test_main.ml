open Alcotest

let () =
  run "LX-Lang Compiler Tests"
    [
      ("Lexer Tests", Test_lexer.tests);
      ("AST Tests", Test_ast_types.tests);
      ("Parser Tests", Test_parser.tests);
      ("Compiler Tests", Test_compiler.tests);
      ("Example-based Tests", Test_examples.tests);
      ("OTP Validator Tests", Test_otp_validator.tests);
      ("Typechecker Tests", Test_typechecker.tests);
      ("Module Generation Tests", Test_module_generation.tests);
      ("Error Handling & Function Calls Tests", Test_error_handling.tests);
      ("Function Calls Tests", Test_function_calls.tests);
      ("Enhanced Error Messages Tests", Test_error_messages.tests);
      ("Assignment Tests", Test_assignment.tests);
      ("Test Blocks Tests", Test_blocks.tests);
      ("Test Scoping Tests", Test_scoping.tests);
      ("Special Syntax Tests", Test_special_syntax.tests);
      ("Supervisor Error Tests", Test_supervisor_errors.tests);
      ("Build Cleanup Tests", Test_build_cleanup.tests);
    ]
