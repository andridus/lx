open Alcotest
open Compiler.Error

let test_supervisor_missing_brackets_multiple_children () =
  (* Test error message when supervisor children are specified without brackets *)
  let code =
    {|
supervisor cart_sup {
  strategy one_for_one
  children cart, inventory, payment
}
|}
  in
  try
    ignore (Compiler.parse_string ~filename:(Some "test.lx") code);
    fail "Expected parse error for missing brackets"
  with
  | CompilationError error ->
      check bool "Should contain 'Invalid supervisor children field'" true
        (Str.string_match
           (Str.regexp ".*Invalid supervisor children field.*")
           error.message 0);
      check bool "Should suggest brackets" true
        (match error.suggestion with
        | Some s -> Str.string_match (Str.regexp ".*children \\[.*\\].*") s 0
        | None -> false);
      check int "Should report correct line" 4 error.position.line
  | _ -> fail "Expected CompilationError"

let test_supervisor_missing_brackets_single_child () =
  (* Test error message when supervisor has single child without brackets *)
  let code =
    {|
supervisor my_sup {
  strategy one_for_all
  children worker1
}
|}
  in
  try
    ignore (Compiler.parse_string ~filename:(Some "test.lx") code);
    fail "Expected parse error for missing brackets"
  with
  | CompilationError error ->
      check bool "Should contain 'Invalid supervisor children field'" true
        (Str.string_match
           (Str.regexp ".*Invalid supervisor children field.*")
           error.message 0);
      check bool "Should mention brackets requirement" true
        (Str.string_match
           (Str.regexp ".*brackets are required.*")
           error.message 0);
      check int "Should report correct line" 5 error.position.line
  | _ -> fail "Expected CompilationError"

let test_supervisor_invalid_children_syntax () =
  (* Test error message when children field has completely invalid syntax *)
  let code =
    {|
supervisor test_sup {
  strategy rest_for_one
  children invalid_syntax_here
}
|}
  in
  try
    ignore (Compiler.parse_string ~filename:(Some "test.lx") code);
    fail "Expected parse error for invalid syntax"
  with
  | CompilationError error ->
      check bool "Should contain error about children field" true
        (Str.string_match (Str.regexp ".*children.*") error.message 0);
      check int "Should report correct line" 5 error.position.line
  | _ -> fail "Expected CompilationError"

let test_supervisor_correct_syntax_with_brackets () =
  (* Test that correct syntax with brackets parses successfully *)
  let code =
    {|
supervisor cart_sup {
  strategy one_for_one
  children [cart, inventory, payment]
}

worker cart {
  fun init(_) { .{:ok, []} }
}

worker inventory {
  fun init(_) { .{:ok, .{}} }
}

worker payment {
  fun init(_) { .{:ok, []} }
}
|}
  in
  try
    let program = Compiler.parse_string code in
    check int "Should parse 4 items (1 supervisor + 3 workers)" 4
      (List.length program.items)
  with
  | CompilationError error ->
      fail ("Correct syntax should not fail: " ^ error.message)
  | _ -> fail "Unexpected error type"

let test_supervisor_empty_children_with_brackets () =
  (* Test that empty children list with brackets works *)
  let code =
    {|
supervisor empty_sup {
  strategy one_for_one
  children []
}
|}
  in
  try
    let program = Compiler.parse_string code in
    check int "Should parse 1 item" 1 (List.length program.items)
  with
  | CompilationError error ->
      fail ("Empty children list should not fail: " ^ error.message)
  | _ -> fail "Unexpected error type"

let test_supervisor_error_message_contains_suggestion () =
  (* Test that error messages contain helpful suggestions *)
  let code =
    {|
supervisor my_sup {
  strategy one_for_one
  children worker1, worker2
}
|}
  in
  try
    ignore (Compiler.parse_string code);
    fail "Expected parse error"
  with
  | CompilationError error ->
      check bool "Should have suggestion field" true (error.suggestion <> None);
      check bool "Should have context field" true (error.context <> None);
      check bool "Should suggest correct syntax" true
        (match error.suggestion with
        | Some s -> Str.string_match (Str.regexp ".*children \\[worker1.*") s 0
        | None -> false)
  | _ -> fail "Expected CompilationError"

let test_supervisor_error_message_educational () =
  (* Test that error messages are educational about why brackets are needed *)
  let code =
    {|
supervisor cart_sup {
  strategy one_for_one
  children cart
}
|}
  in
  try
    ignore (Compiler.parse_string code);
    fail "Expected parse error"
  with
  | CompilationError error ->
      check bool "Should mention consistency with list syntax" true
        (match error.context with
        | Some c ->
            Str.string_match (Str.regexp ".*consistency with list syntax.*") c 0
        | None -> false);
      check bool "Should be educational" true (String.length error.message > 50)
      (* Should be a substantial message *)
  | _ -> fail "Expected CompilationError"

let tests =
  [
    test_case "supervisor_missing_brackets_multiple_children" `Quick
      test_supervisor_missing_brackets_multiple_children;
    test_case "supervisor_missing_brackets_single_child" `Quick
      test_supervisor_missing_brackets_single_child;
    test_case "supervisor_invalid_children_syntax" `Quick
      test_supervisor_invalid_children_syntax;
    test_case "supervisor_correct_syntax_with_brackets" `Quick
      test_supervisor_correct_syntax_with_brackets;
    test_case "supervisor_empty_children_with_brackets" `Quick
      test_supervisor_empty_children_with_brackets;
    test_case "supervisor_error_message_contains_suggestion" `Quick
      test_supervisor_error_message_contains_suggestion;
    test_case "supervisor_error_message_educational" `Quick
      test_supervisor_error_message_educational;
  ]
