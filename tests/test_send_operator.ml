open Compiler.Ast

(* Helper function to parse programs *)
let parse_program_string input =
  try
    Compiler.parse_string input
  with
  | exn -> failwith ("Parse error in: " ^ input ^ " - " ^ (Printexc.to_string exn))

(* Helper function to compile programs *)
let compile_program program =
  Compiler.compile_to_string program

(* Test parsing of send expressions *)
let test_send_parsing () =
  let test_cases = [
    ("fun test(pid, message) { pid ! message }", "Basic send operation");
    ("fun test(pid) { pid ! :wake_up }", "Send to self with atom");
    ("fun test(worker_pid, data) { worker_pid ! (:request, data) }", "Send tuple message");
    ("fun test(a, b, c) { a ! b ! c }", "Chained send operations (right associative)");
    ("fun test(pid, message) { result = pid ! message }", "Send in assignment");
  ] in

  List.iter (fun (input, description) ->
    try
      let _ = parse_program_string input in
      Printf.printf "✓ %s: %s\n" description input
    with
    | exn ->
        Printf.printf "✗ %s: %s - Error: %s\n" description input (Printexc.to_string exn);
        failwith ("Failed to parse: " ^ description)
  ) test_cases

(* Test compilation of send expressions *)
let test_send_compilation () =
  let test_cases = [
    ("fun test(pid, message) { pid ! message }", "Basic send compilation");
    ("fun test(pid) { pid ! :wake_up }", "Send to self compilation");
  ] in

  List.iter (fun (input, description) ->
    try
      let program = parse_program_string input in
      let compiled = compile_program program in
      Printf.printf "✓ %s: %s -> %s\n" description input compiled;
      (* Check if the compiled output contains the send operator *)
      if String.contains compiled '!' then
        Printf.printf "  Contains send operator ✓\n"
      else
        failwith ("Compiled output should contain send operator: " ^ compiled)
    with
    | exn ->
        Printf.printf "✗ %s: Error: %s\n" description (Printexc.to_string exn);
        failwith ("Failed to compile: " ^ description)
  ) test_cases

(* Test send operator precedence *)
let test_send_precedence () =
  let test_cases = [
    ("fun test(a, b, c) { a + b ! c }", "Arithmetic before send");
    ("fun test(a, b, c) { a ! b + c }", "Send has appropriate precedence");
    ("fun test(a, b, c) { a ! b ! c }", "Right associative chaining");
  ] in

  List.iter (fun (input, description) ->
    try
      let _ = parse_program_string input in
      Printf.printf "✓ %s: %s\n" description input
    with
    | exn ->
        Printf.printf "✗ %s: %s - Error: %s\n" description input (Printexc.to_string exn);
        failwith ("Failed to parse precedence test: " ^ description)
  ) test_cases

(* Test send operator AST structure *)
let test_send_ast_structure () =
  let input = "fun test(pid, message) { pid ! message }" in
  try
    let program = parse_program_string input in
    match program.items with
    | [Function { clauses = [{ body = Send (Var "pid", Var "message"); _ }]; _ }] ->
        Printf.printf "✓ Send AST structure is correct\n"
    | [Function { clauses = [{ body = _; _ }]; _ }] ->
        Printf.printf "✗ Expected Send(Var \"pid\", Var \"message\"), got different AST structure\n";
        failwith "Send AST structure is incorrect"
    | _ ->
        Printf.printf "✗ Unexpected program structure\n";
        failwith "Program structure is incorrect"
  with
  | exn ->
      Printf.printf "✗ AST structure test failed: %s\n" (Printexc.to_string exn);
      failwith "Failed to parse AST structure test"

(* Test send operator in OTP context *)
let test_send_in_otp () =
  let input = "fun test_send(pid, msg) { pid ! msg }" in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    (* Check if the compiled output contains the send operator *)
    if String.contains compiled '!' then
      Printf.printf "✓ Send operator works in OTP context\n"
    else (
      Printf.printf "✗ Send operator failed in OTP context - no ! found in: %s\n" compiled;
      failwith "Send operator should work in OTP context"
    )
  with
  | exn ->
      Printf.printf "✗ OTP send test failed: %s\n" (Printexc.to_string exn);
      failwith "Failed to parse OTP send example"

(* Test send return value *)
let test_send_return_value () =
  let input = "
    fun send_and_return(pid, message) {
      result = pid ! message
      result
    }" in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    (* Check if the compiled output contains the send operator and assignment *)
    if String.contains compiled '!' && String.contains compiled '=' then
      Printf.printf "✓ Send operator returns message value\n"
    else (
      Printf.printf "✗ Send operator return value test failed - compiled: %s\n" compiled;
      failwith "Send operator should return the message"
    )
  with
  | exn ->
      Printf.printf "✗ Send return value test failed: %s\n" (Printexc.to_string exn);
      failwith "Failed to parse send return value example"

(* Test error cases *)
let test_send_errors () =
  let invalid_cases = [
    ("fun test() { ! message }", "Missing target should fail");
    ("fun test() { pid ! }", "Missing message should fail");
  ] in

  List.iter (fun (input, description) ->
    try
      let _ = parse_program_string input in
      Printf.printf "✗ %s: %s - Should have failed but didn't\n" description input;
      failwith ("Should have failed: " ^ description)
    with
    | _ ->
        Printf.printf "✓ %s: %s correctly failed\n" description input
  ) invalid_cases

(* Export tests for main test runner *)
let tests = [
  ("Send Operator Parsing", `Quick, fun () -> test_send_parsing ());
  ("Send Operator Compilation", `Quick, fun () -> test_send_compilation ());
  ("Send Operator Precedence", `Quick, fun () -> test_send_precedence ());
  ("Send AST Structure", `Quick, fun () -> test_send_ast_structure ());
  ("Send in OTP Context", `Quick, fun () -> test_send_in_otp ());
  ("Send Return Value", `Quick, fun () -> test_send_return_value ());
  ("Send Error Cases", `Quick, fun () -> test_send_errors ());
]