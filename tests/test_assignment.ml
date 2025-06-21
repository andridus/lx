(* Helper function to check if string contains substring *)
let string_contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(* Helper function to check if string matches regex pattern *)
let string_matches_pattern s pattern =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

let test_simple_assignment () =
  let source = {|
fun test() {
  x = 42
  x
}
|} in
  let lexbuf = Lexing.from_string source in
  let ast = Compiler.Parser.main Compiler.Lexer.read lexbuf in
  let modules =
    Compiler.compile_to_string_with_module_name ast "test_assignment" ()
  in
  let erlang_code =
    match modules with
    | [ (_, code) ] -> code
    | _ -> Alcotest.fail "Expected exactly one module"
  in
  (* Accept X_hash = 42 pattern *)
  let expected_pattern = "X_[a-z0-9]+ = 42" in
  if string_matches_pattern erlang_code expected_pattern then ()
  else Alcotest.fail ("Expected assignment pattern not found in: " ^ erlang_code)

let test_multiple_assignments () =
  let source = {|
fun test() {
  x = 42
  y = "hello"
  z = true
  z
}
|} in
  let lexbuf = Lexing.from_string source in
  let ast = Compiler.Parser.main Compiler.Lexer.read lexbuf in
  let modules =
    Compiler.compile_to_string_with_module_name ast "test_multiple" ()
  in
  let erlang_code =
    match modules with
    | [ (_, code) ] -> code
    | _ -> Alcotest.fail "Expected exactly one module"
  in
  let patterns =
    [ "X_[a-z0-9]+ = 42"; "Y_[a-z0-9]+ = \"hello\""; "Z_[a-z0-9]+ = true" ]
  in
  List.iter
    (fun pattern ->
      if not (string_matches_pattern erlang_code pattern) then
        Alcotest.fail
          ("Expected pattern '" ^ pattern ^ "' not found in: " ^ erlang_code))
    patterns

let test_reassignment_error () =
  let source = {|
fun test() {
  x = 42
  x = 100
}
|} in
  let lexbuf = Lexing.from_string source in
  let ast = Compiler.Parser.main Compiler.Lexer.read lexbuf in
  try
    let _ =
      Compiler.compile_to_string_with_module_name ast "test_reassignment" ()
    in
    Alcotest.fail "Expected reassignment error but compilation succeeded"
  with
  | Compiler.Error.CompilationError err
    when string_contains_substring
           (Compiler.Error.string_of_error err)
           "already defined" ->
      ()
  | Failure msg when string_contains_substring msg "already defined" -> ()
  | e -> Alcotest.fail ("Unexpected error: " ^ Printexc.to_string e)

let tests =
  [
    ("simple assignment", `Quick, test_simple_assignment);
    ("multiple assignments", `Quick, test_multiple_assignments);
    ("reassignment error", `Quick, test_reassignment_error);
  ]
