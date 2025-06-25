open Alcotest
open Compiler.Ast

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

(* Helper function to parse Lx code *)
let parse_lx_code code =
  try Ok (Compiler.parse_string code) with e -> Error (Printexc.to_string e)

(* Helper function to compile Lx code to Erlang *)
let compile_lx_code code =
  match parse_lx_code code with
  | Ok program -> (
      try
        let erlang_code = Compiler.compile_to_string_for_tests program in
        Ok erlang_code
      with e -> Error (Printexc.to_string e))
  | Error e -> Error e

(* Test individual match rescue step parsing *)
let test_individual_match_rescue_parsing () =
  let code =
    {|
def test() do
  match .{:ok, 1} <- get_value() rescue :error end
  :continue
end
|}
  in
  match parse_lx_code code with
  | Ok program -> (
      match program.items with
      | [ Function func ] -> (
          match func.clauses with
          | [ clause ] -> (
              match clause.body with
              | Sequence
                  [
                    MatchRescueStep (pattern, value, rescue);
                    Literal (LAtom "continue");
                  ] ->
                  check string "Pattern should match"
                    "PTuple [PAtom \"ok\"; PLiteral (LInt 1)]"
                    (match pattern with
                    | PTuple [ PAtom "ok"; PLiteral (LInt 1) ] ->
                        "PTuple [PAtom \"ok\"; PLiteral (LInt 1)]"
                    | _ -> "wrong pattern");
                  check string "Value should match"
                    "App (Var \"get_value\", [])"
                    (match value with
                    | App (Var "get_value", []) -> "App (Var \"get_value\", [])"
                    | _ -> "wrong value");
                  check string "Rescue should match" "Literal (LAtom \"error\")"
                    (match rescue with
                    | Literal (LAtom "error") -> "Literal (LAtom \"error\")"
                    | _ -> "wrong rescue")
              | _ -> fail "Expected sequence with MatchRescueStep and continue")
          | _ -> fail "Expected single clause")
      | _ -> fail "Expected single function")
  | Error e -> fail ("Parse error: " ^ e)

(* Test block match rescue parsing *)
let test_block_match_rescue_parsing () =
  let code =
    {|
def test() do
  match .{:ok, 1} <- get_value() rescue :error1 end
  match .{:ok, 2} <- get_value2() rescue :error2 end
  :success
end
|}
  in
  match parse_lx_code code with
  | Ok program -> (
      match program.items with
      | [ Function func ] -> (
          match func.clauses with
          | [ clause ] -> (
              match clause.body with
              | Sequence
                  [
                    MatchRescueStep (pattern1, _value1, _rescue1);
                    MatchRescueStep (pattern2, _value2, _rescue2);
                    Literal (LAtom "success");
                  ] ->
                  check string "First pattern should match"
                    "PTuple [PAtom \"ok\"; PLiteral (LInt 1)]"
                    (match pattern1 with
                    | PTuple [ PAtom "ok"; PLiteral (LInt 1) ] ->
                        "PTuple [PAtom \"ok\"; PLiteral (LInt 1)]"
                    | _ -> "wrong pattern");
                  check string "Second pattern should match"
                    "PTuple [PAtom \"ok\"; PLiteral (LInt 2)]"
                    (match pattern2 with
                    | PTuple [ PAtom "ok"; PLiteral (LInt 2) ] ->
                        "PTuple [PAtom \"ok\"; PLiteral (LInt 2)]"
                    | _ -> "wrong pattern")
              | _ ->
                  fail "Expected sequence with two MatchRescueStep and success")
          | _ -> fail "Expected single clause")
      | _ -> fail "Expected single function")
  | Error e -> fail ("Parse error: " ^ e)

(* Test individual match rescue code generation *)
let test_individual_match_rescue_codegen () =
  let code =
    {|
def c() do .{:ok, 1} end
def test() do
  match .{:ok, 1} <- c() rescue 1 end
  :done
end
|}
  in
  match compile_lx_code code with
  | Ok erlang_code ->
      check bool "Should contain nested case structure" true
        (string_contains_substring erlang_code "case c() of");
      check bool "Should contain pattern match" true
        (string_contains_substring erlang_code "{ok, 1} ->");
      check bool "Should contain rescue clause" true
        (string_contains_substring erlang_code "_ ->");
      check bool "Should contain done atom" true
        (string_contains_substring erlang_code "done")
  | Error e -> fail ("Compile error: " ^ e)

(* Test block match rescue code generation *)
let test_block_match_rescue_codegen () =
  let code =
    {|
def c() do .{:ok, 1} end
def e() do .{:ok, 2} end
def test() do
  match .{:ok, 1} <- c() rescue 1 end
  match .{:ok, 2} <- e() rescue 2 end
  :success
end
|}
  in
  match compile_lx_code code with
  | Ok erlang_code ->
      check bool "Should contain nested case for c()" true
        (string_contains_substring erlang_code "case c() of");
      check bool "Should contain nested case for e()" true
        (string_contains_substring erlang_code "case e() of");
      check bool "Should contain success atom" true
        (string_contains_substring erlang_code "success");
      check bool "Should contain rescue values" true
        (string_contains_substring erlang_code "_ ->"
        && string_contains_substring erlang_code "1"
        && string_contains_substring erlang_code "2")
  | Error e -> fail ("Compile error: " ^ e)

(* Test complex match rescue with multiple statements *)
let test_complex_match_rescue_sequence () =
  let code =
    {|
def get_user() do .{:ok, "alice"} end
def get_perms() do .{:ok, [:read, :write]} end
def process() do
  match .{:ok, user} <- get_user() rescue :user_error end
  :log_user
  :validate_user
  match .{:ok, perms} <- get_perms() rescue :perm_error end
  :log_perms
  :final_result
end
|}
  in
  match compile_lx_code code with
  | Ok erlang_code ->
      check bool "Should contain first case for get_user()" true
        (string_contains_substring erlang_code "case get_user() of");
      check bool "Should contain second case for get_perms()" true
        (string_contains_substring erlang_code "case get_perms() of");
      check bool "Should contain log_user" true
        (string_contains_substring erlang_code "log_user");
      check bool "Should contain validate_user" true
        (string_contains_substring erlang_code "validate_user");
      check bool "Should contain log_perms" true
        (string_contains_substring erlang_code "log_perms");
      check bool "Should contain final_result" true
        (string_contains_substring erlang_code "final_result")
  | Error e -> fail ("Compile error: " ^ e)

(* Test match rescue with different pattern types *)
let test_match_rescue_patterns () =
  let code =
    {|
def test_patterns() do
  match [head | tail] <- get_list() rescue [] end
  match %{name: user_name} <- get_user() rescue %{name: "unknown"} end
  :done
end
|}
  in
  match parse_lx_code code with
  | Ok program -> (
      match program.items with
      | [ Function func ] -> (
          match func.clauses with
          | [ clause ] -> (
              match clause.body with
              | Sequence
                  [
                    MatchRescueStep (PCons (PVar "head", PVar "tail"), _, _);
                    MatchRescueStep
                      (PMap [ AtomKeyPattern ("name", PVar "user_name") ], _, _);
                    Literal (LAtom "done");
                  ] ->
                  check bool "Patterns parsed correctly" true true
              | _ -> fail "Expected sequence with different pattern types")
          | _ -> fail "Expected single clause")
      | _ -> fail "Expected single function")
  | Error e -> fail ("Parse error: " ^ e)

(* Test match rescue error handling *)
let test_match_rescue_error_cases () =
  (* Test missing rescue clause *)
  let invalid_code = {|
def test() do
  match .{:ok, 1} <- get_value()
end
|} in
  (match parse_lx_code invalid_code with
  | Ok _ -> fail "Should fail without rescue clause"
  | Error _ -> check bool "Correctly failed without rescue clause" true true);

  (* Test invalid pattern *)
  let invalid_pattern =
    {|
def test() do
  match 123 <- get_value() rescue :error end
end
|}
  in
  match parse_lx_code invalid_pattern with
  | Ok _ -> check bool "Parsed with simple literal pattern" true true
  | Error _ -> fail "Should parse literal patterns"

(* Test match rescue with guards (should not be supported in individual steps) *)
let test_match_rescue_no_guards () =
  let code =
    {|
def test() do
  match .{:ok, x} <- get_value() rescue :error end
  :done
end
|}
  in
  match parse_lx_code code with
  | Ok _ -> check bool "Individual match rescue parsed without guards" true true
  | Error e -> fail ("Unexpected parse error: " ^ e)

(* Test nested match rescue in functions *)
let test_nested_match_rescue () =
  let code =
    {|
def test_nested() do
  match .{:ok, _value1} <- erlang.system_time() rescue :error1 end
  match .{:ok, _value2} <- erlang.system_time() rescue :error2 end
  :success
end
|}
  in
  match compile_lx_code code with
  | Ok erlang_code ->
      check bool "Should contain first case" true
        (string_contains_substring erlang_code "case erlang:system_time() of");
      check bool "Should contain second case" true
        (string_contains_substring erlang_code "case erlang:system_time() of")
  | Error e -> fail ("Compile error: " ^ e)

(* Test match rescue return value handling *)
let test_match_rescue_return_values () =
  let code =
    {|
def test_returns() do
  x = match .{:ok, _value} <- erlang.system_time() rescue 0 end
  y = match .{:error, _} <- erlang.system_time() rescue 1 end
  x + y
end
|}
  in
  match compile_lx_code code with
  | Ok erlang_code ->
      check bool "Should handle assignment to match rescue" true
        (string_contains_substring erlang_code "X = case"
        && string_contains_substring erlang_code "Y = case")
  | Error e -> fail ("Compile error: " ^ e)

let tests =
  [
    test_case "individual match rescue parsing" `Quick
      test_individual_match_rescue_parsing;
    test_case "block match rescue parsing" `Quick
      test_block_match_rescue_parsing;
    test_case "individual match rescue codegen" `Quick
      test_individual_match_rescue_codegen;
    test_case "block match rescue codegen" `Quick
      test_block_match_rescue_codegen;
    test_case "complex match rescue sequence" `Quick
      test_complex_match_rescue_sequence;
    test_case "match rescue patterns" `Quick test_match_rescue_patterns;
    test_case "match rescue error cases" `Quick test_match_rescue_error_cases;
    test_case "match rescue no guards" `Quick test_match_rescue_no_guards;
    (* TODO: Fix these tests - they have type checking issues *)
    (* test_case "nested match rescue" `Quick test_nested_match_rescue; *)
    (* test_case "match rescue return values" `Quick test_match_rescue_return_values; *)
  ]
