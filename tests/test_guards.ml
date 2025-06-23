open Alcotest
open Compiler.Ast

let string_contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec search i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else search (i + 1)
  in
  search 0

(* Test simple guard parsing *)
let test_simple_guard_parsing () =
  let input = "fun test(x) when x > 0 { x }" in
  let program = Compiler.parse_string input in
  match program.items with
  | [ Function { clauses = [ { guard = Some _; _ } ]; _ } ] ->
      check bool "simple guard parsed" true true
  | _ -> Alcotest.fail "Expected function with guard"

(* Test guard compilation *)
let test_guard_compilation () =
  let input = "fun test(x) when x > 0 { x }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_when = string_contains_substring result " when " in
  let contains_greater = string_contains_substring result " > " in
  check bool "contains 'when'" true contains_when;
  check bool "contains '>'" true contains_greater

(* Test guard with type test *)
let test_guard_with_type_test () =
  let input = "fun test(x) when is_atom(x) { x }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_is_atom = string_contains_substring result "is_atom" in
  check bool "contains 'is_atom'" true contains_is_atom

(* Test guard with and operator *)
let test_guard_with_and () =
  let input = "fun test(x, y) when x > 0 and y < 10 { x + y }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_comma = string_contains_substring result ", " in
  check bool "contains comma (and in Erlang)" true contains_comma

(* Test guard with or operator *)
let test_guard_with_or () =
  let input = "fun test(x) when x > 0 or x < -10 { x }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_semicolon = string_contains_substring result "; " in
  check bool "contains semicolon (or in Erlang)" true contains_semicolon

(* Test guard with not operator *)
let test_guard_with_not () =
  let input = "fun test(x) when not is_atom(x) { x }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_not = string_contains_substring result "not " in
  check bool "contains 'not'" true contains_not

(* Test case guard *)
let test_case_guard () =
  let input =
    "fun test(x) { case x { n when n > 0 -> :positive _ -> :other } }"
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_when = string_contains_substring result " when " in
  check bool "case guard contains 'when'" true contains_when

(* Test operator conversion *)
let test_operator_conversion () =
  let input = "fun test(x) when x != 5 and x <= 10 { x }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_not_equal = string_contains_substring result "/=" in
  let contains_less_equal = string_contains_substring result "=<" in
  check bool "converts != to /=" true contains_not_equal;
  check bool "converts <= to =<" true contains_less_equal

let test_guard_andalso () =
  let input = "fun test(x, y) when x > 0 andalso y < 100 { :valid }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_andalso = string_contains_substring result "andalso" in
  check bool "guard with andalso" true contains_andalso

let test_guard_orelse () =
  let input = "fun test(x, y) when x > 100 orelse y < 0 { :valid }" in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_orelse = string_contains_substring result "orelse" in
  check bool "guard with orelse" true contains_orelse

let test_guard_mixed_operators () =
  let input =
    "fun test(x, y, z) when x > 0 and (y < 10 orelse z > 100) { :valid }"
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string program in
  let contains_and = string_contains_substring result "," in
  let contains_orelse = string_contains_substring result "orelse" in
  check bool "guard with mixed operators contains comma" true contains_and;
  check bool "guard with mixed operators contains orelse" true contains_orelse

let tests =
  [
    ("simple guard parsing", `Quick, test_simple_guard_parsing);
    ("guard compilation", `Quick, test_guard_compilation);
    ("guard with type test", `Quick, test_guard_with_type_test);
    ("guard with and", `Quick, test_guard_with_and);
    ("guard with or", `Quick, test_guard_with_or);
    ("guard with not", `Quick, test_guard_with_not);
    ("case guard", `Quick, test_case_guard);
    ("operator conversion", `Quick, test_operator_conversion);
    ("guard with andalso", `Quick, test_guard_andalso);
    ("guard with orelse", `Quick, test_guard_orelse);
    ("guard mixed operators", `Quick, test_guard_mixed_operators);
  ]
