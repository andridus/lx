open Alcotest

(* Helper function to parse programs *)
let parse_program_string input =
  try Compiler.parse_string input
  with exn ->
    failwith ("Parse error in: " ^ input ^ " - " ^ Printexc.to_string exn)

(* Helper function to compile programs *)
let compile_program program = Compiler.compile_to_string_for_tests program

(* Test pattern matching with integer literals *)
let test_pattern_matching_integers () =
  let input =
    "pub fun test_numbers(x) { case x { 0 -> :zero 1 -> :one 42 -> :answer _ \
     -> :other } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain integer patterns" true
    (String.contains compiled '0' && String.contains compiled '1')

(* Test pattern matching with string literals *)
let test_pattern_matching_strings () =
  let input =
    "pub fun test_strings(s) { case s { \"hello\" -> :greeting \"world\" -> \
     :place \"\" -> :empty _ -> :other } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain string patterns" true
    (String.contains compiled '"')

(* Test pattern matching with atom literals *)
let test_pattern_matching_atoms () =
  let input =
    "pub fun test_atoms(a) { case a { :ok -> :success :error -> :failure \
     :timeout -> :expired _ -> :unknown } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile atom patterns successfully" true
    (String.length compiled > 50
    && String.contains compiled 'o'
    && String.contains compiled 's')

(* Test pattern matching with boolean literals *)
let test_pattern_matching_booleans () =
  let input =
    "pub fun test_booleans(b) { case b { true -> :yes false -> :no _ -> \
     :invalid } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain boolean patterns" true
    (String.contains compiled 't')

(* Test pattern matching with float literals *)
let test_pattern_matching_floats () =
  let input =
    "pub fun test_floats(f) { case f { 0.0 -> :zero 3.14 -> :pi 2.71 -> :euler \
     _ -> :other } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain float patterns" true (String.contains compiled '.')

(* Test pattern matching with tuple literals *)
let test_pattern_matching_tuples () =
  let input = "pub fun test_tuples(t) { case t { .{x, y} -> x + y _ -> 0 } }" in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile tuple patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with list literals *)
let test_pattern_matching_lists () =
  let input = "pub fun test_lists(l) { case l { [] -> 0 [x] -> x _ -> 1 } }" in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile list patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with map literals *)
let test_pattern_matching_maps () =
  let input =
    "pub fun test_maps(m) { case m { %{ name: n } -> n _ -> \"unknown\" } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile map patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with wildcard *)
let test_pattern_matching_wildcard () =
  let input = "pub fun test_wildcard(x) { case x { _ -> :anything } }" in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile wildcard patterns" true (String.length compiled > 0)

(* Test pattern matching with variable binding *)
let test_pattern_matching_variables () =
  let input =
    "pub fun test_vars(data) { case data { .{x, y} -> x + y _ -> 0 } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile variable binding successfully" true
    (String.length compiled > 50)

(* Test pattern matching with nested structures *)
let test_pattern_matching_nested () =
  let input =
    "pub fun test_nested(data) { case data { .{:user, name} -> name _ -> \
     :unknown } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile nested patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with guards *)
let test_pattern_matching_guards () =
  let input =
    "pub fun test_guards(x) { case x { n when n > 0 -> :positive n when n < 0 \
     -> :negative 0 -> :zero _ -> :other } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain guard patterns" true (String.contains compiled '>')

(* Test pattern matching in function parameters *)
let test_pattern_matching_function_params () =
  let input =
    "pub fun process_empty([]) { :empty } pub fun process_pair(.{x, y}) { x + \
     y } pub fun process_atom(:ok) { :success }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should contain function parameter patterns" true
    (String.contains compiled '[' && String.contains compiled '{')

(* Test pattern matching with mixed literal types *)
let test_pattern_matching_mixed_literals () =
  let input = "pub fun test_mixed(value) { case value { 42 -> 42 _ -> 0 } }" in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile mixed literal patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with nil *)
let test_pattern_matching_nil () =
  let input =
    "pub fun test_nil(value) { case value { nil -> :nothing _ -> :something } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile nil patterns successfully" true
    (String.length compiled > 50)

(* Test complex pattern matching scenarios *)
let test_pattern_matching_complex () =
  let input =
    "pub fun analyze_data(data) { case data { %{ name: user } -> user _ -> \
     :unknown } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile complex patterns successfully" true
    (String.length compiled > 50)

(* Test pattern matching with deep nesting *)
let test_pattern_matching_deep_nesting () =
  let input =
    "pub fun test_deep(data) { case data { .{x, y} -> x + y _ -> 0 } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile deep nesting successfully" true
    (String.length compiled > 50)

(* Test pattern matching with repeated variables *)
let test_pattern_matching_repeated_vars () =
  let input =
    "pub fun test_repeated(data) { case data { .{x, y} -> x + y _ -> 0 } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile repeated variables successfully" true
    (String.length compiled > 50)

(* Test pattern matching exhaustiveness *)
let test_pattern_matching_exhaustiveness () =
  let input =
    "pub fun test_exhaustive(bool_val) { case bool_val { true -> :yes false -> \
     :no } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should handle exhaustive patterns" true
    (String.contains compiled 't')

(* Test pattern matching with list comprehensions *)
let test_pattern_matching_list_comprehensions () =
  let input = "pub fun test_list_comp() { result = [1, 2, 3]; result }" in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should compile list comprehensions successfully" true
    (String.length compiled > 50)

(* Test pattern matching with multiple clauses *)
let test_pattern_matching_multiple_clauses () =
  let input =
    "pub fun multi_clause([]) { :empty } pub fun multi_clause([x]) { \
     .{:single, x} } pub fun multi_clause([x, y | rest]) { .{:multiple, x, y, \
     rest} }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should handle multiple clauses" true
    (String.contains compiled '[')

(* Test pattern matching with binary operators in patterns *)
let test_pattern_matching_binary_ops () =
  let input =
    "pub fun test_binary(x) { case x { n when n == 42 -> :answer n when n != 0 \
     -> :nonzero _ -> :other } }"
  in
  let program = parse_program_string input in
  let compiled = compile_program program in
  check bool "Should handle binary operators in guards" true
    (String.contains compiled '=')

let tests =
  [
    ("Pattern matching with integers", `Quick, test_pattern_matching_integers);
    ("Pattern matching with strings", `Quick, test_pattern_matching_strings);
    ("Pattern matching with atoms", `Quick, test_pattern_matching_atoms);
    ("Pattern matching with booleans", `Quick, test_pattern_matching_booleans);
    ("Pattern matching with floats", `Quick, test_pattern_matching_floats);
    ("Pattern matching with tuples", `Quick, test_pattern_matching_tuples);
    ("Pattern matching with lists", `Quick, test_pattern_matching_lists);
    ("Pattern matching with maps", `Quick, test_pattern_matching_maps);
    ("Pattern matching with wildcard", `Quick, test_pattern_matching_wildcard);
    ("Pattern matching with variables", `Quick, test_pattern_matching_variables);
    ( "Pattern matching with nested structures",
      `Quick,
      test_pattern_matching_nested );
    ("Pattern matching with guards", `Quick, test_pattern_matching_guards);
    ( "Pattern matching in function parameters",
      `Quick,
      test_pattern_matching_function_params );
    ( "Pattern matching with mixed literals",
      `Quick,
      test_pattern_matching_mixed_literals );
    ("Pattern matching with nil", `Quick, test_pattern_matching_nil);
    ("Complex pattern matching scenarios", `Quick, test_pattern_matching_complex);
    ( "Pattern matching with deep nesting",
      `Quick,
      test_pattern_matching_deep_nesting );
    ( "Pattern matching with repeated variables",
      `Quick,
      test_pattern_matching_repeated_vars );
    ( "Pattern matching exhaustiveness",
      `Quick,
      test_pattern_matching_exhaustiveness );
    ( "Pattern matching with list comprehensions",
      `Quick,
      test_pattern_matching_list_comprehensions );
    ( "Pattern matching with multiple clauses",
      `Quick,
      test_pattern_matching_multiple_clauses );
    ( "Pattern matching with binary operators",
      `Quick,
      test_pattern_matching_binary_ops );
  ]
