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

(* Test Tuple expressions *)
let test_tuple_expressions () =
  let test_cases =
    [
      ("fun num() { (42, \"hello\") }", [ "{42, \"hello\"}" ]);
      ("fun num() { (1, 2, 3) }", [ "{1, 2, 3}" ]);
      ("fun num() { () }", [ "{}" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("tuple expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test List expressions *)
let test_list_expressions () =
  let test_cases =
    [
      ("fun num() { [1, 2, 3] }", [ "[1, 2, 3]" ]);
      ("fun str() { [\"a\", \"b\"] }", [ "[\"a\", \"b\"]" ]);
      ("fun num() { [] }", [ "[]" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          check bool
            ("list expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test If expressions *)
let test_if_expressions () =
  let test_cases =
    [
      ( "fun boolean() { if true { 42 } else { 0 } }",
        [ "case true of true -> 42; _ -> 0" ] );
      ( "fun boolean() { if false { \"no\" } else { \"yes\" } }",
        [ "case false of true -> \"no\"; _ -> \"yes\"" ] );
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string program in
      List.iter
        (fun part ->
          let contains = string_contains_substring result part in
          Printf.printf "if expression: %s contains: %s\n" input part;
          check bool
            ("if expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test Case expressions with patterns *)
let test_case_expressions () =
  let test_cases =
    [
      ( "fun num() { case 42 { 42 -> \"found\" } }",
        [ "case 42 of 42 -> \"found\"" ] );
      ( "fun str(x) { case x { _ -> \"default\" } }",
        [ "case X_[a-z0-9]+ of _ -> \"default\"" ] );
      ( "fun atom(a) { case a { :hello -> \"hi\" } }",
        [ "case A_[a-z0-9]+ of hello -> \"hi\"" ] );
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string program in
      List.iter
        (fun part ->
          let contains =
            if String.contains part '[' then
              (* Use regex matching for patterns with hash *)
              try
                let _ = Str.search_forward (Str.regexp part) result 0 in
                true
              with Not_found -> false
            else string_contains_substring result part
          in
          check bool
            ("case expression: " ^ input ^ " contains: " ^ part)
            true contains)
        expected_parts)
    test_cases

(* Test Pattern types *)
let test_pattern_types () =
  let program =
    Compiler.parse_string
      "fun num() {\n\
      \  case x {\n\
      \    _ -> 1\n\
      \    y -> 2\n\
      \    :atom -> 3\n\
      \    }\n\
      \  }"
  in
  match program.items with
  | [ Function { clauses = [ { body = Match (_, cases); _ } ]; _ } ] -> (
      let patterns = List.map fst cases in
      match patterns with
      | [ PWildcard; PVar "y"; PAtom "atom" ] -> ()
      | _ -> Alcotest.fail "Expected wildcard, variable, and atom patterns")
  | _ -> Alcotest.fail "Expected function with case expression"

(* Test Spec definitions *)
let test_spec_definitions () =
  let program = Compiler.parse_string "spec my_function {}" in
  match program.items with
  | [ Spec { name = "my_function"; requires = []; ensures = [] } ] -> ()
  | _ -> Alcotest.fail "Expected spec definition"

(* Test Test blocks *)
let test_test_blocks () =
  let program =
    Compiler.parse_string
      "describe \"My Tests\" { test \"should work\" { true } }"
  in
  match program.items with
  | [
   Test
     {
       name = "My Tests";
       tests = [ { name = "should work"; body = Literal (LBool true) } ];
     };
  ] ->
      ()
  | _ -> Alcotest.fail "Expected test block with one test"

(* Test Mixed module items *)
let test_mixed_module_items () =
  let input =
    "\n\
    \    fun hello() { \"world\" }\n\
    \    spec hello {}\n\
    \    describe \"Tests\" { test \"hello test\" { hello() } }\n\
    \  "
  in
  let program = Compiler.parse_string input in
  match program.items with
  | [ Function _; Spec _; Test _ ] -> ()
  | _ -> Alcotest.fail "Expected function, spec, and test block"

(* Test Literal types coverage *)
let test_all_literal_types () =
  let test_cases =
    [
      ("fun num() { 42 }", LInt 42);
      ("fun num() { 3.14 }", LFloat 3.14);
      ("fun str() { \"hello\" }", LString "hello");
      ("fun bool() { true }", LBool true);
      ("fun bool() { false }", LBool false);
      ("fun atom() { :atom }", LAtom "atom");
    ]
  in

  List.iter
    (fun (input, expected_literal) ->
      let program = Compiler.parse_string input in
      match program.items with
      | [ Function { clauses = [ { body = Literal l; _ } ]; _ } ] ->
          check bool ("literal type: " ^ input) true (l = expected_literal)
      | _ -> Alcotest.fail ("Expected function with literal: " ^ input))
    test_cases

let tests =
  [
    ("tuple expressions", `Quick, test_tuple_expressions);
    ("list expressions", `Quick, test_list_expressions);
    ("if expressions", `Quick, test_if_expressions);
    ("case expressions", `Quick, test_case_expressions);
    ("pattern types", `Quick, test_pattern_types);
    ("spec definitions", `Quick, test_spec_definitions);
    ("test blocks", `Quick, test_test_blocks);
    ("mixed module items", `Quick, test_mixed_module_items);
    ("all literal types", `Quick, test_all_literal_types);
  ]
