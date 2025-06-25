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
      ("pub fun num() do (42, \"hello\") end", [ "{42, \"hello\"}" ]);
      ("pub fun num() do (1, 2, 3) end", [ "{1, 2, 3}" ]);
      ("pub fun num() do () end", [ "{}" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
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
      ("pub fun num() do [1, 2, 3] end", [ "[1, 2, 3]" ]);
      ("pub fun str() do [\"a\", \"b\"] end", [ "[\"a\", \"b\"]" ]);
      ("pub fun num() do [] end", [ "[]" ]);
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
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
      ( "pub fun boolean() do if true do 42 else 0 end end",
        [ "case true of true -> 42; _ -> 0" ] );
      ( "pub fun boolean() do if false do \"no\" else \"yes\" end end",
        [ "case false of true -> \"no\"; _ -> \"yes\"" ] );
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
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
      ( "pub fun num() do case 42 do 42 -> \"found\" end end",
        [ "case 42 of 42 -> \"found\"" ] );
      ( "pub fun str(x) do case x do _ -> \"default\" end end",
        [ "case X_[a-z0-9]+ of _ -> \"default\"" ] );
      ( "pub fun atom(a) do case a do :hello -> \"hi\" end end",
        [ "case A_[a-z0-9]+ of hello -> \"hi\"" ] );
    ]
  in

  List.iter
    (fun (input, expected_parts) ->
      let program = Compiler.parse_string input in
      let result = Compiler.compile_to_string_for_tests program in
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
      "pub fun num() do\n\
      \  case x do\n\
      \    _ -> 1\n\
      \    y -> 2\n\
      \    :atom -> 3\n\
      \  end\n\
      \ end"
  in
  match program.items with
  | [ Function { clauses = [ { body = Match (_, cases); _ } ]; _ } ] -> (
      let patterns = List.map (fun (p, _, _) -> p) cases in
      match patterns with
      | [ PWildcard; PVar "y"; PAtom "atom" ] -> ()
      | _ -> Alcotest.fail "Expected wildcard, variable, and atom patterns")
  | _ -> Alcotest.fail "Expected function with case expression"

(* Test Spec definitions *)
let test_spec_definitions () =
  let program = Compiler.parse_string "spec my_function do end" in
  match program.items with
  | [ Spec { name = "my_function"; requires = []; ensures = [] } ] -> ()
  | _ -> Alcotest.fail "Expected spec definition"

(* Test Test blocks *)
let test_test_blocks () =
  let program =
    Compiler.parse_string
      "describe \"My Tests\" do test \"should work\" do true end end"
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
    \    pub fun hello() do \"world\" end\n\
    \    spec hello do end\n\
    \    describe \"Tests\" do test \"hello test\" do hello() end end\n\
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
      ("pub fun num() do 42 end", LInt 42);
      ("pub fun num() do 3.14 end", LFloat 3.14);
      ("pub fun str() do \"hello\" end", LString "hello");
      ("pub fun bool() do true end", LBool true);
      ("pub fun bool() do false end", LBool false);
      ("pub fun atom() do :atom end", LAtom "atom");
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
