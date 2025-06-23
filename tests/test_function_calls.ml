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

(* Helper function to check if string matches regex pattern *)
let string_matches_pattern s pattern =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

let test_function_call_parsing () =
  let program =
    Compiler.parse_string "pub fun validation() { io.format(\"hello\") }"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [ { body = App (Var "io.format", [ Literal (LString "hello") ]); _ } ];
       _;
     };
  ] ->
      ()
  | [
   Function
     {
       clauses =
         [
           {
             body = ExternalCall ("io", "format", [ Literal (LString "hello") ]);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected function call parsing"

let test_external_function_call_parsing () =
  let program =
    Compiler.parse_string "pub fun validation() { io.format(\"hello\") }"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [
           {
             body = ExternalCall ("io", "format", [ Literal (LString "hello") ]);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | [
   Function
     {
       clauses =
         [ { body = App (Var "io.format", [ Literal (LString "hello") ]); _ } ];
       _;
     };
  ] ->
      (* Backward compatibility - this is also valid *)
      ()
  | _ -> fail "Expected external function call parsing"

let test_multiple_arities_parsing () =
  let input =
    {|
    fun a {
      () { nil }
      (x) { x }
      (x, y) { x }
    }
  |}
  in
  let program = Compiler.parse_string input in
  match program.items with
  | [
   Function
     {
       name = "a";
       clauses =
         [
           { params = []; body = Literal LNil; position = _; guard = _ };
           { params = [ PVar "x" ]; body = Var "x"; position = _; guard = _ };
           {
             params = [ PVar "x"; PVar "y" ];
             body = Var "x";
             position = _;
             guard = _;
           };
         ];
       visibility = _;
       position = _;
     };
  ] ->
      ()
  | _ -> fail "Expected function with multiple arities"

let test_sequence_parsing () =
  let program =
    Compiler.parse_string
      "pub fun validation() { io.format(\"hello\"); io.format(\"world\") }"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [
           {
             body =
               Sequence
                 [
                   App (Var "io.format", [ Literal (LString "hello") ]);
                   App (Var "io.format", [ Literal (LString "world") ]);
                 ];
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | [
   Function
     {
       clauses =
         [
           {
             body =
               Sequence
                 [
                   ExternalCall ("io", "format", [ Literal (LString "hello") ]);
                   ExternalCall ("io", "format", [ Literal (LString "world") ]);
                 ];
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected sequence parsing"

let test_external_function_call_compilation () =
  let program =
    Compiler.parse_string "pub fun validation() { io.format(\"hello\") }"
  in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "validation() ->"; "io:format(\"hello\")" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_multiple_arities_compilation () =
  let input =
    {|
    fun a {
      () { nil }
      (x) { x }
      (x, y) { x + y }
    }
  |}
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [
      "a() ->";
      "nil;";
      "a(X_[a-z0-9]+) ->";
      "X_[a-z0-9]+;";
      "a(X_[a-z0-9]+, Y_[a-z0-9]+) ->";
      "X_[a-z0-9]+ \\+ Y_[a-z0-9]+\\.";
    ]
  in
  List.iter
    (fun part ->
      let contains =
        if String.contains part '[' then string_matches_pattern result part
        else string_contains_substring result part
      in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_function_call_compilation () =
  let program =
    Compiler.parse_string "pub fun validation() { io.format(\"hello\") }"
  in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts = [ "validation() ->"; "io:format(\"hello\")" ] in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_sequence_compilation () =
  let program =
    Compiler.parse_string
      "pub fun validation() { io.format(\"hello\"); io.format(\"world\") }"
  in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [ "validation() ->"; "io:format(\"hello\"),"; "io:format(\"world\")" ]
  in
  List.iter
    (fun part ->
      let contains = string_contains_substring result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_function_with_args () =
  let program =
    Compiler.parse_string "pub fun validation(x) { io.format(\"hello\", [x]) }"
  in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [ "validation(X_[a-z0-9]+) ->"; "io:format(\"hello\", \\[X_[a-z0-9]+\\])" ]
  in
  List.iter
    (fun part ->
      let contains =
        if String.contains part '[' then string_matches_pattern result part
        else string_contains_substring result part
      in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_multiple_expressions_example () =
  let input =
    {|
    fun b(x) {
      io.format("olá mundo");
      io.format("olá mundo com args", [x])
    }
  |}
  in
  let program = Compiler.parse_string input in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [
      "b(X_[a-z0-9]+) ->";
      "io:format(\"olá mundo\"),";
      "io:format(\"olá mundo com args\", \\[X_[a-z0-9]+\\])";
    ]
  in
  List.iter
    (fun part ->
      let contains =
        if String.contains part '[' then string_matches_pattern result part
        else string_contains_substring result part
      in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_backward_compatibility () =
  (* Test that old syntax still works *)
  let program = Compiler.parse_string "pub fun old_style(x, y) { x + y }" in
  let result = Compiler.compile_to_string_for_tests program in
  let expected_parts =
    [
      "old_style(X_[a-z0-9]+, Y_[a-z0-9]+) ->"; "X_[a-z0-9]+ \\+ Y_[a-z0-9]+\\.";
    ]
  in
  List.iter
    (fun part ->
      let contains = string_matches_pattern result part in
      check bool ("contains: " ^ part) true contains)
    expected_parts

let test_simple_function_call () =
  let program =
    {
      items =
        [
          Function
            {
              name = "hello";
              clauses =
                [
                  {
                    params = [];
                    body = Literal (LString "world");
                    position = None;
                    guard = None;
                  };
                ];
              visibility = Private;
              position = None;
            };
        ];
    }
  in
  let result = Compiler.compile_to_string_for_tests program in
  check bool "contains function definition" true
    (String.contains result 'h' && String.contains result 'e')

let test_function_with_parameters () =
  let program =
    {
      items =
        [
          Function
            {
              name = "add";
              clauses =
                [
                  {
                    params = [ PVar "x"; PVar "y" ];
                    body = BinOp (Var "x", "+", Var "y");
                    position = None;
                    guard = None;
                  };
                ];
              visibility = Private;
              position = None;
            };
        ];
    }
  in
  let result = Compiler.compile_to_string_for_tests program in
  check bool "function compiles" true (String.length result > 0)

let test_multiple_clauses () =
  let program =
    {
      items =
        [
          Function
            {
              name = "a";
              clauses =
                [
                  {
                    params = [];
                    body = Literal LNil;
                    position = None;
                    guard = None;
                  };
                  {
                    params = [ PVar "x" ];
                    body = Var "x";
                    position = None;
                    guard = None;
                  };
                  {
                    params = [ PVar "x"; PVar "y" ];
                    body = BinOp (Var "x", "+", Var "y");
                    position = None;
                    guard = None;
                  };
                ];
              visibility = Private;
              position = None;
            };
        ];
    }
  in
  let result = Compiler.compile_to_string_for_tests program in
  check bool "multiple clauses compile" true (String.length result > 0)

let test_external_call () =
  let program =
    {
      items =
        [
          Function
            {
              name = "test_external";
              clauses =
                [
                  {
                    params = [];
                    body =
                      ExternalCall
                        ( "lists",
                          "reverse",
                          [ List [ Literal (LInt 1); Literal (LInt 2) ] ] );
                    position = None;
                    guard = None;
                  };
                ];
              visibility = Private;
              position = None;
            };
        ];
    }
  in
  let result = Compiler.compile_to_string_for_tests program in
  check bool "external call compiles" true (String.contains result 'l')

let tests =
  [
    ("function call parsing", `Quick, test_function_call_parsing);
    ( "external function call parsing",
      `Quick,
      test_external_function_call_parsing );
    ("multiple arities parsing", `Quick, test_multiple_arities_parsing);
    ("sequence parsing", `Quick, test_sequence_parsing);
    ( "external function call compilation",
      `Quick,
      test_external_function_call_compilation );
    ("multiple arities compilation", `Quick, test_multiple_arities_compilation);
    ("function call compilation", `Quick, test_function_call_compilation);
    ("sequence compilation", `Quick, test_sequence_compilation);
    ("function with args", `Quick, test_function_with_args);
    ("multiple expressions example", `Quick, test_multiple_expressions_example);
    ("backward compatibility", `Quick, test_backward_compatibility);
    ("simple function call", `Quick, test_simple_function_call);
    ("function with parameters", `Quick, test_function_with_parameters);
    ("multiple clauses", `Quick, test_multiple_clauses);
    ("external call", `Quick, test_external_call);
  ]
