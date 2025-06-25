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

let debug_print_program program =
  match program.items with
  | [ Function { clauses = [ { body; _ } ]; _ } ] -> (
      match body with
      | ExternalCall (m, f, args, _) ->
          Printf.printf "ExternalCall(%s, %s, %d args)\n" m f (List.length args)
      | App (Var s, args) ->
          Printf.printf "App(Var %s, %d args)\n" s (List.length args)
      | _ -> Printf.printf "Other AST node\n")
  | _ -> Printf.printf "Unexpected program structure\n"

let test_function_call_parsing () =
  let program =
    Compiler.parse_string "def validation() do io.format(\"hello\") end"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [
           {
             body =
               ExternalCall ("io", "format", [ Literal (LString "hello") ], _);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ ->
      debug_print_program program;
      fail "Expected function call parsing"

let test_external_function_call_parsing () =
  let program =
    Compiler.parse_string "def validation() do io.format(\"hello\") end"
  in
  match program.items with
  | [
   Function
     {
       clauses =
         [
           {
             body =
               ExternalCall ("io", "format", [ Literal (LString "hello") ], _);
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> fail "Expected external function call parsing"

let test_sequence_parsing () =
  let program =
    Compiler.parse_string
      "def validation() do io.format(\"hello\"); io.format(\"world\") end"
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
                   ExternalCall
                     ("io", "format", [ Literal (LString "hello") ], _);
                   ExternalCall
                     ("io", "format", [ Literal (LString "world") ], _);
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
    Compiler.parse_string "def validation() do io.format(\"hello\") end"
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
    def a do
      () do nil end
      (x) do x end
      (x, y) do x + y end
    end
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
    Compiler.parse_string "def validation() do io.format(\"hello\") end"
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
      "def validation() do io.format(\"hello\"); io.format(\"world\") end"
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
    Compiler.parse_string "def validation(x) do io.format(\"hello\", [x]) end"
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
    def b(x) do
      io.format("olá mundo");
      io.format("olá mundo com args", [x])
    end
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
  let program = Compiler.parse_string "def old_style(x, y) do x + y end" in
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
      deps = None;
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
      deps = None;
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
      deps = None;
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
      deps = None;
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
                          [ List [ Literal (LInt 1); Literal (LInt 2) ] ],
                          None );
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
