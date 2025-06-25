open Alcotest
open Compiler.Error

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

let test_correct_tuple_syntax_works () =
  (* Test that correct tuple syntax compiles successfully *)
  let result =
    Compiler.parse_string "def test() do {:result, 200, \"done\"} end"
  in
  match result.items with
  | [
   Compiler.Ast.Function
     {
       name = "test";
       clauses =
         [
           {
             params = [];
             body =
               Tuple
                 [
                   Literal (LAtom "result");
                   Literal (LInt 200);
                   Literal (LString "done");
                 ];
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      () (* Success - correct tuple parsed properly *)
  | _ -> fail "Expected correct tuple syntax to parse successfully"

let test_correct_block_syntax_works () =
  (* Test that correct block syntax still works *)
  let result = Compiler.parse_string "def test() do x = 42; x end" in
  match result.items with
  | [
   Compiler.Ast.Function
     {
       name = "test";
       clauses =
         [
           {
             params = [];
             body = Sequence [ Assign ("x", Literal (LInt 42), _); Var "x" ];
             position = _;
             guard = _;
           };
         ];
       visibility = Public;
       position = _;
     };
  ] ->
      () (* Success - correct block syntax works *)
  | _ -> fail "Expected correct block syntax to work"

let test_no_false_positives () =
  (* Test that legitimate block expressions don't trigger tuple detection *)
  let legitimate_blocks =
    [
      "def test() do if true do :ok else :error end";
      "def test() do case x do 1 -> :one _ -> :other end";
      "def test() do receive do :msg -> :received end";
      "def test() do for i in [1, 2, 3] do i * 2 end";
    ]
  in
  List.iter
    (fun code ->
      try
        ignore (Compiler.parse_string code);
        () (* Should parse successfully *)
      with
      | CompilationError error
        when string_contains_substring error.message "tuple" ->
          fail ("False positive tuple detection for: " ^ code)
      | _ -> () (* Other errors are fine, just not tuple detection *))
    legitimate_blocks

let tests =
  [
    test_case "correct_tuple_syntax_works" `Quick
      test_correct_tuple_syntax_works;
    test_case "correct_block_syntax_works" `Quick
      test_correct_block_syntax_works;
    test_case "no_false_positives" `Quick test_no_false_positives;
  ]
