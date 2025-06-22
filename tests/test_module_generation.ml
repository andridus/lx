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

(* Test module name extraction from filename *)
let test_module_name_generation () =
  let func = make_single_clause_function "example" [] (Literal (LInt 42)) in
  let program = { items = [ Function func ] } in
  let modules =
    Compiler.compile_to_string_with_module_name program "my_app" ()
  in

  match modules with
  | [ (module_name, module_content) ] ->
      check string "module name" "my_app" module_name;
      let contains_module =
        string_contains_substring module_content "-module(my_app)"
      in
      check bool "contains module declaration" true contains_module
  | _ -> fail "Expected exactly one module"

(* Test that multiple modules are generated for OTP components *)
let test_multiple_modules_generation () =
  (* This test would require OTP components to be properly implemented *)
  (* For now, we test that regular functions generate a single module *)
  let func1 = make_single_clause_function "start" [] (Literal (LAtom "ok")) in
  let func2 = make_single_clause_function "stop" [] (Literal (LAtom "ok")) in
  let program = { items = [ Function func1; Function func2 ] } in
  let modules =
    Compiler.compile_to_string_with_module_name program "test_app" ()
  in

  match modules with
  | [ (module_name, module_content) ] ->
      check string "module name" "test_app" module_name;
      let contains_start =
        string_contains_substring module_content "start() ->"
      in
      let contains_stop =
        string_contains_substring module_content "stop() ->"
      in
      check bool "contains start function" true contains_start;
      check bool "contains stop function" true contains_stop
  | _ -> fail "Expected exactly one module for regular functions"

(* Test empty module generation *)
let test_empty_module_generation () =
  let program = { items = [] } in
  let modules =
    Compiler.compile_to_string_with_module_name program "empty_app" ()
  in

  match modules with
  | [ (module_name, module_content) ] ->
      check string "module name" "empty_app" module_name;
      let contains_module =
        string_contains_substring module_content "-module(empty_app)"
      in
      let contains_export_all =
        string_contains_substring module_content "-compile(export_all)"
      in
      check bool "contains module declaration" true contains_module;
      check bool "should not contain export_all" false contains_export_all
  | _ -> fail "Expected exactly one module for empty program"

(* Test module content structure *)
let test_module_content_structure () =
  let func = make_single_clause_function "hello" [ "name" ] (Var "name") in
  let program = { items = [ Function func ] } in
  let modules =
    Compiler.compile_to_string_with_module_name program "greeting" ()
  in

  match modules with
  | [ (_, module_content) ] ->
      let expected_parts =
        [ "-module(greeting)"; "hello(Name_[a-z0-9]+) ->"; "Name_[a-z0-9]+\\." ]
      in
      let unexpected_parts = [ "-compile(export_all)" ] in
      List.iter
        (fun part ->
          let contains =
            if String.contains part '[' then
              string_matches_pattern module_content part
            else string_contains_substring module_content part
          in
          check bool ("contains: " ^ part) true contains)
        expected_parts;
      List.iter
        (fun part ->
          let contains = string_contains_substring module_content part in
          check bool ("should not contain: " ^ part) false contains)
        unexpected_parts
  | _ -> fail "Expected exactly one module"

let tests =
  [
    ("module name generation", `Quick, test_module_name_generation);
    ("multiple modules generation", `Quick, test_multiple_modules_generation);
    ("empty module generation", `Quick, test_empty_module_generation);
    ("module content structure", `Quick, test_module_content_structure);
  ]
