open Alcotest
open Compiler.Error

let test_enhance_ocaml_error () =
  let original_error =
    "The record field params belongs to the type function_"
  in
  let enhanced = enhance_ocaml_error original_error in

  let contains_hint =
    let len_s = String.length enhanced in
    let len_sub = String.length "This field belongs to type" in
    let rec search i =
      if i > len_s - len_sub then false
      else if String.sub enhanced i len_sub = "This field belongs to type" then
        true
      else search (i + 1)
    in
    search 0
  in

  check bool "Enhanced error contains hint" true contains_hint;
  check bool "Enhanced error is longer than original" true
    (String.length enhanced > String.length original_error)

let test_record_field_error () =
  let error_kind =
    RecordFieldError ("params", "function_", [ "name"; "clauses" ])
  in
  let suggestion = make_suggestion error_kind in

  match suggestion with
  | Some s ->
      let contains_available =
        let len_s = String.length s in
        let len_sub = String.length "Available fields" in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = "Available fields" then true
          else search (i + 1)
        in
        search 0
      in
      check bool "Suggestion contains available fields" true contains_available
  | None -> check bool "Should have suggestion" false true

let test_unbound_variable_error () =
  let similar_vars = [ "param"; "parameters"; "args" ] in
  let error_kind = UnboundVariable ("params", similar_vars) in
  let suggestion = make_suggestion error_kind in

  match suggestion with
  | Some s ->
      let contains_did_you_mean =
        let len_s = String.length s in
        let len_sub = String.length "Did you mean" in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = "Did you mean" then true
          else search (i + 1)
        in
        search 0
      in
      check bool "Suggestion contains 'Did you mean'" true contains_did_you_mean
  | None -> check bool "Should have suggestion" false true

let test_type_mismatch_error () =
  let error_kind =
    TypeMismatch ("string", "integer", Some "function definition")
  in
  let suggestion = make_suggestion error_kind in

  match suggestion with
  | Some s ->
      let contains_make_sure =
        let len_s = String.length s in
        let len_sub = String.length "Make sure" in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = "Make sure" then true
          else search (i + 1)
        in
        search 0
      in
      check bool "Suggestion contains helpful advice" true contains_make_sure
  | None ->
      check bool "Should have suggestion for function definition context" false
        true

let test_arity_mismatch_error () =
  let error_kind = ArityMismatch ("test_function", 2, 1) in
  let suggestion = make_suggestion error_kind in

  match suggestion with
  | Some s ->
      let contains_add =
        let len_s = String.length s in
        let len_sub = String.length "Add" in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = "Add" then true
          else search (i + 1)
        in
        search 0
      in
      check bool "Suggestion contains 'Add' for missing arguments" true
        contains_add
  | None -> check bool "Should have suggestion" false true

let test_similar_names_finder () =
  let candidates = [ "name"; "clauses"; "params"; "body"; "handlers" ] in
  let similar = find_similar_names "param" candidates in

  check bool "Should find similar names" true (List.length similar > 0);
  check bool "Should include 'params'" true (List.mem "params" similar)

let tests =
  [
    ("enhance OCaml error message", `Quick, test_enhance_ocaml_error);
    ("record field error suggestion", `Quick, test_record_field_error);
    ("unbound variable error suggestion", `Quick, test_unbound_variable_error);
    ("type mismatch error suggestion", `Quick, test_type_mismatch_error);
    ("arity mismatch error suggestion", `Quick, test_arity_mismatch_error);
    ("similar names finder", `Quick, test_similar_names_finder);
  ]
