open Compiler.Error

let () =
  Printf.printf "=== Enhanced Error Messages Demo ===\n\n";

  (* Demo 1: Record Field Error *)
  Printf.printf "1. Record Field Error Enhancement:\n";
  Printf.printf "Original OCaml error:\n";
  Printf.printf
    "  \"The record field params belongs to the type function_\"\n\n";

  let enhanced1 =
    enhance_ocaml_error "The record field params belongs to the type function_"
  in
  Printf.printf "Enhanced error:\n";
  Printf.printf "  %s\n\n" enhanced1;

  (* Demo 2: Custom Record Field Error *)
  Printf.printf "2. Custom Record Field Error:\n";
  let error_kind =
    RecordFieldError ("params", "function_def", [ "name"; "clauses" ])
  in
  let suggestion = make_suggestion error_kind in
  Printf.printf
    "Error: Record field 'params' does not exist in type 'function_def'\n";
  (match suggestion with
  | Some s -> Printf.printf "Suggestion: %s\n" s
  | None -> Printf.printf "No suggestion available\n");
  Printf.printf "\n";

  (* Demo 3: Unbound Variable with Suggestions *)
  Printf.printf "3. Unbound Variable Error with Suggestions:\n";
  let similar_vars = [ "parameter"; "param_list"; "arguments" ] in
  let error_kind2 = UnboundVariable ("params", similar_vars) in
  let suggestion2 = make_suggestion error_kind2 in
  Printf.printf "Error: Unbound variable 'params'\n";
  (match suggestion2 with
  | Some s -> Printf.printf "Suggestion: %s\n" s
  | None -> Printf.printf "No suggestion available\n");
  Printf.printf "\n";

  (* Demo 4: Type Mismatch Error *)
  Printf.printf "4. Type Mismatch Error:\n";
  let error_kind3 =
    TypeMismatch ("string", "integer", Some "function definition")
  in
  let suggestion3 = make_suggestion error_kind3 in
  Printf.printf
    "Error: Type mismatch in function definition: expected 'string', but found \
     'integer'\n";
  (match suggestion3 with
  | Some s -> Printf.printf "Suggestion: %s\n" s
  | None -> Printf.printf "No suggestion available\n");
  Printf.printf "\n";

  (* Demo 5: Arity Mismatch Error *)
  Printf.printf "5. Arity Mismatch Error:\n";
  let error_kind4 = ArityMismatch ("test_function", 3, 1) in
  let suggestion4 = make_suggestion error_kind4 in
  Printf.printf
    "Error: Function 'test_function' expects 3 arguments, but 1 were provided\n";
  (match suggestion4 with
  | Some s -> Printf.printf "Suggestion: %s\n" s
  | None -> Printf.printf "No suggestion available\n");
  Printf.printf "\n";

  (* Demo 6: Similar Names Finder *)
  Printf.printf "6. Similar Names Finder:\n";
  let candidates =
    [ "name"; "clauses"; "params"; "body"; "handlers"; "functions" ]
  in
  let similar_to_param = find_similar_names "param" candidates in
  let similar_to_clause = find_similar_names "clause" candidates in
  Printf.printf "Similar to 'param': %s\n" (String.concat ", " similar_to_param);
  Printf.printf "Similar to 'clause': %s\n"
    (String.concat ", " similar_to_clause);
  Printf.printf "\n";

  Printf.printf "=== Demo Complete ===\n"
