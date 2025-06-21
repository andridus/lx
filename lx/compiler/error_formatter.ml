open Error

(* Enhanced error formatter that improves OCaml's built-in error messages *)

(* Function to extract record information from AST *)
let get_record_types_from_ast ast =
  let rec extract_records acc = function
    | [] -> acc
    | item :: rest -> (
        match item with
        | Ast.Function func ->
            (* Extract record types from function definitions *)
            let func_records = extract_from_function func in
            extract_records (func_records @ acc) rest
        | _ -> extract_records acc rest)
  and extract_from_function _func =
    (* This would extract record type information from function bodies *)
    (* For now, return known record types *)
    [
      ("function_def", [ "name"; "clauses" ]);
      ("function_clause", [ "params"; "body" ]);
      ("otp_component", [ "name"; "handlers"; "functions"; "specs" ]);
    ]
  in
  extract_records [] ast.Ast.items

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

let rec enhance_error_line ast_context line =
  if string_contains_substring line "belongs to the type" then
    enhance_record_field_error line ast_context
  else if string_contains_substring line "Unbound" then
    enhance_unbound_error line ast_context
  else if
    string_contains_substring line "type"
    && string_contains_substring line "expected"
  then enhance_type_mismatch_error line ast_context
  else line

and enhance_record_field_error line ast_context =
  (* Parse the error message to extract field and type information *)
  let regex =
    Str.regexp
      "The record field \\([a-zA-Z_][a-zA-Z0-9_]*\\) belongs to the type \
       \\([a-zA-Z_][a-zA-Z0-9_]*\\)"
  in
  if Str.string_match regex line 0 then
    let field = Str.matched_group 1 line in
    let record_type = Str.matched_group 2 line in
    let available_fields = get_available_fields record_type ast_context in
    let similar_fields = Error.find_similar_names field available_fields in

    let base_msg =
      Printf.sprintf "Record field '%s' does not exist in the current context."
        field
    in
    let type_hint =
      Printf.sprintf "This field belongs to type '%s'." record_type
    in
    let suggestion =
      match similar_fields with
      | [] ->
          Printf.sprintf "Available fields: %s"
            (String.concat ", " available_fields)
      | [ s ] -> Printf.sprintf "Did you mean '%s'?" s
      | suggestions ->
          Printf.sprintf "Did you mean one of: %s?"
            (String.concat ", " suggestions)
    in
    let fix_hint =
      "Make sure you're accessing the correct record type or add the missing \
       type annotation."
    in

    Printf.sprintf "%s\n  Hint: %s\n  Suggestion: %s\n  Fix: %s" base_msg
      type_hint suggestion fix_hint
  else line

and enhance_unbound_error line ast_context =
  let regex =
    Str.regexp "Unbound \\(value\\|variable\\) \\([a-zA-Z_][a-zA-Z0-9_]*\\)"
  in
  if Str.string_match regex line 0 then
    let var_type = Str.matched_group 1 line in
    let var_name = Str.matched_group 2 line in
    let available_vars = get_available_variables ast_context in
    let similar_vars = Error.find_similar_names var_name available_vars in

    let base_msg = Printf.sprintf "Unbound %s '%s'." var_type var_name in
    let suggestion =
      match similar_vars with
      | [] -> "Check if the variable is defined in the current scope."
      | [ s ] -> Printf.sprintf "Did you mean '%s'?" s
      | suggestions ->
          Printf.sprintf "Did you mean one of: %s?"
            (String.concat ", " suggestions)
    in

    Printf.sprintf "%s\n  Suggestion: %s" base_msg suggestion
  else line

and enhance_type_mismatch_error line _ast_context =
  (* Enhance type mismatch errors with context *)
  if string_contains_substring line "This expression has type" then
    let parts = String.split_on_char ':' line in
    match parts with
    | prefix :: rest ->
        let enhanced_prefix = prefix ^ ":" in
        let rest_content = String.concat ":" rest in
        let context_hint =
          "\n\
          \  Hint: Check if you're using the correct type or if a type \
           conversion is needed."
        in
        enhanced_prefix ^ rest_content ^ context_hint
    | _ -> line
  else line

and get_available_fields record_type _ast_context =
  (* Get available fields for a record type from AST context *)
  match record_type with
  | "function_def" -> [ "name"; "clauses" ]
  | "function_clause" -> [ "params"; "body" ]
  | "otp_component" -> [ "name"; "handlers"; "functions"; "specs" ]
  | "worker" -> [ "name"; "handlers"; "functions"; "specs" ]
  | "supervisor" -> [ "name"; "strategy"; "children" ]
  | "spec" -> [ "name"; "requires"; "ensures" ]
  | "test_def" -> [ "name"; "body" ]
  | "describe_block" -> [ "name"; "tests" ]
  | _ -> []

and get_available_variables _ast_context =
  (* Extract available variable names from AST context *)
  (* This is a simplified version - in practice, you'd analyze the AST more thoroughly *)
  [
    "x";
    "y";
    "z";
    "result";
    "value";
    "state";
    "msg";
    "args";
    "name";
    "body";
    "params";
  ]

(* Enhanced error message formatter *)
let format_compilation_error error_output ast_context =
  let lines = String.split_on_char '\n' error_output in
  let enhanced_lines = List.map (enhance_error_line ast_context) lines in
  String.concat "\n" enhanced_lines

(* Function to wrap compilation and enhance error messages *)
let compile_with_enhanced_errors compile_fn program =
  try compile_fn program with
  | Failure msg ->
      let enhanced_msg = enhance_ocaml_error msg in
      failwith enhanced_msg
  | exn ->
      let msg = Printexc.to_string exn in
      let enhanced_msg = enhance_ocaml_error msg in
      failwith enhanced_msg

(* Function to enhance error messages from external tools *)
let enhance_external_error error_output ast_context =
  let lines = String.split_on_char '\n' error_output in
  let enhanced_lines =
    List.map
      (fun line ->
        if string_contains_substring line "Error:" then
          enhance_error_line ast_context line
        else line)
      lines
  in
  String.concat "\n" enhanced_lines

(* Function to create contextual error messages *)
let create_contextual_error position error_type details =
  let base_error = Error.make_error_with_position error_type position details in
  match error_type with
  | Error.RecordFieldError (field, record_type, available) ->
      let suggestion =
        match Error.find_similar_names field available with
        | [] ->
            Some
              (Printf.sprintf "Available fields in '%s': %s" record_type
                 (String.concat ", " available))
        | [ s ] -> Some (Printf.sprintf "Did you mean '%s'?" s)
        | suggestions ->
            Some
              (Printf.sprintf "Did you mean one of: %s?"
                 (String.concat ", " suggestions))
      in
      { base_error with suggestion }
  | _ -> base_error

(* Function to format error with file context *)
let format_error_with_context error filename =
  let file_context =
    match filename with
    | Some f -> Printf.sprintf "In file '%s':\n" f
    | None -> ""
  in
  file_context ^ Error.string_of_error error
