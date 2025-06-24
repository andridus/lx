type position = { line : int; column : int; filename : string option }

type error_kind =
  | SyntaxError of string
  | UnexpectedToken of string * string option (* found, expected *)
  | UnterminatedString
  | UnexpectedCharacter of char
  | ReservedWordError of string
  | ParseError of string
  | TypeError of string * string option (* error message, suggestion *)
  | RecordFieldError of
      string * string * string list (* field, record_type, available_fields *)
  | UnboundVariable of string * string list (* variable, similar_names *)
  | TypeMismatch of
      string * string * string option (* expected, found, context *)
  | ArityMismatch of string * int * int (* function_name, expected, found *)
  | MissingField of
      string * string * string list (* field, record_type, required_fields *)
  | VariableRedefinition of
      string * position option (* variable name, first definition position *)
  | VariableShadowing of
      string * position option (* variable name, parent definition position *)
  | PatternMatchError of
      string (* Special case for clean pattern match errors *)

type compilation_error = {
  kind : error_kind;
  position : position;
  message : string;
  suggestion : string option;
  context : string option;
}

exception CompilationError of compilation_error

(* ANSI color codes *)
let yellow = "\027[33m"
let red = "\027[31m"
let blue = "\027[34m"
let green = "\027[32m"
let bold = "\027[1m"
let reset = "\027[0m"

let position_of_lexbuf ?(filename = None) lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol + 1; filename }

let string_of_position pos =
  let file_part = match pos.filename with Some f -> f ^ ":" | None -> "" in
  Printf.sprintf "%s%s%d:%d%s" yellow file_part pos.line pos.column reset

(* Helper function to find similar names using Levenshtein distance *)
let levenshtein_distance s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let matrix = Array.make_matrix (len1 + 1) (len2 + 1) 0 in

  for i = 0 to len1 do
    matrix.(i).(0) <- i
  done;
  for j = 0 to len2 do
    matrix.(0).(j) <- j
  done;

  for i = 1 to len1 do
    for j = 1 to len2 do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      matrix.(i).(j) <-
        min
          (min (matrix.(i - 1).(j) + 1) (* deletion *) (matrix.(i).(j - 1) + 1))
          (* insertion *)
          (matrix.(i - 1).(j - 1) + cost)
      (* substitution *)
    done
  done;
  matrix.(len1).(len2)

let find_similar_names target candidates =
  let scored_candidates =
    List.map (fun name -> (name, levenshtein_distance target name)) candidates
  in
  let sorted_candidates =
    List.sort (fun (_, d1) (_, d2) -> compare d1 d2) scored_candidates
  in
  let filtered_candidates =
    List.filter (fun (_, distance) -> distance <= 3) sorted_candidates
  in
  let rec take n lst =
    match (n, lst) with
    | 0, _ | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs
  in
  List.map fst (take 3 filtered_candidates)

let string_of_error_kind = function
  | SyntaxError msg -> "Syntax Error: " ^ msg
  | UnexpectedToken (found, expected) ->
      let exp_part =
        match expected with Some exp -> " (expected " ^ exp ^ ")" | None -> ""
      in
      "Unexpected token '" ^ found ^ "'" ^ exp_part
  | UnterminatedString -> "Unterminated string literal"
  | UnexpectedCharacter c -> "Unexpected character '" ^ String.make 1 c ^ "'"
  | ReservedWordError word ->
      "'" ^ word ^ "' is a reserved word and cannot be used as an identifier"
  | ParseError msg -> "Parse Error: " ^ msg
  | TypeError (msg, _) -> "Type Error: " ^ msg
  | RecordFieldError (field, record_type, _available) ->
      Printf.sprintf "Record field '%s' does not exist in type '%s'" field
        record_type
  | UnboundVariable (var, _) -> Printf.sprintf "Unbound variable '%s'" var
  | TypeMismatch (expected, found, context) ->
      let context_part =
        match context with Some ctx -> " in " ^ ctx | None -> ""
      in
      Printf.sprintf "Type mismatch%s: expected '%s', but found '%s'"
        context_part expected found
  | ArityMismatch (func, expected, found) ->
      Printf.sprintf "Function '%s' expects %d arguments, but %d were provided"
        func expected found
  | MissingField (field, record_type, _required) ->
      Printf.sprintf "Missing required field '%s' in record of type '%s'" field
        record_type
  | VariableRedefinition (var, first_pos) ->
      let first_pos_str =
        match first_pos with
        | Some pos ->
            Printf.sprintf " (first defined at line %d, column %d)" pos.line
              pos.column
        | None -> ""
      in
      let scope_context = " within the same scope" in
      Printf.sprintf
        "Variable %s%s%s%s is already defined%s and cannot be reassigned%s"
        yellow bold var reset scope_context first_pos_str
  | VariableShadowing (var, parent_pos) ->
      let parent_pos_str =
        match parent_pos with
        | Some pos ->
            Printf.sprintf " (defined in parent scope at line %d, column %d)"
              pos.line pos.column
        | None -> " (defined in parent scope)"
      in
      Printf.sprintf "Variable %s%s%s%s cannot shadow parent scope variable%s"
        yellow bold var reset parent_pos_str
  | PatternMatchError msg -> msg

let make_suggestion = function
  | RecordFieldError (field, record_type, available) -> (
      let similar = find_similar_names field available in
      match similar with
      | [] ->
          Some
            (Printf.sprintf "Available fields in '%s': %s" record_type
               (String.concat ", " available))
      | [ suggestion ] -> Some (Printf.sprintf "Did you mean '%s'?" suggestion)
      | suggestions ->
          Some
            (Printf.sprintf "Did you mean one of: %s?"
               (String.concat ", " suggestions)))
  | UnboundVariable (var, candidates) -> (
      let similar = find_similar_names var candidates in
      match similar with
      | [] -> None
      | [ suggestion ] -> Some (Printf.sprintf "Did you mean '%s'?" suggestion)
      | suggestions ->
          Some
            (Printf.sprintf "Did you mean one of: %s?"
               (String.concat ", " suggestions)))
  | TypeMismatch (expected, found, Some context)
    when context = "function definition" ->
      Some
        (Printf.sprintf
           "Make sure the function body returns a value of type '%s', or \
            change the expected return type to '%s'"
           expected found)
  | ArityMismatch (func, expected, found) when found < expected ->
      Some
        (Printf.sprintf "Add %d more argument%s to the call to '%s'"
           (expected - found)
           (if expected - found = 1 then "" else "s")
           func)
  | ArityMismatch (func, expected, found) when found > expected ->
      Some
        (Printf.sprintf "Remove %d argument%s from the call to '%s'"
           (found - expected)
           (if found - expected = 1 then "" else "s")
           func)
  | ReservedWordError word ->
      Some
        (Printf.sprintf
           "Try using a different name like '%s_func', 'my_%s', or '%s_val'"
           word word word)
  | VariableRedefinition (var, _) ->
      Some
        (Printf.sprintf
           "Use a different variable name like '%s_new', '%s_2', or \
            'updated_%s'"
           var var var)
  | VariableShadowing (var, _) ->
      Some
        (Printf.sprintf
           "Use a different variable name like '%s_local', 'inner_%s', or \
            '%s_block'"
           var var var)
  | PatternMatchError _msg -> None
  | _ -> None

let make_context = function
  | RecordFieldError (_, record_type, _) ->
      Some
        (Printf.sprintf "when accessing fields of record type '%s'" record_type)
  | TypeMismatch (_, _, context) -> context
  | _ -> None

let string_of_error err =
  let suggestion_part =
    match err.suggestion with Some s -> "\n  Suggestion: " ^ s | None -> ""
  in
  let context_part =
    match err.context with Some c -> "\n  Context: " ^ c | None -> ""
  in
  let message_part =
    match err.kind with
    | SyntaxError _ -> "" (* Don't repeat the message *)
    | PatternMatchError _ -> "" (* Don't repeat the message *)
    | _ -> "\n" ^ err.message
  in
  Printf.sprintf "%s: %s%s%s%s"
    (string_of_position err.position)
    (string_of_error_kind err.kind)
    message_part suggestion_part context_part

let make_error ?(filename = None) kind lexbuf message =
  let position = position_of_lexbuf ~filename lexbuf in
  let suggestion = make_suggestion kind in
  let context = make_context kind in
  { kind; position; message; suggestion; context }

let syntax_error ?(filename = None) lexbuf message =
  let err = make_error ~filename (SyntaxError message) lexbuf message in
  raise (CompilationError err)

let unexpected_token ?(filename = None) ?(expected = None) lexbuf found =
  let message =
    Printf.sprintf "Found '%s'%s" found
      (match expected with Some exp -> ", expected " ^ exp | None -> "")
  in
  let err =
    make_error ~filename (UnexpectedToken (found, expected)) lexbuf message
  in
  raise (CompilationError err)

let parse_error ?(filename = None) lexbuf message =
  let err = make_error ~filename (ParseError message) lexbuf message in
  raise (CompilationError err)

let unterminated_string ?(filename = None) lexbuf =
  let err =
    make_error ~filename UnterminatedString lexbuf
      "String literal is not properly terminated with a closing quote"
  in
  raise (CompilationError err)

let unexpected_character ?(filename = None) lexbuf c =
  let message =
    Printf.sprintf "Character '%c' is not valid in this context" c
  in
  let err = make_error ~filename (UnexpectedCharacter c) lexbuf message in
  raise (CompilationError err)

let reserved_word_error ?(filename = None) lexbuf word =
  let message =
    Printf.sprintf
      "'%s' is a reserved word. Try using a different name like '%s_func' or \
       'my_%s'"
      word word word
  in
  let err = make_error ~filename (ReservedWordError word) lexbuf message in
  raise (CompilationError err)

(* New error functions for better type error reporting *)
let type_error ?(filename = None) ?(suggestion = None) lexbuf message =
  let err =
    make_error ~filename (TypeError (message, suggestion)) lexbuf message
  in
  raise (CompilationError err)

let record_field_error ?(filename = None) lexbuf field record_type
    available_fields =
  let message =
    Printf.sprintf "Field '%s' does not exist in record type '%s'" field
      record_type
  in
  let err =
    make_error ~filename
      (RecordFieldError (field, record_type, available_fields))
      lexbuf message
  in
  raise (CompilationError err)

let unbound_variable_error ?(filename = None) lexbuf var similar_names =
  let message =
    Printf.sprintf "Variable '%s' is not defined in this scope" var
  in
  let err =
    make_error ~filename (UnboundVariable (var, similar_names)) lexbuf message
  in
  raise (CompilationError err)

let type_mismatch_error ?(filename = None) ?(context = None) lexbuf expected
    found =
  let message =
    Printf.sprintf "Expected type '%s', but found type '%s'" expected found
  in
  let err =
    make_error ~filename
      (TypeMismatch (expected, found, context))
      lexbuf message
  in
  raise (CompilationError err)

let arity_mismatch_error ?(filename = None) lexbuf func_name expected found =
  let message =
    Printf.sprintf "Function '%s' expects %d arguments, but %d were provided"
      func_name expected found
  in
  let err =
    make_error ~filename
      (ArityMismatch (func_name, expected, found))
      lexbuf message
  in
  raise (CompilationError err)

(* New error functions for variable scoping *)
let variable_redefinition_error ?(filename = None) ?(first_pos = None) lexbuf
    var_name =
  let message =
    Printf.sprintf
      "Variable '%s' is already defined in this scope and cannot be reassigned"
      var_name
  in
  let err =
    make_error ~filename
      (VariableRedefinition (var_name, first_pos))
      lexbuf message
  in
  raise (CompilationError err)

let variable_shadowing_error ?(filename = None) ?(parent_pos = None) lexbuf
    var_name =
  let message =
    Printf.sprintf
      "Variable '%s' is already defined in parent scope and cannot be shadowed"
      var_name
  in
  let err =
    make_error ~filename
      (VariableShadowing (var_name, parent_pos))
      lexbuf message
  in
  raise (CompilationError err)

(* Helper function to create position from line and column *)
let make_position ?(filename = None) line column = { line; column; filename }

(* Helper function to create error with position *)
let make_error_with_position ?(suggestion = None) ?(context = None) kind
    position message =
  { kind; position; message; suggestion; context }

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

(* Function to enhance OCaml's built-in error messages *)
let enhance_ocaml_error error_msg =
  if string_contains_substring error_msg "Enhanced:" then
    (* Parse enhanced error format: Enhanced:message|Suggestion:suggestion|Context:context *)
    let parts = String.split_on_char '|' error_msg in
    let parse_part part =
      match String.split_on_char ':' part with
      | key :: value_parts ->
          (String.trim key, String.concat ":" value_parts |> String.trim)
      | _ -> ("", part)
    in
    let parsed_parts = List.map parse_part parts in
    let get_value key =
      try Some (List.assoc key parsed_parts) with Not_found -> None
    in
    let message =
      match get_value "Enhanced" with Some m -> m | None -> error_msg
    in
    let suggestion = get_value "Suggestion" in
    let context = get_value "Context" in

    let suggestion_part =
      match suggestion with Some s -> "\n  Suggestion: " ^ s | None -> ""
    in
    let context_part =
      match context with Some c -> "\n  Context: " ^ c | None -> ""
    in

    Printf.sprintf "%s%s%s" message suggestion_part context_part
  else if string_contains_substring error_msg "belongs to the type" then
    let parts = String.split_on_char ' ' error_msg in
    match parts with
    | "The" :: "record" :: "field" :: field :: "belongs" :: "to" :: "the"
      :: "type" :: record_type :: _ ->
        let clean_field = String.trim field in
        let clean_type = String.trim record_type in
        Printf.sprintf
          "Record field '%s' does not exist in the current context.\n\
          \  Hint: This field belongs to type '%s'. Make sure you're accessing \
           the correct record type.\n\
          \  Suggestion: Check if you meant to use a different record or if \
           there's a type annotation missing."
          clean_field clean_type
    | _ -> error_msg
  else error_msg
