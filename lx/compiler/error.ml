type position = { line : int; column : int; filename : string option }

type error_kind =
  | SyntaxError of string
  | UnexpectedToken of string * string option (* found, expected *)
  | UnterminatedString
  | UnexpectedCharacter of char
  | ReservedWordError of string
  | ParseError of string

type compilation_error = {
  kind : error_kind;
  position : position;
  message : string;
}

exception CompilationError of compilation_error

let position_of_lexbuf ?(filename = None) lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol + 1; filename }

let string_of_position pos =
  let file_part = match pos.filename with Some f -> f ^ ":" | None -> "" in
  Printf.sprintf "%s%d:%d" file_part pos.line pos.column

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

let string_of_error err =
  Printf.sprintf "%s: %s\n%s"
    (string_of_position err.position)
    (string_of_error_kind err.kind)
    err.message

let make_error ?(filename = None) kind lexbuf message =
  let position = position_of_lexbuf ~filename lexbuf in
  { kind; position; message }

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
