{
open Parser

let filename_ref = ref None

let set_filename filename = filename_ref := filename

let get_filename () = !filename_ref
}

rule read = parse
  (* Whitespace *)
  | [' ' '\t'] { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }

  (* Comments *)
  | "#" [^ '\n']* '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "#" [^ '\n']* eof { EOF }

  (* Operators and Punctuation *)
  | "=="            { EQEQ }
  | "!="            { NEQ }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | "<"             { LT }
  | ">"             { GT }
  | "="             { EQ }
  | "->"            { ARROW }
  | "|"             { PIPE }
  | "_"             { WILDCARD }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | ".{"            { DOT_LBRACE }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ","             { COMMA }
  | ";"             { SEMICOLON }
  | "::"            { CONS }
  | ":"             { COLON }
  | "."             { DOT }
  | "++"            { CONCAT }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "!"             { SEND }

  (* Special tokens *)
  | "__MODULE__"    { MODULE_MACRO }

  (* Literals *)
  | ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']* as id {
      (* Check if it's a reserved word first *)
      match id with
      | "pub" -> PUB | "fun" -> FUN | "case" -> CASE
      | "if" -> IF | "else" -> ELSE | "for" -> FOR | "when" -> WHEN | "in" -> IN
      | "and" -> AND | "or" -> OR | "not" -> NOT
      | "andalso" -> ANDALSO | "orelse" -> ORELSE
      | "receive" -> RECEIVE | "after" -> AFTER
      | "true" -> BOOL true | "false" -> BOOL false | "nil" -> NIL
      | "worker" -> WORKER | "supervisor" -> SUPERVISOR | "strategy" -> STRATEGY | "children" -> CHILDREN
      | "one_for_one" -> ONE_FOR_ONE | "one_for_all" -> ONE_FOR_ALL | "rest_for_one" -> REST_FOR_ONE
      | "spec" -> SPEC | "requires" -> REQUIRES | "ensures" -> ENSURES | "matches" -> MATCHES
      | "assert" -> ASSERT
      | "application" -> APPLICATION | "description" -> DESCRIPTION | "vsn" -> VSN
      | "applications" -> APPLICATIONS | "registered" -> REGISTERED | "env" -> ENV
      | "record" -> RECORD
      | word when List.mem word ["spec"; "worker"; "supervisor"] ->
          (* Other commonly misused reserved words *)
          Error.reserved_word_error ~filename:(!filename_ref) lexbuf word
      | _ -> IDENT id
    }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id { UPPER_IDENT id }
  | ['0'-'9']+ as n {
      try INT (int_of_string n)
      with Failure _ -> Error.syntax_error ~filename:(!filename_ref) lexbuf ("Invalid integer: " ^ n)
    }
  | ['0'-'9']+ '.' ['0'-'9']+ as f {
      try FLOAT (float_of_string f)
      with Failure _ -> Error.syntax_error ~filename:(!filename_ref) lexbuf ("Invalid float: " ^ f)
    }
  | ':' ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as atom { ATOM (String.sub atom 1 (String.length atom - 1)) }
  | '"'             { read_string (Buffer.create 16) lexbuf }

  (* End of file *)
  | eof             { EOF }

  (* Unexpected character *)
  | _ as c          { Error.unexpected_character ~filename:(!filename_ref) lexbuf c }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | eof       { Error.unterminated_string ~filename:(!filename_ref) lexbuf }
