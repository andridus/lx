{
open Parser
}

rule read = parse
  (* Whitespace *)
  | [' ' '\t' '\n'] { read lexbuf }

  (* Comments *)
  | "//" [^ '\n']* '\n' { read lexbuf }



  (* Operators and Punctuation *)
  | "="             { EQ }
  | "->"            { ARROW }
  | "|"             { PIPE }
  | "_"             { WILDCARD }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ","             { COMMA }
  | ";"             { SEMICOLON }
  | "::"            { CONS }

  (* Literals *)
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as id {
      (* Check if it's a reserved word first *)
      match id with
      | "let" -> LET | "in" -> IN | "fun" -> FUN | "case" -> CASE
      | "if" -> IF | "then" -> THEN | "else" -> ELSE | "for" -> FOR | "when" -> WHEN
      | "true" -> BOOL true | "false" -> BOOL false
      | "worker" -> WORKER | "supervisor" -> SUPERVISOR | "strategy" -> STRATEGY | "children" -> CHILDREN
      | "one_for_one" -> ONE_FOR_ONE | "one_for_all" -> ONE_FOR_ALL | "rest_for_one" -> REST_FOR_ONE
      | "init" -> INIT | "call" -> CALL | "cast" -> CAST | "info" -> INFO | "terminate" -> TERMINATE
      | "spec" -> SPEC | "requires" -> REQUIRES | "ensures" -> ENSURES | "matches" -> MATCHES
      | "describe" -> DESCRIBE | "test" -> TEST | "assert" -> ASSERT
      | _ -> IDENT id
    }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id { UPPER_IDENT id }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | ['0'-'9']+ '.' ['0'-'9']+ as f { FLOAT (float_of_string f) }
  | ':' ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as atom { ATOM (String.sub atom 1 (String.length atom - 1)) }
  | '"'             { read_string (Buffer.create 16) lexbuf }

  (* End of file *)
  | eof             { EOF }

  (* Unexpected character *)
  | _ as c          { failwith ("Unexpected character: " ^ String.make 1 c) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof       { failwith "Unterminated string" }
