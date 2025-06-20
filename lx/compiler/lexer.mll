{
open Parser
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }
  | "let"           { LET }
  | "in"            { IN }
  | "="             { EQ }
  | ['a'-'z']+ as id { IDENT id }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | '"'             { read_string (Buffer.create 16) lexbuf }
  | eof             { EOF }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof       { failwith "Unterminated string" }
