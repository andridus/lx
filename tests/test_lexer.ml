let test_tokenize_int () =
  let lexbuf = Lexing.from_string "42" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.INT 42 -> ()
  | _ -> Alcotest.fail "Expected INT 42"

let test_tokenize_ident () =
  let lexbuf = Lexing.from_string "hello" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.IDENT "hello" -> ()
  | _ -> Alcotest.fail "Expected IDENT hello"

let test_tokenize_eq () =
  let lexbuf = Lexing.from_string "=" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.EQ -> ()
  | _ -> Alcotest.fail "Expected EQ token"

let test_comment_with_newline () =
  let lexbuf = Lexing.from_string "# This is a comment\ndef" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.DEF -> ()
  | _ -> Alcotest.fail "Expected DEF token after comment"

let test_comment_at_eof () =
  let lexbuf = Lexing.from_string "# This is a comment at end of file" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.EOF -> ()
  | _ -> Alcotest.fail "Expected EOF token after comment at end of file"

let tests =
  [
    ("tokenize integer", `Quick, test_tokenize_int);
    ("tokenize identifier", `Quick, test_tokenize_ident);
    ("tokenize equals", `Quick, test_tokenize_eq);
    ("comment with newline", `Quick, test_comment_with_newline);
    ("comment at end of file", `Quick, test_comment_at_eof);
  ]
