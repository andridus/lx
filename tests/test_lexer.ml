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

let test_tokenize_let () =
  let lexbuf = Lexing.from_string "let" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.LET -> ()
  | _ -> Alcotest.fail "Expected LET token"

let test_tokenize_eq () =
  let lexbuf = Lexing.from_string "=" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.EQ -> ()
  | _ -> Alcotest.fail "Expected EQ token"

let test_tokenize_in () =
  let lexbuf = Lexing.from_string "in" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.IN -> ()
  | _ -> Alcotest.fail "Expected IN token"

let test_comment_with_newline () =
  let lexbuf = Lexing.from_string "# This is a comment\nlet" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.LET -> ()
  | _ -> Alcotest.fail "Expected LET token after comment"

let test_comment_at_eof () =
  let lexbuf = Lexing.from_string "# This is a comment at end of file" in
  let token = Compiler.Lexer.read lexbuf in
  match token with
  | Compiler.Parser.EOF -> ()
  | _ -> Alcotest.fail "Expected EOF token after comment at end of file"

let test_comment_with_code () =
  let lexbuf = Lexing.from_string "let x = 42  # End-of-line comment\nin" in
  let token1 = Compiler.Lexer.read lexbuf in
  let token2 = Compiler.Lexer.read lexbuf in
  let token3 = Compiler.Lexer.read lexbuf in
  let token4 = Compiler.Lexer.read lexbuf in
  let token5 = Compiler.Lexer.read lexbuf in
  match (token1, token2, token3, token4, token5) with
  | ( Compiler.Parser.LET,
      Compiler.Parser.IDENT "x",
      Compiler.Parser.EQ,
      Compiler.Parser.INT 42,
      Compiler.Parser.IN ) ->
      ()
  | _ -> Alcotest.fail "Expected LET x = 42 IN tokens with comment ignored"

let tests =
  [
    ("tokenize integer", `Quick, test_tokenize_int);
    ("tokenize identifier", `Quick, test_tokenize_ident);
    ("tokenize let keyword", `Quick, test_tokenize_let);
    ("tokenize equals", `Quick, test_tokenize_eq);
    ("tokenize in keyword", `Quick, test_tokenize_in);
    ("comment with newline", `Quick, test_comment_with_newline);
    ("comment at end of file", `Quick, test_comment_at_eof);
    ("comment with code", `Quick, test_comment_with_code);
  ]
