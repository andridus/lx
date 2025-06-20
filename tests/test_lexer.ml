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

let tests = [
  ("tokenize integer", `Quick, test_tokenize_int);
  ("tokenize identifier", `Quick, test_tokenize_ident);
  ("tokenize let keyword", `Quick, test_tokenize_let);
  ("tokenize equals", `Quick, test_tokenize_eq);
  ("tokenize in keyword", `Quick, test_tokenize_in);
]