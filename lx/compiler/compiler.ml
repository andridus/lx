(* Module aliases for external access *)
module Ast = Ast
module Lexer = Lexer
module Parser = Parser

open Ast

let capitalize_var id =
  if String.length id > 0 then
    String.uppercase_ascii (String.sub id 0 1) ^ String.sub id 1 (String.length id - 1)
  else
    id

let rec emit_expr = function
  | Literal (LInt n) -> string_of_int n
  | Literal (LString s) -> "\"" ^ s ^ "\""
  | Var id -> capitalize_var id
  | Let (id, e1) -> capitalize_var id ^ " = " ^ emit_expr e1

let parse_string content =
  let lexbuf = Lexing.from_string content in
  Parser.main Lexer.read lexbuf

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let result = Parser.main Lexer.read lexbuf in
  close_in ic;
  result

let compile_to_string program =
  match program with
  | Expr expr ->
      let erl = emit_expr expr in
      "-module(dummy).\n-compile(export_all).\n\nstart() -> " ^ erl ^ "."

let compile_file filename =
  let program = parse_file filename in
  let output_content = compile_to_string program in
  let output_filename = Filename.remove_extension filename ^ ".erl" in
  let oc = open_out output_filename in
  output_string oc output_content;
  close_out oc;
  Printf.printf "Compiled %s -> %s\n" filename output_filename