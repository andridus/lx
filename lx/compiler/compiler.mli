(* Expose the AST module *)
module Ast = Ast

(* Expose the Lexer module *)
module Lexer = Lexer

(* Expose the Parser module *)
module Parser = Parser

(* Main compilation functions *)
val parse_string : string -> Ast.program
val compile_to_string : Ast.program -> string
val compile_file : string -> unit

(* Helper functions *)
val capitalize_var : string -> string