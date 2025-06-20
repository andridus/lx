(* Expose the AST module *)
module Ast = Ast

(* Expose the Lexer module *)
module Lexer = Lexer

(* Expose the Parser module *)
module Parser = Parser

(* Expose the Type Checker module *)
module Typechecker = Typechecker

(* Expose the OTP Validator module *)
module Otp_validator = Otp_validator

(* Expose the Error module *)
module Error = Error

(* Main compilation functions *)
val parse_string : ?filename:string option -> string -> Ast.program
val parse_file : string -> Ast.program
val compile_to_string : Ast.program -> string

val compile_to_string_with_module_name :
  Ast.program -> string -> (string * string) list

val compile_file : string -> unit

(* Type checking functions *)
val type_check_program : Ast.program -> Typechecker.type_env
val type_check_file : string -> Typechecker.type_env

(* Helper functions *)
val capitalize_var : string -> string
