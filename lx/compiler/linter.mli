(* Linter module interface for LX language *)

open Ast

(* Lint error types *)
type lint_error_kind =
  | UnusedVariable of string * Error.position option
  | UndefinedVariable of string * Error.position option
  | UnreachableClause of string * int
  | OverlappingClause of string * int * int
  | InvalidImport of string
  | InvalidExport of string
  | UndefinedFunction of string * int * Error.position option
  | InvalidRecordDefinition of string
  | InvalidTypeDefinition of string
  | InvalidMacroDefinition of string
  | IncompatibleReturnType of string * string * string
  | VariableShadowing of string * Error.position option * Error.position option
  | MissingOtpCallback of string * string (* worker_name, callback_name *)
  | InvalidOtpCallback of
      string
      * string
      * int
      * string (* worker_name, callback_name, found_arity, expected_arities *)
  | UnusedFunction of string * int (* function_name, arity *)
  | UnusedExternalCall of string * string (* module, function *)
  | UnusedLiteral of
      string * Error.position option (* literal_value, position *)

type lint_error = {
  kind : lint_error_kind;
  position : Error.position;
  message : string;
  suggestion : string option;
  severity : [ `Error | `Warning ];
}

exception LintError of lint_error list

(* Main linting function *)
val lint_program : ?skip_unused_functions:bool -> program -> unit

(* Error formatting *)
val string_of_lint_error : lint_error -> string
val string_of_lint_errors : lint_error list -> string
