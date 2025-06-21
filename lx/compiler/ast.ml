type ident = string

(* Position information for better error reporting *)
type position = { line : int; column : int; filename : string option }

(* Basic literals *)
type literal =
  | LString of string
  | LInt of int
  | LFloat of float
  | LBool of bool
  | LAtom of string
  | LNil

(* Pattern matching *)
type pattern =
  | PWildcard
  | PVar of ident
  | PAtom of string
  | PLiteral of literal
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern

(* OTP strategies for supervisors *)
type otp_strategy = OneForOne | OneForAll | RestForOne

(* Special OTP callback function names *)
type otp_callback =
  | Init
  | HandleCall
  | HandleCast
  | HandleInfo
  | Terminate
  | CodeChange
  | FormatStatus

(* Expressions *)
type expr =
  | Literal of literal
  | Var of ident
  | Assign of ident * expr * position option (* variable, value, position *)
  | Fun of ident list * expr
  | App of expr * expr list
  | ExternalCall of string * string * expr list (* module, function, args *)
  | Tuple of expr list
  | List of expr list
  | Match of expr * (pattern * expr) list
  | If of expr * expr * expr option
  | For of ident * expr * expr
  | Sequence of expr list (* Function body sequences *)
  | Block of expr list (* Explicit block expressions {} *)
  | BinOp of expr * string * expr (* Binary operations *)

(* Function clause for multiple arities *)
type function_clause = {
  params : pattern list;
  body : expr;
  position : position option (* Position of the clause *)
}

(* Function definitions with multiple arities *)
type function_def = {
  name : ident;
  clauses : function_clause list;
  position : position option (* Position of the function definition *)
}

(* Helper function to create single-clause function for backward compatibility *)
let make_single_clause_function name params body =
  { name; clauses = [ { params = List.map (fun p -> PVar p) params; body; position = None } ]; position = None }

(* Formal specifications *)
type spec = { name : ident; requires : expr list; ensures : expr list }

(* Test definitions *)
type test_def = { name : string; body : expr }
type describe_block = { name : string; tests : test_def list }

(* OTP components *)
type otp_component =
  | Worker of { name : ident; functions : function_def list; specs : spec list }
  | Supervisor of {
      name : ident;
      strategy : otp_strategy;
      children : ident list;
    }

(* Module items *)
type module_item =
  | Function of function_def
  | OtpComponent of otp_component
  | Spec of spec
  | Test of describe_block

(* Complete program *)
type program = { items : module_item list }

(* Helper functions for OTP callback validation *)
let string_of_otp_callback = function
  | Init -> "init"
  | HandleCall -> "handle_call"
  | HandleCast -> "handle_cast"
  | HandleInfo -> "handle_info"
  | Terminate -> "terminate"
  | CodeChange -> "code_change"
  | FormatStatus -> "format_status"

let is_otp_callback_name name =
  match name with
  | "init" | "handle_call" | "handle_cast" | "handle_info" | "terminate"
  | "code_change" | "format_status" ->
      true
  | _ -> false

let otp_callback_of_string = function
  | "init" -> Some Init
  | "handle_call" -> Some HandleCall
  | "handle_cast" -> Some HandleCast
  | "handle_info" -> Some HandleInfo
  | "terminate" -> Some Terminate
  | "code_change" -> Some CodeChange
  | "format_status" -> Some FormatStatus
  | _ -> None

let expected_arity_for_callback = function
  | Init -> 1
  | HandleCall -> 3
  | HandleCast -> 2
  | HandleInfo -> 2
  | Terminate -> 2
  | CodeChange -> 3
  | FormatStatus -> 1
