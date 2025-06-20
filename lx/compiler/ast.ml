type ident = string

(* Basic literals *)
type literal =
  | LString of string
  | LInt of int
  | LFloat of float
  | LBool of bool
  | LAtom of string

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

(* OTP handler types *)
type otp_handler = Init | Call | Cast | Info | Terminate

(* Expressions *)
type expr =
  | Literal of literal
  | Var of ident
  | Let of ident * expr * expr
  | Fun of ident list * expr
  | App of expr * expr list
  | Tuple of expr list
  | List of expr list
  | Match of expr * (pattern * expr) list
  | If of expr * expr * expr option
  | For of ident * expr * expr

(* Function definitions *)
type function_def = { name : ident; params : ident list; body : expr }

(* Formal specifications *)
type spec = { name : ident; requires : expr list; ensures : expr list }

(* Test definitions *)
type test_def = { name : string; body : expr }
type describe_block = { name : string; tests : test_def list }

(* OTP components *)
type otp_component =
  | Worker of {
      name : ident;
      handlers : (otp_handler * function_def) list;
      functions : function_def list;
      specs : spec list;
    }
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
