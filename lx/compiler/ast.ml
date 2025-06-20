type ident = string

type literal =
  | LString of string
  | LInt of int

type expr =
  | Literal of literal
  | Var of ident
  | Let of ident * expr

type program =
  | Expr of expr