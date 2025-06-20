(* Type Checker for LX Language *)
(* Implements Hindley-Milner type inference with OTP support *)

open Ast

(* Type variables and type expressions *)
type type_var = int

type lx_type =
  | TVar of type_var
  | TInteger
  | TFloat
  | TString
  | TBool
  | TAtom
  | TNil
  | TOption of lx_type
  | TFun of lx_type * lx_type
  | TTuple of lx_type list
  | TList of lx_type
  | TRecord of (string * lx_type) list
  | TOtpState
  | TOtpReply of lx_type
  | TOtpCast
  | TOtpInfo

(* Type environment *)
type type_env = (ident * lx_type) list

(* Type substitution *)
type substitution = (type_var * lx_type) list

(* Error types *)
type type_error =
  | UnificationError of lx_type * lx_type
  | UnboundVariable of ident
  | OccursCheck of type_var * lx_type
  | HandlerTypeMismatch of otp_handler * lx_type * lx_type
  | MissingHandler of otp_handler
  | InvalidOtpComponent of string

exception TypeError of type_error

(* Global type variable counter *)
let type_var_counter = ref 0

let fresh_type_var () =
  incr type_var_counter;
  TVar !type_var_counter

let reset_type_vars () = type_var_counter := 0

(* Pretty printing for types *)
let rec string_of_type = function
  | TVar n -> "t" ^ string_of_int n
  | TInteger -> "integer"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TAtom -> "atom"
  | TNil -> "nil"
  | TOption t -> "?" ^ string_of_type t
  | TFun (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TTuple ts -> "{" ^ String.concat ", " (List.map string_of_type ts) ^ "}"
  | TList t -> "[" ^ string_of_type t ^ "]"
  | TRecord fields ->
      "#{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> k ^ ": " ^ string_of_type v) fields)
      ^ "}"
  | TOtpState -> "otp_state"
  | TOtpReply t -> "otp_reply(" ^ string_of_type t ^ ")"
  | TOtpCast -> "otp_cast"
  | TOtpInfo -> "otp_info"

let string_of_type_error = function
  | UnificationError (t1, t2) ->
      "Cannot unify types: " ^ string_of_type t1 ^ " and " ^ string_of_type t2
  | UnboundVariable var -> "Unbound variable: " ^ var
  | OccursCheck (var, typ) ->
      "Occurs check failed: " ^ string_of_type (TVar var) ^ " occurs in "
      ^ string_of_type typ
  | HandlerTypeMismatch (handler, expected, actual) ->
      "Handler "
      ^ (match handler with
        | Init -> "init"
        | Call -> "call"
        | Cast -> "cast"
        | Info -> "info"
        | Terminate -> "terminate")
      ^ " has wrong type. Expected: " ^ string_of_type expected ^ ", but got: "
      ^ string_of_type actual
  | MissingHandler handler -> (
      "Missing required handler: "
      ^
      match handler with
      | Init -> "init"
      | Call -> "call"
      | Cast -> "cast"
      | Info -> "info"
      | Terminate -> "terminate")
  | InvalidOtpComponent msg -> "Invalid OTP component: " ^ msg

(* Substitution operations *)
let rec apply_subst (subst : substitution) (typ : lx_type) : lx_type =
  match typ with
  | TVar var -> ( try List.assoc var subst with Not_found -> typ)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TTuple ts -> TTuple (List.map (apply_subst subst) ts)
  | TList t -> TList (apply_subst subst t)
  | TOption t -> TOption (apply_subst subst t)
  | TRecord fields ->
      TRecord (List.map (fun (k, v) -> (k, apply_subst subst v)) fields)
  | TOtpReply t -> TOtpReply (apply_subst subst t)
  | _ -> typ

let apply_subst_env (subst : substitution) (env : type_env) : type_env =
  List.map (fun (var, typ) -> (var, apply_subst subst typ)) env

let compose_subst (s1 : substitution) (s2 : substitution) : substitution =
  let s1_applied = List.map (fun (var, typ) -> (var, apply_subst s2 typ)) s1 in
  s1_applied @ s2

(* Occurs check for infinite types *)
let rec occurs_check (var : type_var) (typ : lx_type) : bool =
  match typ with
  | TVar v -> var = v
  | TFun (t1, t2) -> occurs_check var t1 || occurs_check var t2
  | TTuple ts -> List.exists (occurs_check var) ts
  | TList t -> occurs_check var t
  | TOption t -> occurs_check var t
  | TRecord fields -> List.exists (fun (_, t) -> occurs_check var t) fields
  | TOtpReply t -> occurs_check var t
  | _ -> false

(* Unification algorithm *)
let rec unify (t1 : lx_type) (t2 : lx_type) : substitution =
  match (t1, t2) with
  | TVar var, typ | typ, TVar var ->
      if TVar var = typ then []
      else if occurs_check var typ then
        raise (TypeError (OccursCheck (var, typ)))
      else [ (var, typ) ]
  | TInteger, TInteger
  | TFloat, TFloat
  | TString, TString
  | TBool, TBool
  | TAtom, TAtom
  | TNil, TNil
  | TOtpState, TOtpState
  | TOtpCast, TOtpCast
  | TOtpInfo, TOtpInfo ->
      []
  | TFun (t1a, t1b), TFun (t2a, t2b) ->
      let s1 = unify t1a t2a in
      let s2 = unify (apply_subst s1 t1b) (apply_subst s1 t2b) in
      compose_subst s1 s2
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
      List.fold_left2
        (fun acc t1 t2 ->
          let s = unify (apply_subst acc t1) (apply_subst acc t2) in
          compose_subst acc s)
        [] ts1 ts2
  | TList t1, TList t2 -> unify t1 t2
  | TOption t1, TOption t2 -> unify t1 t2
  | TOtpReply t1, TOtpReply t2 -> unify t1 t2
  | _ -> raise (TypeError (UnificationError (t1, t2)))

(* Type inference for literals *)
let infer_literal = function
  | LInt _ -> TInteger
  | LFloat _ -> TFloat
  | LString _ -> TString
  | LBool _ -> TBool
  | LAtom _ -> TAtom
  | LNil -> TNil

(* Type inference for patterns *)
let rec infer_pattern (env : type_env) (pattern : pattern) :
    lx_type * type_env * substitution =
  match pattern with
  | PWildcard -> (fresh_type_var (), env, [])
  | PVar var ->
      let typ = fresh_type_var () in
      (typ, (var, typ) :: env, [])
  | PAtom _ -> (TAtom, env, [])
  | PLiteral lit -> (infer_literal lit, env, [])
  | PTuple patterns ->
      let types, new_env, substs =
        List.fold_left
          (fun (types, env_acc, substs_acc) pat ->
            let typ, new_env, subst = infer_pattern env_acc pat in
            (typ :: types, new_env, compose_subst substs_acc subst))
          ([], env, []) patterns
      in
      (TTuple (List.rev types), new_env, substs)
  | PList patterns ->
      let elem_type = fresh_type_var () in
      let _, new_env, substs =
        List.fold_left
          (fun (_, env_acc, substs_acc) pat ->
            let typ, new_env, subst = infer_pattern env_acc pat in
            let unify_subst = unify typ elem_type in
            ( typ,
              new_env,
              compose_subst (compose_subst substs_acc subst) unify_subst ))
          (elem_type, env, []) patterns
      in
      (TList elem_type, new_env, substs)
  | PCons (head, tail) ->
      let head_type, env1, subst1 = infer_pattern env head in
      let tail_type, env2, subst2 = infer_pattern env1 tail in
      let list_type = TList head_type in
      let unify_subst = unify tail_type list_type in
      let final_subst =
        compose_subst (compose_subst subst1 subst2) unify_subst
      in
      (list_type, env2, final_subst)

(* Type inference for expressions *)
let rec infer_expr (env : type_env) (expr : expr) : lx_type * substitution =
  match expr with
  | Literal lit -> (infer_literal lit, [])
  | Var var -> (
      try (List.assoc var env, [])
      with Not_found -> raise (TypeError (UnboundVariable var)))
  | Let (var, value, body) ->
      let value_type, subst1 = infer_expr env value in
      let new_env = (var, value_type) :: apply_subst_env subst1 env in
      let body_type, subst2 = infer_expr new_env body in
      (body_type, compose_subst subst1 subst2)
  | Fun (params, body) ->
      let param_types = List.map (fun _ -> fresh_type_var ()) params in
      let param_env = List.combine params param_types in
      let new_env = param_env @ env in
      let body_type, subst = infer_expr new_env body in
      let fun_type =
        List.fold_right
          (fun param_type acc -> TFun (param_type, acc))
          param_types body_type
      in
      (fun_type, subst)
  | App (func, args) ->
      let func_type, subst1 = infer_expr env func in
      let arg_types, subst2 =
        List.fold_left
          (fun (types, subst_acc) arg ->
            let arg_type, subst =
              infer_expr (apply_subst_env subst_acc env) arg
            in
            (arg_type :: types, compose_subst subst_acc subst))
          ([], subst1) args
      in
      let arg_types = List.rev arg_types in
      let result_type = fresh_type_var () in
      let expected_func_type =
        List.fold_right
          (fun arg_type acc -> TFun (arg_type, acc))
          arg_types result_type
      in
      let unify_subst =
        unify (apply_subst subst2 func_type) expected_func_type
      in
      let final_subst = compose_subst subst2 unify_subst in
      (apply_subst final_subst result_type, final_subst)
  | Tuple exprs ->
      let types, subst =
        List.fold_left
          (fun (types, subst_acc) expr ->
            let typ, subst = infer_expr (apply_subst_env subst_acc env) expr in
            (typ :: types, compose_subst subst_acc subst))
          ([], []) exprs
      in
      (TTuple (List.rev types), subst)
  | List exprs ->
      let elem_type = fresh_type_var () in
      let subst =
        List.fold_left
          (fun subst_acc expr ->
            let typ, subst = infer_expr (apply_subst_env subst_acc env) expr in
            let unify_subst = unify typ elem_type in
            compose_subst (compose_subst subst_acc subst) unify_subst)
          [] exprs
      in
      (TList (apply_subst subst elem_type), subst)
  | Match (value, cases) ->
      let value_type, subst1 = infer_expr env value in
      let result_type = fresh_type_var () in
      let final_subst, _ =
        List.fold_left
          (fun (subst_acc, env_acc) (pattern, case_expr) ->
            let pattern_type, pattern_env, pattern_subst =
              infer_pattern env_acc pattern
            in
            let unify_subst1 =
              unify (apply_subst subst_acc value_type) pattern_type
            in
            let combined_subst =
              compose_subst (compose_subst subst_acc pattern_subst) unify_subst1
            in
            let case_env =
              pattern_env @ apply_subst_env combined_subst env_acc
            in
            let case_type, case_subst = infer_expr case_env case_expr in
            let unify_subst2 =
              unify (apply_subst case_subst result_type) case_type
            in
            let final_subst =
              compose_subst
                (compose_subst combined_subst case_subst)
                unify_subst2
            in
            (final_subst, apply_subst_env final_subst env_acc))
          (subst1, env) cases
      in
      (apply_subst final_subst result_type, final_subst)
  | If (cond, then_expr, else_expr) -> (
      let cond_type, subst1 = infer_expr env cond in
      let bool_unify = unify cond_type TBool in
      let combined_subst1 = compose_subst subst1 bool_unify in
      let then_type, subst2 =
        infer_expr (apply_subst_env combined_subst1 env) then_expr
      in
      let combined_subst2 = compose_subst combined_subst1 subst2 in
      match else_expr with
      | Some else_expr ->
          let else_type, subst3 =
            infer_expr (apply_subst_env combined_subst2 env) else_expr
          in
          let else_unify = unify (apply_subst subst3 then_type) else_type in
          let final_subst =
            compose_subst (compose_subst combined_subst2 subst3) else_unify
          in
          (apply_subst final_subst then_type, final_subst)
      | None ->
          (* If without else returns optional type: T | nil *)
          (TOption (apply_subst combined_subst2 then_type), combined_subst2))
  | For (_, _, _) ->
      (* For expressions need more complex handling - simplified for now *)
      (TNil, [])

(* Type inference for function definitions *)
let infer_function_def (env : type_env) (func : function_def) :
    lx_type * substitution =
  let param_types = List.map (fun _ -> fresh_type_var ()) func.params in
  let param_env = List.combine func.params param_types in
  let new_env = param_env @ env in
  let body_type, subst = infer_expr new_env func.body in
  let fun_type =
    List.fold_right
      (fun param_type acc -> TFun (param_type, acc))
      param_types body_type
  in
  (fun_type, subst)

(* Built-in environment with OTP types *)
let builtin_env : type_env =
  [
    ( "gen_server",
      TFun
        (TAtom, TFun (TAtom, TFun (TList TAtom, TFun (TList TAtom, TOtpState))))
    );
    ("logger", TFun (TString, TNil));
    ("ok", TAtom);
    ("error", TFun (TAtom, TTuple [ TAtom; TAtom ]));
    ("reply", TFun (TAtom, TFun (TOtpState, TOtpReply TAtom)));
    ("noreply", TFun (TOtpState, TOtpReply TNil));
    ("stop", TFun (TAtom, TFun (TOtpState, TOtpReply TNil)));
    ("nil", TNil);
  ]

(* Main type checking function *)
let type_check_program (program : program) : type_env =
  reset_type_vars ();
  let env = builtin_env in

  (* First pass: collect function signatures *)
  let env_with_functions =
    List.fold_left
      (fun env_acc item ->
        match item with
        | Function func ->
            let func_type, _ = infer_function_def env_acc func in
            (func.name, func_type) :: env_acc
        | _ -> env_acc)
      env program.items
  in

  (* Second pass: type check all items *)
  List.fold_left
    (fun env_acc item ->
      match item with
      | Function func ->
          let _, subst = infer_function_def env_with_functions func in
          apply_subst_env subst env_acc
      | OtpComponent _ ->
          (* OTP component type checking handled by otp_validator *)
          env_acc
      | Spec _ ->
          (* Specs are handled separately *)
          env_acc
      | Test desc ->
          (* Type check test expressions *)
          List.fold_left
            (fun test_env test ->
              let _, subst = infer_expr env_with_functions test.body in
              apply_subst_env subst test_env)
            env_acc desc.tests)
    env_with_functions program.items
