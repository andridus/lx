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
  | TPid
  | TNil
  | TOption of lx_type
  | TFun of lx_type * lx_type
  | TTuple of lx_type list
  | TList of lx_type
  | TRecord of (string * lx_type) list
  | TNamedRecord of string (* Named record type *)
  | TMap of lx_type * lx_type (* TMap(key_type, value_type) *)
  | TOtpState
  | TOtpReply of lx_type
  | TOtpCast
  | TOtpInfo

(* Type environment *)
type type_env = (ident * lx_type) list

(* Record environment *)
type record_env = (string * record_def) list

let record_env = ref []

let add_record_def (def : record_def) =
  record_env := (def.record_name, def) :: !record_env

(* Symbol table context for managing variable scopes *)
type symbol_context = {
  variables : (ident * lx_type * Error.position option) list;
      (* name, type, definition position *)
  parent : symbol_context option;
}

(* Create new context *)
let create_context parent = { variables = []; parent }

(* Lookup variable in context hierarchy *)
let rec lookup_variable_in_context name context =
  try
    let _, var_type, _ =
      List.find (fun (n, _, _) -> n = name) context.variables
    in
    Some var_type
  with Not_found -> (
    match context.parent with
    | Some parent_ctx -> lookup_variable_in_context name parent_ctx
    | None -> None)

(* Get variable position from context *)
let rec get_variable_position name context =
  try
    let _, _, pos = List.find (fun (n, _, _) -> n = name) context.variables in
    pos
  with Not_found -> (
    match context.parent with
    | Some parent_ctx -> get_variable_position name parent_ctx
    | None -> None)

(* Global scope tracking for better error reporting *)
let scope_depth = ref 0

(* Helper function to determine scope context *)
let get_scope_context () =
  if !scope_depth > 1 then " (within nested block scope)"
  else if !scope_depth > 0 then " (within block scope)"
  else ""

(* Add variable to current context, checking for reassignment and shadowing *)
let add_variable_to_context ?(filename = None) ?(lexbuf = None) name var_type
    ?pos context =
  (* Check if variable already exists in current scope *)
  if List.exists (fun (n, _, _) -> n = name) context.variables then
    let first_pos = get_variable_position name context in
    match lexbuf with
    | Some lb -> Error.variable_redefinition_error ~filename ~first_pos lb name
    | None ->
        (* Use the actual position passed in, not an estimate *)
        let current_pos = pos in
        let error =
          Error.make_error_with_position
            (Error.VariableRedefinition (name, first_pos))
            (match current_pos with
            | Some p -> p
            | None -> Error.make_position 1 1)
            ""
        in
        raise (Error.CompilationError error)
  else
    (* Check if variable exists in parent scope - shadowing not allowed *)
    let parent_has_var =
      match context.parent with
      | Some parent_ctx -> lookup_variable_in_context name parent_ctx <> None
      | None -> false
    in
    if parent_has_var then
      let parent_pos =
        match context.parent with
        | Some parent_ctx -> get_variable_position name parent_ctx
        | None -> None
      in
      match lexbuf with
      | Some lb -> Error.variable_shadowing_error ~filename ~parent_pos lb name
      | None ->
          (* Use the actual position passed in, not an estimate *)
          let current_pos = pos in
          let error =
            Error.make_error_with_position
              (Error.VariableShadowing (name, parent_pos))
              (match current_pos with
              | Some p -> p
              | None -> Error.make_position 1 1)
              ""
          in
          raise (Error.CompilationError error)
    else
      (* Store the actual position information *)
      { context with variables = (name, var_type, pos) :: context.variables }

(* Convert context to type_env for compatibility *)
let context_to_env context =
  let rec collect_vars ctx acc =
    let vars =
      List.map (fun (name, typ, _) -> (name, typ)) ctx.variables @ acc
    in
    match ctx.parent with
    | Some parent -> collect_vars parent vars
    | None -> vars
  in
  collect_vars context []

(* Type substitution *)
type substitution = (type_var * lx_type) list

(* Enhanced error types with better context *)
type type_error =
  | UnificationError of lx_type * lx_type * string option
  | PatternMatchError of lx_type * lx_type * Error.position option * string
  | UnboundVariable of ident * string list
  | OccursCheck of type_var * lx_type
  | InvalidOtpComponent of string
  | RecordFieldMismatch of string * string * string list
  | ArityMismatch of string * int * int
  | ContextualTypeError of string * string * string option
  | UndefinedRecord of string
  | MissingRecordField of string * string
  | InvalidRecordAccess of lx_type * string
  | InvalidRecordUpdate of lx_type

exception TypeError of type_error

(* Guard type checking error *)
type guard_error =
  | InvalidGuardFunction of string
  | InvalidGuardCall of
      string * int * int (* function, given arity, expected arity *)
  | UndefinedGuardVariable of string

exception GuardError of guard_error

(* Find record definition function - moved here after TypeError is declared *)
let find_record_def (name : string) : record_def =
  try List.assoc name !record_env
  with Not_found -> raise (TypeError (UndefinedRecord name))

(* Global type variable counter *)
let type_var_counter = ref 0

let fresh_type_var () =
  incr type_var_counter;
  TVar !type_var_counter

let reset_type_vars () = type_var_counter := 0

(* Convert type expressions to internal types *)
let rec convert_type_expr (type_expr : type_expr) : lx_type =
  match type_expr with
  | TypeName "string" -> TString
  | TypeName "integer" -> TInteger
  | TypeName "float" -> TFloat
  | TypeName "boolean" -> TBool
  | TypeName "atom" -> TAtom
  | TypeName "pid" -> TPid
  | TypeName "nil" -> TNil
  | TypeName name ->
      TRecord [ (name, fresh_type_var ()) ]
      (* For now, treat unknown types as records *)
  | TypeList inner -> TList (convert_type_expr inner)
  | TypeTuple types -> TTuple (List.map convert_type_expr types)
  | TypeUnion (t1, _t2) -> TOption (convert_type_expr t1)
(* Simplified union handling *)

(* Pretty printing for types *)
let rec string_of_type = function
  | TVar n -> "t" ^ string_of_int n
  | TInteger -> "integer"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TAtom -> "atom"
  | TPid -> "pid"
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
  | TNamedRecord name -> name
  | TMap (key_type, value_type) ->
      "%{" ^ string_of_type key_type ^ " => " ^ string_of_type value_type ^ "}"
  | TOtpState -> "otp_state"
  | TOtpReply t -> "otp_reply(" ^ string_of_type t ^ ")"
  | TOtpCast -> "otp_cast"
  | TOtpInfo -> "otp_info"

let string_of_type_error = function
  | PatternMatchError (_pattern_type, _value_type, _pos, explanation) ->
      explanation
  | UnificationError (t1, t2, context) ->
      let context_part =
        match context with Some ctx -> " in " ^ ctx | None -> ""
      in
      Printf.sprintf "Cannot unify types%s: %s and %s" context_part
        (string_of_type t1) (string_of_type t2)
  | UnboundVariable (var, similar) ->
      let suggestion =
        match similar with
        | [] -> ""
        | [ s ] -> Printf.sprintf " (did you mean '%s'?)" s
        | suggestions ->
            Printf.sprintf " (did you mean one of: %s?)"
              (String.concat ", " suggestions)
      in
      Printf.sprintf "Unbound variable: %s%s" var suggestion
  | OccursCheck (var, typ) ->
      "Occurs check failed: " ^ string_of_type (TVar var) ^ " occurs in "
      ^ string_of_type typ
  | InvalidOtpComponent msg -> "Invalid OTP component: " ^ msg
  | RecordFieldMismatch (field, record_type, available) ->
      let available_str = String.concat ", " available in
      Printf.sprintf
        "Field '%s' does not exist in record type '%s'. Available fields: %s"
        field record_type available_str
  | ArityMismatch (func, expected, found) ->
      Printf.sprintf "Function '%s' expects %d arguments, but %d were provided"
        func expected found
  | ContextualTypeError (msg, context, suggestion) ->
      let suggestion_part =
        match suggestion with Some s -> " Suggestion: " ^ s | None -> ""
      in
      Printf.sprintf "%s in %s.%s" msg context suggestion_part
  | UndefinedRecord name ->
      Printf.sprintf
        "Undefined record: %s\n\
        \  Suggestion: Make sure the record is defined with 'record %s { ... \
         }' before using it in functions\n\
        \  Context: Record definitions must appear at the module level, not \
         inside functions"
        name name
  | MissingRecordField (field, record) ->
      Printf.sprintf "Missing required field '%s' in record '%s'" field record
  | InvalidRecordAccess (typ, field) ->
      Printf.sprintf "Cannot access field '%s' on non-record type: %s" field
        (string_of_type typ)
  | InvalidRecordUpdate typ ->
      Printf.sprintf "Cannot update non-record type: %s" (string_of_type typ)

(* Helper function to get variable names from environment *)
let get_env_var_names env = List.map fst env

(* Helper function to get record field names *)
let get_record_fields = function
  | TRecord fields -> List.map fst fields
  | _ -> []

(* Generate specific error message for pattern matching *)
let generate_pattern_match_error pattern_type value_type =
  match (pattern_type, value_type) with
  | TInteger, TNamedRecord name ->
      Printf.sprintf
        "Cannot match integer pattern against record value of type '%s'.\n\
         The pattern '1 = x' attempts to match the integer 1 against a record \
         value, which is impossible."
        name
  | TNamedRecord name, TInteger ->
      Printf.sprintf
        "Cannot match record pattern of type '%s' against integer value.\n\
         Make sure the pattern type matches the value type."
        name
  | TInteger, _ ->
      Printf.sprintf
        "Cannot match integer pattern against value of type '%s'.\n\
         Pattern and value types must be compatible."
        (string_of_type value_type)
  | _, TInteger ->
      Printf.sprintf
        "Cannot match pattern of type '%s' against integer value.\n\
         Pattern and value types must be compatible."
        (string_of_type pattern_type)
  | _ ->
      Printf.sprintf
        "Pattern type '%s' does not match value type '%s'.\n\
         Pattern and value types must be compatible."
        (string_of_type pattern_type)
        (string_of_type value_type)

(* Convert AST position to Error position *)
let convert_position = function
  | Some pos ->
      Some
        {
          Error.line = pos.line;
          Error.column = pos.column;
          Error.filename = pos.filename;
        }
  | None -> None

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
  | TNamedRecord name ->
      TNamedRecord name (* Named records don't need substitution *)
  | TMap (key_type, value_type) ->
      TMap (apply_subst subst key_type, apply_subst subst value_type)
  | TOtpReply t -> TOtpReply (apply_subst subst t)
  | _ -> typ

let apply_subst_env (subst : substitution) (env : type_env) : type_env =
  List.map (fun (var, typ) -> (var, apply_subst subst typ)) env

let compose_subst (s1 : substitution) (s2 : substitution) : substitution =
  let s1_applied = List.map (fun (var, typ) -> (var, apply_subst s2 typ)) s1 in
  s1_applied @ s2

(* Type inference for literals *)
let infer_literal = function
  | LInt _ -> TInteger
  | LFloat _ -> TFloat
  | LString _ -> TString
  | LBool _ -> TBool
  | LAtom _ -> TAtom
  | LNil -> TNil

(* Guard type checking functions *)
let rec infer_guard_expr (env : type_env) (guard : guard_expr) : substitution =
  match guard with
  | GuardAnd (g1, g2)
  | GuardOr (g1, g2)
  | GuardAndalso (g1, g2)
  | GuardOrelse (g1, g2) ->
      let s1 = infer_guard_expr env g1 in
      let s2 = infer_guard_expr (apply_subst_env s1 env) g2 in
      compose_subst s1 s2
  | GuardNot g -> infer_guard_expr env g
  | GuardBinOp (left, _op, right) ->
      (* Type check operands and ensure comparison returns boolean *)
      let _ = infer_guard_value env left in
      let _ = infer_guard_value env right in
      [] (* Comparisons always return boolean *)
  | GuardCall (func, args) ->
      (* Validate built-in guard functions *)
      validate_guard_call_values func args;
      []
  | GuardAtom atom ->
      let _typ = infer_guard_atom env atom in
      (* Ensure guard atom evaluates to boolean if used alone *)
      []

and infer_guard_atom (env : type_env) (atom : guard_atom) : lx_type =
  match atom with
  | GuardVar var -> (
      try List.assoc var env
      with Not_found -> raise (GuardError (UndefinedGuardVariable var)))
  | GuardLiteral lit -> infer_literal lit
  | GuardCallAtom (func, args) -> (
      validate_guard_call func args;
      (* Return appropriate type based on guard function *)
      match func with
      | "hd" | "tl" ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for head/tail *)
      | "length" -> TInteger
      | "element" ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for element *)
      | "is_atom" | "is_integer" | "is_float" | "is_number" | "is_boolean"
      | "is_list" | "is_tuple" ->
          TBool
      | "abs" | "round" | "trunc" -> TInteger
      | _ ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for unknown functions *))

and validate_guard_call (func : string) (args : guard_atom list) : unit =
  match func with
  | "is_atom" | "is_integer" | "is_float" | "is_number" | "is_boolean"
  | "is_list" | "is_tuple" ->
      if List.length args != 1 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 1)))
  | "abs" | "round" | "trunc" | "hd" | "tl" | "length" ->
      if List.length args != 1 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 1)))
  | "element" ->
      if List.length args != 2 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 2)))
  | _ -> raise (GuardError (InvalidGuardFunction func))

and validate_guard_call_values (func : string) (args : guard_value list) : unit
    =
  match func with
  | "is_atom" | "is_integer" | "is_float" | "is_number" | "is_boolean"
  | "is_list" | "is_tuple" ->
      if List.length args != 1 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 1)))
  | "abs" | "round" | "trunc" | "hd" | "tl" | "length" ->
      if List.length args != 1 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 1)))
  | "element" ->
      if List.length args != 2 then
        raise (GuardError (InvalidGuardCall (func, List.length args, 2)))
  | _ -> raise (GuardError (InvalidGuardFunction func))

and infer_guard_value (env : type_env) (value : guard_value) : lx_type =
  match value with
  | GuardAtomValue atom -> infer_guard_atom env atom
  | GuardCallValue (func, args) -> (
      validate_guard_call_values func args;
      (* Return appropriate type based on guard function *)
      match func with
      | "hd" | "tl" ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for head/tail *)
      | "length" -> TInteger
      | "element" ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for element *)
      | "is_atom" | "is_integer" | "is_float" | "is_number" | "is_boolean"
      | "is_list" | "is_tuple" ->
          TBool
      | "abs" | "round" | "trunc" -> TInteger
      | _ ->
          incr type_var_counter;
          TVar !type_var_counter (* Generic type for unknown functions *))

(* Occurs check for infinite types *)
let rec occurs_check (var : type_var) (typ : lx_type) : bool =
  match typ with
  | TVar v -> var = v
  | TFun (t1, t2) -> occurs_check var t1 || occurs_check var t2
  | TTuple ts -> List.exists (occurs_check var) ts
  | TList t -> occurs_check var t
  | TOption t -> occurs_check var t
  | TRecord fields -> List.exists (fun (_, t) -> occurs_check var t) fields
  | TNamedRecord _ -> false (* Named records don't contain type variables *)
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
  | TNamedRecord name1, TNamedRecord name2 when name1 = name2 -> []
  | TOtpReply t1, TOtpReply t2 -> unify t1 t2
  | TMap (k1, v1), TMap (k2, v2) ->
      let s1 = unify k1 k2 in
      let s2 = unify (apply_subst s1 v1) (apply_subst s1 v2) in
      compose_subst s1 s2
  | _ -> raise (TypeError (UnificationError (t1, t2, None)))

(* Special unification for pattern matching with position and better error messages *)
let unify_pattern_match (pattern_type : lx_type) (value_type : lx_type)
    (pos : Error.position option) : substitution =
  try unify pattern_type value_type with
  | TypeError (UnificationError (_t1, _t2, _)) ->
      let error_msg = generate_pattern_match_error pattern_type value_type in
      raise
        (TypeError
           (PatternMatchError (pattern_type, value_type, pos, error_msg)))
  | other -> raise other

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
  | PRecord (record_name, field_patterns) ->
      let record_def = find_record_def record_name in
      let record_fields =
        List.map
          (fun field -> (field.field_name, convert_type_expr field.field_type))
          record_def.fields
      in
      let record_type = TNamedRecord record_name in

      (* Type check each field pattern *)
      let final_env, final_subst =
        List.fold_left
          (fun (env_acc, subst_acc) (field_name, field_pattern) ->
            let field_type =
              try List.assoc field_name record_fields
              with Not_found ->
                raise
                  (TypeError
                     (RecordFieldMismatch
                        (field_name, record_name, List.map fst record_fields)))
            in
            let pattern_type, new_env, pattern_subst =
              infer_pattern env_acc field_pattern
            in
            let unify_subst = unify pattern_type field_type in
            ( new_env,
              compose_subst (compose_subst subst_acc pattern_subst) unify_subst
            ))
          (env, []) field_patterns
      in

      (record_type, final_env, final_subst)
  | PMap pattern_fields ->
      (* For map patterns, we need to be more flexible with types *)
      (* Extract the pattern variables and let the unification handle the types *)
      let final_env, final_subst =
        List.fold_left
          (fun (env_acc, subst_acc) field ->
            match field with
            | AtomKeyPattern (_, field_pattern) ->
                let _field_type, field_env, field_subst =
                  infer_pattern env_acc field_pattern
                in
                (field_env, compose_subst subst_acc field_subst)
            | GeneralKeyPattern (_key_expr, field_pattern) ->
                (* For general patterns, we need to infer the key type too *)
                let _field_type, field_env, field_subst =
                  infer_pattern env_acc field_pattern
                in
                (field_env, compose_subst subst_acc field_subst))
          (env, []) pattern_fields
      in

      (* For map patterns, we create a generic map type that can be unified later *)
      (* The key insight is that the pattern should be flexible enough to match any map *)
      let key_type = fresh_type_var () in
      let value_type = fresh_type_var () in
      (TMap (key_type, value_type), final_env, final_subst)

(* Type inference for expressions with symbol context *)
let rec infer_expr_with_context (context : symbol_context) (expr : expr) :
    lx_type * substitution * symbol_context =
  match expr with
  | Literal l -> (infer_literal l, [], context)
  | Var id -> (
      match lookup_variable_in_context id context with
      | Some var_type -> (var_type, [], context)
      | None ->
          let similar_vars = List.map fst (context_to_env context) in
          raise (TypeError (UnboundVariable (id, similar_vars))))
  | Assign (id, value, pos) ->
      let value_type, subst, new_context =
        infer_expr_with_context context value
      in
      (* Use the position information from the AST if available *)
      let error_position =
        match pos with
        | Some p ->
            Some
              {
                Error.line = p.line;
                Error.column = p.column;
                Error.filename = p.filename;
              }
        | None -> Some (Error.make_position 1 1)
      in
      let updated_context =
        add_variable_to_context id value_type ?pos:error_position new_context
      in
      (value_type, subst, updated_context)
  | PatternMatch (pattern, value, _pos) ->
      let value_type, subst, new_context =
        infer_expr_with_context context value
      in
      let pattern_type, pattern_env, pattern_subst =
        infer_pattern (context_to_env new_context) pattern
      in
      (* Unify the pattern type with the value type *)
      let unify_subst =
        unify_pattern_match pattern_type value_type (convert_position _pos)
      in
      let final_subst =
        compose_subst (compose_subst subst pattern_subst) unify_subst
      in
      (* Add pattern variables to the context *)
      (* Extract only the new variables from pattern, not the entire environment *)
      let original_env = context_to_env new_context in
      let new_pattern_vars =
        List.filter
          (fun (name, _) -> not (List.mem_assoc name original_env))
          pattern_env
      in
      let updated_context =
        List.fold_left
          (fun ctx_acc (var_name, var_type) ->
            add_variable_to_context var_name var_type ctx_acc)
          new_context new_pattern_vars
      in
      (apply_subst final_subst value_type, final_subst, updated_context)
  | Sequence exprs -> (
      (* Type of sequence is the type of the last expression *)
      match List.rev exprs with
      | [] -> (TNil, [], context)
      | last_expr :: rest_exprs ->
          let rest_exprs = List.rev rest_exprs in
          (* Type check all expressions and accumulate context changes *)
          let final_subst, accumulated_context =
            List.fold_left
              (fun (subst_acc, ctx_acc) expr ->
                let _, subst, new_ctx = infer_expr_with_context ctx_acc expr in
                (compose_subst subst_acc subst, new_ctx))
              ([], context) rest_exprs
          in
          let last_type, last_subst, final_context =
            infer_expr_with_context accumulated_context last_expr
          in
          (last_type, compose_subst final_subst last_subst, final_context))
  | Block exprs ->
      (* Block expressions create a new scope *)
      incr scope_depth;
      (* Enter nested scope *)
      let block_context = create_context (Some context) in
      let result =
        match List.rev exprs with
        | [] -> (TNil, [], context)
        | last_expr :: rest_exprs ->
            let rest_exprs = List.rev rest_exprs in
            (* Type check all expressions in the block scope *)
            let final_subst, accumulated_context =
              List.fold_left
                (fun (subst_acc, ctx_acc) expr ->
                  let _, subst, new_ctx =
                    infer_expr_with_context ctx_acc expr
                  in
                  (compose_subst subst_acc subst, new_ctx))
                ([], block_context) rest_exprs
            in
            let last_type, last_subst, _ =
              infer_expr_with_context accumulated_context last_expr
            in
            (* Return the original context (block variables don't leak out) *)
            (last_type, compose_subst final_subst last_subst, context)
      in
      decr scope_depth;
      (* Exit nested scope *)
      result
  | Send (target_expr, message_expr) ->
      (* Type check target and message *)
      let target_type, s1, ctx1 = infer_expr_with_context context target_expr in
      let message_type, s2, ctx2 = infer_expr_with_context ctx1 message_expr in
      let combined_subst = compose_subst s1 s2 in

      (* Validate target type - should be pid, atom (registered name), or tuple *)
      let target_type_applied = apply_subst combined_subst target_type in
      (match target_type_applied with
      | TPid | TAtom | TTuple _ -> () (* Valid target types *)
      | TVar _ -> () (* Allow type variables for flexibility *)
      | _ ->
          let context_msg = "send operation target" in
          raise
            (TypeError
               (UnificationError (target_type_applied, TPid, Some context_msg))));

      (* Send operation returns the message *)
      (apply_subst combined_subst message_type, combined_subst, ctx2)
  | _ ->
      (* For other expressions, convert context to env and use original function *)
      let env = context_to_env context in
      let result_type, subst = infer_expr_original env expr in
      (result_type, subst, context)

(* Wrapper function that maintains original interface *)
and infer_expr (env : type_env) (expr : expr) : lx_type * substitution =
  let base_context =
    {
      variables = List.map (fun (name, typ) -> (name, typ, None)) env;
      parent = None;
    }
  in
  let result_type, subst, _ = infer_expr_with_context base_context expr in
  (result_type, subst)

(* Original type inference function for compatibility *)
and infer_expr_original (env : type_env) (expr : expr) : lx_type * substitution
    =
  match expr with
  | Literal l -> (infer_literal l, [])
  | Var id -> (
      try (List.assoc id env, [])
      with Not_found ->
        let similar_vars = get_env_var_names env in
        raise (TypeError (UnboundVariable (id, similar_vars))))
  | Assign (_id, value, _pos) ->
      let value_type, subst = infer_expr_original env value in
      (* Assignment returns the assigned value type *)
      (value_type, subst)
  | PatternMatch (pattern, value, _pos) ->
      let value_type, subst = infer_expr_original env value in
      let pattern_type, _pattern_env, pattern_subst =
        infer_pattern env pattern
      in
      (* Unify the pattern type with the value type *)
      let unify_subst =
        unify_pattern_match pattern_type value_type (convert_position _pos)
      in
      let final_subst =
        compose_subst (compose_subst subst pattern_subst) unify_subst
      in
      (* Pattern matching returns the value type *)
      (apply_subst final_subst value_type, final_subst)
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
  | ExternalCall (_module_name, _func_name, args) ->
      (* Type check external function calls *)
      let _arg_types, subst =
        List.fold_left
          (fun (types, subst_acc) arg ->
            let arg_type, subst =
              infer_expr (apply_subst_env subst_acc env) arg
            in
            (arg_type :: types, compose_subst subst_acc subst))
          ([], []) args
      in
      (* For external calls, we assume they return a generic type *)
      (* In a real implementation, we would look up the module's type signature *)
      let result_type = fresh_type_var () in
      (result_type, subst)
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
          (fun (subst_acc, env_acc) (pattern, guard_opt, case_expr) ->
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

            (* Type check guard if present *)
            let guard_subst =
              match guard_opt with
              | Some guard -> infer_guard_expr case_env guard
              | None -> []
            in
            let case_env_with_guard = apply_subst_env guard_subst case_env in
            let case_type, case_subst =
              infer_expr case_env_with_guard case_expr
            in
            let unify_subst2 =
              unify (apply_subst case_subst result_type) case_type
            in
            let final_subst =
              compose_subst
                (compose_subst
                   (compose_subst combined_subst guard_subst)
                   case_subst)
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
  | Sequence exprs -> (
      (* Sequence expressions have the same type behavior as blocks *)
      match List.rev exprs with
      | [] -> (TNil, [])
      | last_expr :: rest_exprs ->
          let rest_exprs = List.rev rest_exprs in
          (* Type check all expressions but return type of last one *)
          let final_subst =
            List.fold_left
              (fun subst_acc expr ->
                let _, subst =
                  infer_expr (apply_subst_env subst_acc env) expr
                in
                compose_subst subst_acc subst)
              [] rest_exprs
          in
          let last_type, last_subst =
            infer_expr (apply_subst_env final_subst env) last_expr
          in
          (last_type, compose_subst final_subst last_subst))
  | Block exprs -> (
      (* Block expressions have the same type behavior as sequences *)
      match List.rev exprs with
      | [] -> (TNil, [])
      | last_expr :: rest_exprs ->
          let rest_exprs = List.rev rest_exprs in
          (* Type check all expressions but return type of last one *)
          let final_subst =
            List.fold_left
              (fun subst_acc expr ->
                let _, subst =
                  infer_expr (apply_subst_env subst_acc env) expr
                in
                compose_subst subst_acc subst)
              [] rest_exprs
          in
          let last_type, last_subst =
            infer_expr (apply_subst_env final_subst env) last_expr
          in
          (last_type, compose_subst final_subst last_subst))
  | UnaryOp (op, operand) -> (
      let operand_type, subst = infer_expr env operand in
      match op with
      | "not" ->
          (* not operator requires boolean operand and returns boolean *)
          let bool_unify = unify (apply_subst subst operand_type) TBool in
          let final_subst = compose_subst subst bool_unify in
          (TBool, final_subst)
      | _ -> failwith ("Unknown unary operator: " ^ op))
  | BinOp (left, op, right) -> (
      let left_type, subst1 = infer_expr env left in
      let right_type, subst2 = infer_expr (apply_subst_env subst1 env) right in
      let combined_subst = compose_subst subst1 subst2 in
      (* For arithmetic operations, both operands should be numbers *)
      match op with
      | "++" ->
          (* String concatenation requires string operands and returns string *)
          let string_unify1 =
            unify (apply_subst combined_subst left_type) TString
          in
          let string_unify2 =
            unify (apply_subst combined_subst right_type) TString
          in
          let final_subst =
            compose_subst
              (compose_subst combined_subst string_unify1)
              string_unify2
          in
          (TString, final_subst)
      | "+" | "-" | "*" | "/" ->
          let int_unify1 =
            unify (apply_subst combined_subst left_type) TInteger
          in
          let int_unify2 =
            unify (apply_subst combined_subst right_type) TInteger
          in
          let final_subst =
            compose_subst (compose_subst combined_subst int_unify1) int_unify2
          in
          (TInteger, final_subst)
      (* Comparison operations return boolean *)
      | "==" | "!=" | "<" | ">" | "<=" | ">=" ->
          (* For now, allow comparison of any types - could be more strict *)
          (TBool, combined_subst)
      (* Logical operations require boolean operands and return boolean *)
      | "and" | "or" | "andalso" | "orelse" ->
          let bool_unify1 =
            unify (apply_subst combined_subst left_type) TBool
          in
          let bool_unify2 =
            unify (apply_subst combined_subst right_type) TBool
          in
          let final_subst =
            compose_subst (compose_subst combined_subst bool_unify1) bool_unify2
          in
          (TBool, final_subst)
      | _ -> failwith ("Unknown binary operator: " ^ op))
  | Send (target_expr, message_expr) ->
      (* Type check target and message *)
      let target_type, s1 = infer_expr env target_expr in
      let message_type, s2 = infer_expr (apply_subst_env s1 env) message_expr in
      let combined_subst = compose_subst s1 s2 in

      (* Validate target type - should be pid, atom (registered name), or tuple *)
      let target_type_applied = apply_subst combined_subst target_type in
      (match target_type_applied with
      | TPid | TAtom | TTuple _ -> () (* Valid target types *)
      | TVar _ -> () (* Allow type variables for flexibility *)
      | _ ->
          let context = "send operation target" in
          raise
            (TypeError
               (UnificationError (target_type_applied, TPid, Some context))));

      (* Send operation returns the message *)
      (apply_subst combined_subst message_type, combined_subst)
  | RecordCreate (record_name, field_inits) ->
      let record_def = find_record_def record_name in
      let record_fields =
        List.map
          (fun field -> (field.field_name, convert_type_expr field.field_type))
          record_def.fields
      in

      (* Check all required fields are provided *)
      let provided_fields = List.map fst field_inits in
      let required_fields =
        List.filter (fun f -> f.default_value = None) record_def.fields
      in
      List.iter
        (fun field ->
          if not (List.mem field.field_name provided_fields) then
            raise
              (TypeError
                 (RecordFieldMismatch
                    (field.field_name, record_name, provided_fields))))
        required_fields;

      (* Type check each field initialization *)
      let subst =
        List.fold_left
          (fun subst_acc (field_name, field_expr) ->
            let field_type =
              try List.assoc field_name record_fields
              with Not_found ->
                raise
                  (TypeError
                     (RecordFieldMismatch
                        (field_name, record_name, List.map fst record_fields)))
            in
            let actual_type, expr_subst =
              infer_expr (apply_subst_env subst_acc env) field_expr
            in
            let unify_subst = unify actual_type field_type in
            compose_subst (compose_subst subst_acc expr_subst) unify_subst)
          [] field_inits
      in

      (TNamedRecord record_name, subst)
  | RecordAccess (record_expr, field_name) -> (
      let record_type, subst = infer_expr env record_expr in
      match record_type with
      | TNamedRecord record_name ->
          let record_def = find_record_def record_name in
          let record_fields =
            List.map
              (fun field ->
                (field.field_name, convert_type_expr field.field_type))
              record_def.fields
          in
          let field_type =
            try List.assoc field_name record_fields
            with Not_found ->
              raise
                (TypeError
                   (RecordFieldMismatch
                      (field_name, record_name, List.map fst record_fields)))
          in
          (field_type, subst)
      | TRecord fields ->
          let field_type =
            try List.assoc field_name fields
            with Not_found ->
              raise
                (TypeError
                   (RecordFieldMismatch
                      (field_name, "record", List.map fst fields)))
          in
          (field_type, subst)
      | _ ->
          raise
            (TypeError
               (UnificationError (record_type, TRecord [], Some "record access")))
      )
  | RecordUpdate (record_expr, field_updates) -> (
      let record_type, subst1 = infer_expr env record_expr in
      match record_type with
      | TNamedRecord record_name ->
          let record_def = find_record_def record_name in
          let record_fields =
            List.map
              (fun field ->
                (field.field_name, convert_type_expr field.field_type))
              record_def.fields
          in
          let subst2 =
            List.fold_left
              (fun subst_acc (field_name, update_expr) ->
                let field_type =
                  try List.assoc field_name record_fields
                  with Not_found ->
                    raise
                      (TypeError
                         (RecordFieldMismatch
                            (field_name, record_name, List.map fst record_fields)))
                in
                let actual_type, expr_subst =
                  infer_expr (apply_subst_env subst_acc env) update_expr
                in
                let unify_subst = unify actual_type field_type in
                compose_subst (compose_subst subst_acc expr_subst) unify_subst)
              subst1 field_updates
          in
          (record_type, subst2)
      | TRecord fields ->
          let subst2 =
            List.fold_left
              (fun subst_acc (field_name, update_expr) ->
                let field_type =
                  try List.assoc field_name fields
                  with Not_found ->
                    raise
                      (TypeError
                         (RecordFieldMismatch
                            (field_name, "record", List.map fst fields)))
                in
                let actual_type, expr_subst =
                  infer_expr (apply_subst_env subst_acc env) update_expr
                in
                let unify_subst = unify actual_type field_type in
                compose_subst (compose_subst subst_acc expr_subst) unify_subst)
              subst1 field_updates
          in
          (record_type, subst2)
      | _ ->
          raise
            (TypeError
               (UnificationError (record_type, TRecord [], Some "record update")))
      )
  | MapCreate fields ->
      let key_type = fresh_type_var () in
      let value_type = fresh_type_var () in

      let subst =
        List.fold_left
          (fun subst_acc field ->
            match field with
            | AtomKeyField (_, value_expr) ->
                (* Atom keys are always atoms *)
                let value_typ, value_subst =
                  infer_expr (apply_subst_env subst_acc env) value_expr
                in
                let key_unify = unify (apply_subst subst_acc key_type) TAtom in
                let value_unify =
                  unify (apply_subst value_subst value_type) value_typ
                in
                compose_subst
                  (compose_subst subst_acc value_subst)
                  (compose_subst key_unify value_unify)
            | GeneralKeyField (key_expr, value_expr) ->
                let key_typ, key_subst =
                  infer_expr (apply_subst_env subst_acc env) key_expr
                in
                let value_typ, value_subst =
                  infer_expr (apply_subst_env key_subst env) value_expr
                in
                let key_unify =
                  unify (apply_subst key_subst key_type) key_typ
                in
                let value_unify =
                  unify (apply_subst value_subst value_type) value_typ
                in
                compose_subst
                  (compose_subst subst_acc key_subst)
                  (compose_subst value_subst
                     (compose_subst key_unify value_unify)))
          [] fields
      in

      (TMap (apply_subst subst key_type, apply_subst subst value_type), subst)
  | MapAccess (map_expr, key_expr) ->
      let map_type, map_subst = infer_expr env map_expr in
      let key_type, key_subst =
        infer_expr (apply_subst_env map_subst env) key_expr
      in

      (* Ensure it's a map type and key matches *)
      let value_type = fresh_type_var () in
      let expected_map_type = TMap (key_type, value_type) in
      let unify_subst =
        unify (apply_subst key_subst map_type) expected_map_type
      in
      let combined_subst =
        compose_subst (compose_subst map_subst key_subst) unify_subst
      in

      (apply_subst combined_subst value_type, combined_subst)
  | Receive (clauses, timeout_opt) ->
      (* Receive expressions can receive any type of message *)
      let result_type = fresh_type_var () in

      (* Type check all receive clauses *)
      let final_subst, _ =
        List.fold_left
          (fun (subst_acc, env_acc) (pattern, guard_opt, clause_expr) ->
            let _pattern_type, pattern_env, pattern_subst =
              infer_pattern env_acc pattern
            in
            let combined_subst = compose_subst subst_acc pattern_subst in
            let clause_env =
              pattern_env @ apply_subst_env combined_subst env_acc
            in

            (* Type check guard if present *)
            let guard_subst =
              match guard_opt with
              | Some guard -> infer_guard_expr clause_env guard
              | None -> []
            in
            let clause_env_with_guard =
              apply_subst_env guard_subst clause_env
            in

            (* Type check clause body *)
            let clause_type, clause_subst =
              infer_expr clause_env_with_guard clause_expr
            in

            (* Unify clause result with overall result type *)
            let unify_subst =
              unify (apply_subst clause_subst result_type) clause_type
            in

            let final_subst =
              compose_subst
                (compose_subst
                   (compose_subst combined_subst guard_subst)
                   clause_subst)
                unify_subst
            in
            (final_subst, apply_subst_env final_subst env_acc))
          ([], env) clauses
      in

      (* Type check timeout clause if present *)
      let timeout_subst =
        match timeout_opt with
        | Some (timeout_expr, timeout_body) ->
            (* Timeout expression should be integer (milliseconds) *)
            let timeout_type, timeout_s =
              infer_expr (apply_subst_env final_subst env) timeout_expr
            in
            let timeout_unify =
              unify (apply_subst timeout_s timeout_type) TInteger
            in
            let timeout_combined = compose_subst timeout_s timeout_unify in

            (* Timeout body should have same type as receive clauses *)
            let timeout_body_type, timeout_body_s =
              infer_expr
                (apply_subst_env
                   (compose_subst final_subst timeout_combined)
                   env)
                timeout_body
            in
            let timeout_body_unify =
              unify (apply_subst timeout_body_s result_type) timeout_body_type
            in

            compose_subst
              (compose_subst timeout_combined timeout_body_s)
              timeout_body_unify
        | None -> []
      in

      let complete_subst = compose_subst final_subst timeout_subst in
      (apply_subst complete_subst result_type, complete_subst)

(* Extract all variables from a pattern *)
let rec extract_pattern_vars (pattern : pattern) : string list =
  match pattern with
  | PVar name -> [ name ]
  | PWildcard -> []
  | PAtom _ -> []
  | PLiteral _ -> []
  | PTuple patterns -> List.concat (List.map extract_pattern_vars patterns)
  | PList patterns -> List.concat (List.map extract_pattern_vars patterns)
  | PCons (head, tail) -> extract_pattern_vars head @ extract_pattern_vars tail
  | PRecord (_, field_patterns) ->
      List.concat
        (List.map
           (fun (_, pattern) -> extract_pattern_vars pattern)
           field_patterns)
  | PMap pattern_fields ->
      List.concat
        (List.map
           (fun field ->
             match field with
             | AtomKeyPattern (_, pattern) -> extract_pattern_vars pattern
             | GeneralKeyPattern (_, pattern) -> extract_pattern_vars pattern)
           pattern_fields)

(* Type inference for function clauses *)
let infer_function_clause (env : type_env) (clause : function_clause) :
    lx_type * substitution =
  let param_types = List.map (fun _ -> fresh_type_var ()) clause.params in

  (* Type check each parameter pattern and extract variables *)
  let param_env, pattern_substs =
    List.fold_left2
      (fun (env_acc, subst_acc) pattern param_type ->
        let pattern_type, pattern_env, pattern_subst =
          infer_pattern env pattern
        in
        let unify_subst = unify pattern_type param_type in
        let combined_subst = compose_subst pattern_subst unify_subst in
        (pattern_env @ env_acc, compose_subst subst_acc combined_subst))
      ([], []) clause.params param_types
  in
  let new_env = param_env @ env in
  (* Type check guard if present *)
  let guard_subst =
    match clause.guard with
    | Some guard -> infer_guard_expr new_env guard
    | None -> []
  in
  let env_with_guard = apply_subst_env guard_subst new_env in
  let body_type, subst = infer_expr env_with_guard clause.body in
  let combined_subst =
    compose_subst (compose_subst pattern_substs guard_subst) subst
  in
  let fun_type =
    List.fold_right
      (fun param_type acc -> TFun (param_type, acc))
      param_types body_type
  in
  (fun_type, combined_subst)

(* Type inference for function definitions with multiple arities *)
let infer_function_def (env : type_env) (func : function_def) :
    lx_type * substitution =
  match func.clauses with
  | [] -> failwith ("Function " ^ func.name ^ " has no clauses")
  | [ clause ] ->
      (* Single clause - add function to environment for recursion *)
      let func_type_var = fresh_type_var () in
      let env_with_func = (func.name, func_type_var) :: env in
      let inferred_type, subst = infer_function_clause env_with_func clause in
      let unify_subst = unify (apply_subst subst func_type_var) inferred_type in
      let final_subst = compose_subst subst unify_subst in
      (apply_subst final_subst inferred_type, final_subst)
  | clauses ->
      (* Multiple clauses - add function to environment for recursion *)
      let func_type_var = fresh_type_var () in
      let env_with_func = (func.name, func_type_var) :: env in
      (* Multiple clauses - ensure they have compatible types *)
      let clause_types_and_substs =
        List.map (infer_function_clause env_with_func) clauses
      in
      let first_type, first_subst = List.hd clause_types_and_substs in

      (* Check that all clauses have compatible types *)
      let final_subst =
        List.fold_left
          (fun acc_subst (_clause_type, clause_subst) ->
            (* For now, we don't unify different arities - they're separate functions in Erlang *)
            compose_subst acc_subst clause_subst)
          first_subst
          (List.tl clause_types_and_substs)
      in

      (* Unify the function type variable with the inferred type *)
      let unify_subst =
        unify
          (apply_subst final_subst func_type_var)
          (apply_subst final_subst first_type)
      in
      let complete_subst = compose_subst final_subst unify_subst in

      (* Return the type of the first clause as representative *)
      (apply_subst complete_subst first_type, complete_subst)

(* Built-in environment with OTP types *)
let builtin_env : type_env =
  [
    ( "gen_server",
      TFun
        (TAtom, TFun (TAtom, TFun (TList TAtom, TFun (TList TAtom, TOtpState))))
    );
    ("logger", TFun (TString, TNil));
    ("io.format", TFun (TString, TNil));
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

  (* Add io.format with different arities *)
  let tv = TVar 1000 in
  (* Use a high number to avoid conflicts *)
  let env_with_io =
    ("io.format", TFun (TString, TFun (TList tv, TNil))) :: env
  in

  (* First pass: collect record definitions and function signatures *)
  (* Process records first so they're available when type checking functions *)
  List.iter
    (fun item ->
      match item with
      | RecordDef record_def -> add_record_def record_def
      | _ -> ())
    program.items;

  let env_with_functions =
    List.fold_left
      (fun env_acc item ->
        match item with
        | Function func ->
            let func_type, _ = infer_function_def env_acc func in
            (func.name, func_type) :: env_acc
        | _ -> env_acc)
      env_with_io program.items
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
            env_acc desc.tests
      | Application _ ->
          (* Application definitions don't affect type checking *)
          env_acc
      | RecordDef _ ->
          (* Record definitions already processed in first pass *)
          env_acc)
    env_with_functions program.items
