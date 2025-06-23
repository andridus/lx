(* Linter module for LX language
   Performs static analysis and validation before compilation *)

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

(* Context for tracking variables and scopes *)
type lint_context = {
  defined_vars : (string, Error.position option) Hashtbl.t;
  used_vars : (string, Error.position option) Hashtbl.t;
  parent_scope : lint_context option;
  current_function : string option;
}

let create_context parent =
  {
    defined_vars = Hashtbl.create 16;
    used_vars = Hashtbl.create 16;
    parent_scope = parent;
    current_function =
      (match parent with Some p -> p.current_function | None -> None);
  }

let create_root_context () = create_context None

(* Helper functions *)
let string_of_lint_error_kind = function
  | UnusedVariable (var, _) ->
      Printf.sprintf "Variable '%s' is defined but never used" var
  | UndefinedVariable (var, _) ->
      Printf.sprintf "Variable '%s' is used but not defined" var
  | UnreachableClause (func, clause_idx) ->
      Printf.sprintf "Clause %d in function '%s' is unreachable" clause_idx func
  | OverlappingClause (func, clause1, clause2) ->
      Printf.sprintf
        "Clauses %d and %d in function '%s' have overlapping patterns" clause1
        clause2 func
  | InvalidImport module_name ->
      Printf.sprintf "Invalid import: '%s'" module_name
  | InvalidExport func_name -> Printf.sprintf "Invalid export: '%s'" func_name
  | UndefinedFunction (name, arity, _) ->
      Printf.sprintf "Call to undefined function '%s/%d'" name arity
  | InvalidRecordDefinition name ->
      Printf.sprintf "Invalid record definition: '%s'" name
  | InvalidTypeDefinition name ->
      Printf.sprintf "Invalid type definition: '%s'" name
  | InvalidMacroDefinition name ->
      Printf.sprintf "Invalid macro definition: '%s'" name
  | IncompatibleReturnType (func, expected, found) ->
      Printf.sprintf "Function '%s' returns '%s' but expected '%s'" func found
        expected
  | VariableShadowing (var, _, _) ->
      Printf.sprintf "Variable '%s' shadows a variable from parent scope" var
  | MissingOtpCallback (worker_name, callback_name) ->
      Printf.sprintf "Worker '%s' is missing required OTP callback '%s'"
        worker_name callback_name
  | InvalidOtpCallback
      (worker_name, callback_name, found_arity, expected_arities) ->
      Printf.sprintf "Worker '%s' has callback '%s/%d' but expected arity %s"
        worker_name callback_name found_arity expected_arities
  | UnusedFunction (func_name, arity) ->
      Printf.sprintf "Function '%s/%d' is defined but never used" func_name
        arity
  | UnusedExternalCall (module_name, func_name) ->
      Printf.sprintf "External call '%s.%s' is used but may be incorrect"
        module_name func_name
  | UnusedLiteral (literal_value, _) ->
      Printf.sprintf "Literal value '%s' is constructed but never used"
        literal_value

let make_lint_suggestion = function
  | UnusedVariable (var, _) ->
      Some
        (Printf.sprintf
           "Consider removing '%s' or prefix with '_' to mark as intentionally \
            unused"
           var)
  | UndefinedVariable (var, _) ->
      Some
        (Printf.sprintf "Define '%s' before using it, or check for typos" var)
  | UnreachableClause (func, _) ->
      Some
        (Printf.sprintf
           "Remove the unreachable clause or reorder clauses in '%s'" func)
  | OverlappingClause (func, _, _) ->
      Some
        (Printf.sprintf "Make patterns in '%s' more specific or reorder clauses"
           func)
  | UndefinedFunction (name, arity, _) ->
      Some
        (Printf.sprintf
           "Define function '%s/%d' or check the function name and arity" name
           arity)
  | VariableShadowing (var, _, _) ->
      Some
        (Printf.sprintf "Use a different name like '%s_local' or 'inner_%s'" var
           var)
  | MissingOtpCallback (worker_name, callback_name) ->
      Some
        (Printf.sprintf "Add the missing '%s' function to worker '%s'"
           callback_name worker_name)
  | InvalidOtpCallback (_, callback_name, _, expected_arities) ->
      Some
        (Printf.sprintf "Fix callback signature: %s should have arity %s"
           callback_name expected_arities)
  | UnusedFunction (func_name, arity) ->
      Some
        (Printf.sprintf
           "Remove function '%s/%d' or export it with 'pub %s(...)' if it's \
            meant to be used externally"
           func_name arity func_name)
  | UnusedExternalCall (module_name, func_name) ->
      Some
        (Printf.sprintf "Check if module '%s' and function '%s' exist"
           module_name func_name)
  | UnusedLiteral (_, _) ->
      Some
        "Remove the unused literal or assign it to a variable if needed for \
         side effects"
  | _ -> None

let create_lint_error kind position severity =
  {
    kind;
    position;
    message = string_of_lint_error_kind kind;
    suggestion = make_lint_suggestion kind;
    severity;
  }

(* Variable tracking functions *)
let define_variable ctx var_name position =
  if Hashtbl.mem ctx.defined_vars var_name then
    let first_pos = Hashtbl.find ctx.defined_vars var_name in
    Some
      (create_lint_error
         (VariableShadowing (var_name, position, first_pos))
         (match position with
         | Some p -> p
         | None -> { line = 0; column = 0; filename = None })
         `Warning)
  else (
    Hashtbl.replace ctx.defined_vars var_name position;
    None)

let use_variable ctx var_name position =
  Hashtbl.replace ctx.used_vars var_name position

let rec lookup_variable ctx var_name =
  if Hashtbl.mem ctx.defined_vars var_name then true
  else
    match ctx.parent_scope with
    | Some parent -> lookup_variable parent var_name
    | None -> false

let is_ignored_var var_name = String.length var_name > 0 && var_name.[0] = '_'

(* Convert Ast.position to Error.position *)
let convert_position = function
  | None -> None
  | Some ast_pos ->
      Some
        {
          Error.line = ast_pos.line;
          Error.column = ast_pos.column;
          Error.filename = ast_pos.filename;
        }

(* OTP callback validation - callbacks are optional but must have correct arity if defined *)
let valid_gen_server_callbacks =
  [
    ("init", 1);
    ("handle_call", 3);
    ("handle_cast", 2);
    ("handle_info", 2);
    ("handle_continue", 2);
    ("terminate", 2);
    ("code_change", 3);
    ("format_status", 1);
    ("format_status", 2);
    (* format_status can have 1 or 2 arguments *)
  ]

(* OTP callback names for stricter validation *)
let otp_callback_names =
  [
    "init";
    "handle_call";
    "handle_cast";
    "handle_info";
    "handle_continue";
    "terminate";
    "code_change";
    "format_status";
  ]

let check_otp_callbacks worker_name (functions : function_def list) =
  let implemented_callbacks =
    List.fold_left
      (fun acc (func : function_def) ->
        List.fold_left
          (fun acc2 clause ->
            let arity = List.length clause.params in
            (func.name, arity, func.position) :: acc2)
          acc func.clauses)
      [] functions
  in

  (* Check if implemented callbacks have correct arity *)
  List.fold_left
    (fun errors (func_name, arity, position) ->
      (* Check if this function is a known OTP callback *)
      let is_otp_callback =
        List.exists
          (fun (cb_name, _) -> cb_name = func_name)
          valid_gen_server_callbacks
      in

      if is_otp_callback then
        (* Check if the arity is valid for this callback *)
        let valid_arities =
          List.fold_left
            (fun acc (cb_name, cb_arity) ->
              if cb_name = func_name then cb_arity :: acc else acc)
            [] valid_gen_server_callbacks
        in

        if not (List.mem arity valid_arities) then
          let expected_arities =
            String.concat " or " (List.map string_of_int valid_arities)
          in
          let error_position =
            match convert_position position with
            | Some pos -> pos
            | None -> { line = 0; column = 0; filename = None }
          in
          let error =
            create_lint_error
              (InvalidOtpCallback
                 (worker_name, func_name, arity, expected_arities))
              error_position `Error
          in
          error :: errors
        else errors
      else errors)
    [] implemented_callbacks

(* Check for unused functions *)
let check_unused_functions (functions : function_def list) =
  let all_functions =
    List.fold_left
      (fun acc (func : function_def) ->
        List.fold_left
          (fun acc2 clause ->
            let arity = List.length clause.params in
            (func.name, arity, func.visibility, func.position) :: acc2)
          acc func.clauses)
      [] functions
  in

  let called_functions = ref [] in

  (* Simple heuristic: assume init, handle_* functions are called by OTP *)
  let starts_with s prefix =
    let len_s = String.length s in
    let len_prefix = String.length prefix in
    len_s >= len_prefix && String.sub s 0 len_prefix = prefix
  in

  let otp_called_functions =
    List.filter
      (fun (name, _, _, _) ->
        starts_with name "init" || starts_with name "handle_"
        || starts_with name "terminate"
        || starts_with name "code_change")
      all_functions
  in

  called_functions := otp_called_functions @ !called_functions;

  (* Check for unused functions *)
  List.fold_left
    (fun errors (func_name, arity, visibility, position) ->
      let is_called =
        List.exists
          (fun (called_name, called_arity, _, _) ->
            called_name = func_name && called_arity = arity)
          !called_functions
      in

      let is_public = visibility = Public in

      (* Only flag private functions that are not called *)
      if (not is_called) && not is_public then
        let error_position =
          match convert_position position with
          | Some pos -> pos
          | None -> { line = 0; column = 0; filename = None }
        in
        let error =
          create_lint_error
            (UnusedFunction (func_name, arity))
            error_position `Warning
        in
        error :: errors
      else errors)
    [] all_functions

(* Pattern analysis *)
let rec lint_pattern ctx errors pattern =
  match pattern with
  | PVar var_name when not (is_ignored_var var_name) -> (
      let pos = None in
      (* TODO: Add position tracking to patterns *)
      match define_variable ctx var_name pos with
      | Some error -> error :: errors
      | None -> errors)
  | PTuple patterns | PList patterns ->
      List.fold_left (lint_pattern ctx) errors patterns
  | PCons (head, tail) ->
      let errors = lint_pattern ctx errors head in
      lint_pattern ctx errors tail
  | _ -> errors

(* Check for unused variables in a context *)
let check_unused_variables ctx =
  let errors = ref [] in
  let is_otp_callback =
    match ctx.current_function with
    | Some func_name -> List.mem func_name otp_callback_names
    | None -> false
  in

  Hashtbl.iter
    (fun var_name def_pos ->
      if
        (not (Hashtbl.mem ctx.used_vars var_name))
        && not (is_ignored_var var_name)
      then
        (* Variables in OTP callbacks should be errors, others warnings *)
        let severity = if is_otp_callback then `Error else `Warning in
        let error =
          create_lint_error
            (UnusedVariable (var_name, def_pos))
            (match def_pos with
            | Some p -> p
            | None -> { line = 0; column = 0; filename = None })
            severity
        in
        errors := error :: !errors)
    ctx.defined_vars;
  !errors

(* Expression analysis *)
let rec lint_expr ctx errors expr =
  match expr with
  | Var var_name ->
      use_variable ctx var_name None;
      (* Ignore Erlang macros and built-in variables *)
      let is_erlang_macro = String.length var_name > 0 && var_name.[0] = '?' in
      let is_erlang_builtin = List.mem var_name [ "gen_server"; "io" ] in

      if
        (not (lookup_variable ctx var_name))
        && (not (is_ignored_var var_name))
        && (not is_erlang_macro) && not is_erlang_builtin
      then
        let error =
          create_lint_error
            (UndefinedVariable (var_name, None))
            { line = 0; column = 0; filename = None }
            `Error
        in
        error :: errors
      else errors
  | Assign (var_name, value_expr, position) -> (
      let errors = lint_expr ctx errors value_expr in
      (* Check if assigning a literal to an ignored variable *)
      let errors =
        if is_ignored_var var_name then
          match value_expr with
          | Literal l ->
              let literal_str =
                match l with
                | LString s -> "\"" ^ s ^ "\""
                | LInt n -> string_of_int n
                | LFloat f -> string_of_float f
                | LBool true -> "true"
                | LBool false -> "false"
                | LAtom a -> a
                | LNil -> "nil"
              in
              let error =
                create_lint_error
                  (UnusedLiteral (literal_str, convert_position position))
                  (match convert_position position with
                  | Some p -> p
                  | None -> { line = 0; column = 0; filename = None })
                  `Error
              in
              error :: errors
          | _ -> errors
        else errors
      in
      match define_variable ctx var_name (convert_position position) with
      | Some error -> error :: errors
      | None -> errors)
  | App (func_expr, args) ->
      let errors = lint_expr ctx errors func_expr in
      List.fold_left (lint_expr ctx) errors args
  | ExternalCall (_module_name, _func_name, args) ->
      (* Check if the external call is valid *)
      List.fold_left (lint_expr ctx) errors args
  | Fun (params, body) ->
      let func_ctx = create_context (Some ctx) in
      let errors =
        List.fold_left
          (fun acc param ->
            match define_variable func_ctx param None with
            | Some error -> error :: acc
            | None -> acc)
          errors params
      in
      lint_expr func_ctx errors body
  | Tuple exprs | List exprs | Sequence exprs | Block exprs ->
      List.fold_left (lint_expr ctx) errors exprs
  | Match (expr, cases) ->
      let errors = lint_expr ctx errors expr in
      List.fold_left
        (fun acc (pattern, guard_opt, case_expr) ->
          let case_ctx = create_context (Some ctx) in
          let acc = lint_pattern case_ctx acc pattern in
          let acc = match guard_opt with
            | Some guard -> lint_guard_expr case_ctx acc guard
            | None -> acc
          in
          let acc = lint_expr case_ctx acc case_expr in
          (* Check for unused variables in this case *)
          let unused_errors = check_unused_variables case_ctx in
          acc @ unused_errors)
        errors cases
  | If (cond, then_expr, else_expr) -> (
      let errors = lint_expr ctx errors cond in
      let errors = lint_expr ctx errors then_expr in
      match else_expr with Some e -> lint_expr ctx errors e | None -> errors)
  | For (var, iter_expr, body_expr) ->
      let errors = lint_expr ctx errors iter_expr in
      let loop_ctx = create_context (Some ctx) in
      let errors =
        match define_variable loop_ctx var None with
        | Some error -> error :: errors
        | None -> errors
      in
      lint_expr loop_ctx errors body_expr
  | BinOp (left, _, right) ->
      let errors = lint_expr ctx errors left in
      lint_expr ctx errors right
  | UnaryOp (_, operand) ->
      lint_expr ctx errors operand
  | Send (target, message) ->
      let errors = lint_expr ctx errors target in
      lint_expr ctx errors message
  | _ -> errors

(* Guard expression analysis *)
and lint_guard_expr ctx errors guard =
  match guard with
  | GuardAnd (g1, g2) | GuardOr (g1, g2) | GuardAndalso (g1, g2) | GuardOrelse (g1, g2) ->
      let errors = lint_guard_expr ctx errors g1 in
      lint_guard_expr ctx errors g2
  | GuardNot g ->
      lint_guard_expr ctx errors g
  | GuardBinOp (left, _, right) ->
      let errors = lint_guard_value ctx errors left in
      lint_guard_value ctx errors right
  | GuardCall (func, args) ->
      (* Validate guard function names *)
      let errors = match func with
        | "is_atom" | "is_integer" | "is_float" | "is_number"
        | "is_boolean" | "is_list" | "is_tuple" | "abs" | "round" | "trunc"
        | "hd" | "tl" | "length" | "element" ->
            errors
        | _ ->
            let error = create_lint_error
              (UndefinedVariable (func, None))
              { line = 0; column = 0; filename = None }
              `Error
            in
            error :: errors
      in
      List.fold_left (lint_guard_value ctx) errors args
  | GuardAtom atom ->
      lint_guard_atom ctx errors atom

and lint_guard_atom ctx errors atom =
  match atom with
  | GuardVar var_name ->
      use_variable ctx var_name None;
      if not (lookup_variable ctx var_name) && not (is_ignored_var var_name) then
        let error = create_lint_error
          (UndefinedVariable (var_name, None))
          { line = 0; column = 0; filename = None }
          `Error
        in
        error :: errors
      else errors
  | GuardLiteral _ -> errors
  | GuardCallAtom (func, args) ->
      (* Validate guard function names *)
      let errors = match func with
        | "is_atom" | "is_integer" | "is_float" | "is_number"
        | "is_boolean" | "is_list" | "is_tuple" | "abs" | "round" | "trunc"
        | "hd" | "tl" | "length" | "element" ->
            errors
        | _ ->
            let error = create_lint_error
              (UndefinedVariable (func, None))
              { line = 0; column = 0; filename = None }
              `Error
            in
            error :: errors
      in
      List.fold_left (lint_guard_atom ctx) errors args

and lint_guard_value ctx errors value =
  match value with
  | GuardAtomValue atom -> lint_guard_atom ctx errors atom
  | GuardCallValue (func, args) ->
      (* Validate guard function names *)
      let errors = match func with
        | "is_atom" | "is_integer" | "is_float" | "is_number"
        | "is_boolean" | "is_list" | "is_tuple" | "abs" | "round" | "trunc"
        | "hd" | "tl" | "length" | "element" ->
            errors
        | _ ->
            let error = create_lint_error
              (UndefinedVariable (func, None))
              { line = 0; column = 0; filename = None }
              `Error
            in
            error :: errors
      in
      List.fold_left (lint_guard_value ctx) errors args

(* Function definition analysis *)
let lint_function_def ctx (func_def : function_def) =
  let errors = ref [] in
  let func_ctx = { ctx with current_function = Some func_def.name } in

  (* Lint each clause *)
  List.iteri
    (fun _clause_idx clause ->
      let clause_ctx = create_context (Some func_ctx) in
      let clause_errors =
        List.fold_left (lint_pattern clause_ctx) [] clause.params
      in
      let clause_errors = match clause.guard with
        | Some guard -> lint_guard_expr clause_ctx clause_errors guard
        | None -> clause_errors
      in
      let clause_errors = lint_expr clause_ctx clause_errors clause.body in
      (* Check for unused variables in this clause *)
      let unused_errors = check_unused_variables clause_ctx in
      errors := clause_errors @ unused_errors @ !errors)
    func_def.clauses;

  !errors

(* Main linting functions *)
let lint_module_item ctx item =
  match item with
  | Function func_def ->
      let func_ctx = create_context (Some ctx) in
      let func_errors = lint_function_def func_ctx func_def in
      func_errors
      (* unused variables are now checked within lint_function_def *)
  | OtpComponent (Worker { name; functions; _ }) ->
      let func_errors =
        List.fold_left
          (fun acc func_def ->
            let func_ctx = create_context (Some ctx) in
            let func_errors = lint_function_def func_ctx func_def in
            acc @ func_errors
            (* unused variables are now checked within lint_function_def *))
          [] functions
      in

      (* Check for missing OTP callbacks *)
      let callback_errors = check_otp_callbacks name functions in

      (* Check for unused functions *)
      let unused_func_errors = check_unused_functions functions in

      func_errors @ callback_errors @ unused_func_errors
  | OtpComponent (Supervisor _) -> [] (* Supervisors don't have much to lint *)
  | Spec _ | Test _ | Application _ ->
      [] (* These don't need variable linting *)

let lint_program program =
  let root_ctx = create_root_context () in

  let all_errors =
    List.fold_left
      (fun acc item ->
        let item_errors = lint_module_item root_ctx item in
        acc @ item_errors)
      [] program.items
  in

  (* Treat all lint issues as errors - no warnings allowed *)
  let all_lint_errors = all_errors in

  if List.length all_lint_errors > 0 then raise (LintError all_lint_errors)
  else Printf.printf "Linting completed successfully (no issues found)\n"

(* Public interface *)
let string_of_lint_error error =
  let pos_str =
    match error.position.filename with
    | Some f ->
        Printf.sprintf "%s:%d:%d" f error.position.line error.position.column
    | None -> Printf.sprintf "%d:%d" error.position.line error.position.column
  in
  let base_msg = Printf.sprintf "%s: Error: %s" pos_str error.message in
  match error.suggestion with
  | Some suggestion -> base_msg ^ "\n  Suggestion: " ^ suggestion
  | None -> base_msg

let string_of_lint_errors errors =
  String.concat "\n" (List.map string_of_lint_error errors)
