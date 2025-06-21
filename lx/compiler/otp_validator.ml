(* OTP Validator for LX Language *)
(* Validates OTP callback functions according to gen_server specifications *)

open Ast

(* OTP validation errors *)
type otp_error =
  | MissingRequiredCallback of string * string (* callback name, worker name *)
  | InvalidCallbackArity of
      string
      * string
      * int
      * int (* callback name, worker name, expected, actual *)
  | InvalidCallbackReturn of string * string (* callback name, worker name *)
  | InvalidWorkerName of string
  | InvalidSupervisorName of string
  | CircularDependency of string list
  | UnknownChild of string * string

exception OtpValidationError of otp_error

(* Pretty printing for OTP errors *)
let string_of_otp_error = function
  | MissingRequiredCallback (callback, worker) ->
      "Worker must implement function " ^ callback
      ^ " with parameter count equal to "
      ^ (match callback with "init" -> "1" | _ -> "unknown")
      ^ " in worker '" ^ worker ^ "'"
  | InvalidCallbackArity (callback, worker, expected, actual) ->
      "Function " ^ callback ^ " must have parameter count equal to "
      ^ string_of_int expected
      ^ (match callback with
        | "init" -> ": (Args)"
        | "handle_call" -> ": (Request, From, State)"
        | "handle_cast" -> ": (Request, State)"
        | "handle_info" -> ": (Info, State)"
        | "terminate" -> ": (Reason, State)"
        | "code_change" -> ": (OldVsn, State, Extra)"
        | "format_status" -> ": (Status)"
        | _ -> "")
      ^ " in worker '" ^ worker ^ "', but found " ^ string_of_int actual
      ^ " parameters"
  | InvalidCallbackReturn (callback, worker) ->
      "Function " ^ callback ^ " must return a tuple in worker '" ^ worker ^ "'"
  | InvalidWorkerName name ->
      "Invalid worker name: '" ^ name ^ "'. Worker names must be valid atoms"
  | InvalidSupervisorName name ->
      "Invalid supervisor name: '" ^ name
      ^ "'. Supervisor names must be valid atoms"
  | CircularDependency cycle ->
      "Circular dependency detected: " ^ String.concat " -> " cycle
  | UnknownChild (supervisor, child) ->
      "Unknown child '" ^ child ^ "' referenced in supervisor '" ^ supervisor
      ^ "'"

(* Check if an expression returns a tuple *)
let rec returns_tuple = function
  | Tuple _ -> true
  | Let (_, _, body) -> returns_tuple body
  | If (_, then_expr, Some else_expr) ->
      returns_tuple then_expr && returns_tuple else_expr
  | If (_, then_expr, None) -> returns_tuple then_expr
  | Match (_, cases) -> List.for_all (fun (_, expr) -> returns_tuple expr) cases
  | Sequence exprs -> (
      match List.rev exprs with [] -> false | last :: _ -> returns_tuple last)
  | _ -> false

(* Validate atom names (simplified - should match Erlang atom rules) *)
let is_valid_atom_name name =
  let len = String.length name in
  len > 0 && len <= 255
  && (name.[0] >= 'a' && name.[0] <= 'z')
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_' || c = '@')
       name

(* Get all OTP callback functions from a worker's function list *)
let get_worker_otp_callbacks (functions : function_def list) :
    (otp_callback * function_def) list =
  List.filter_map
    (fun (func : function_def) ->
      match otp_callback_of_string func.name with
      | Some callback -> Some (callback, func)
      | None -> None)
    functions

(* Validate a single OTP callback function *)
let validate_otp_callback (callback : otp_callback) (func : function_def)
    (worker_name : string) : unit =
  let callback_name : string = string_of_otp_callback callback in
  let expected_arity : int = expected_arity_for_callback callback in

  (* Check all clauses have the correct arity *)
  List.iter
    (fun (clause : function_clause) ->
      let actual_arity : int = List.length clause.params in
      if actual_arity <> expected_arity then
        raise
          (OtpValidationError
             (InvalidCallbackArity
                (callback_name, worker_name, expected_arity, actual_arity))))
    func.clauses;

  (* Check that all clauses return tuples (except format_status which can return anything) *)
  if callback <> FormatStatus then
    List.iter
      (fun (clause : function_clause) ->
        if not (returns_tuple clause.body) then
          raise
            (OtpValidationError
               (InvalidCallbackReturn (callback_name, worker_name))))
      func.clauses

(* Validate worker component *)
let validate_worker (worker : otp_component) =
  match worker with
  | Worker { name; functions; specs = _ } ->
      (* Validate worker name *)
      if not (is_valid_atom_name name) then
        raise (OtpValidationError (InvalidWorkerName name));

      (* Get all OTP callback functions *)
      let otp_callbacks = get_worker_otp_callbacks functions in

      (* Check that init is present *)
      let has_init =
        List.exists (fun (callback, _) -> callback = Init) otp_callbacks
      in
      if not has_init then
        raise (OtpValidationError (MissingRequiredCallback ("init", name)));

      (* Validate each OTP callback *)
      List.iter
        (fun (callback, func) -> validate_otp_callback callback func name)
        otp_callbacks
  | Supervisor _ -> failwith "validate_worker called with supervisor component"

(* Validate supervisor strategy *)
let validate_supervisor_strategy = function
  | OneForOne | OneForAll | RestForOne -> ()

(* Build dependency graph *)
let build_dependency_graph (components : otp_component list) :
    (string * string list) list =
  List.fold_left
    (fun acc component ->
      match component with
      | Worker { name; _ } -> (name, []) :: acc
      | Supervisor { name; children; _ } -> (name, children) :: acc)
    [] components

(* Detect circular dependencies using DFS *)
let detect_circular_dependencies (graph : (string * string list) list) =
  let rec dfs visited path node =
    if List.mem node path then
      raise (OtpValidationError (CircularDependency (path @ [ node ])));
    if List.mem node visited then visited
    else
      let children = try List.assoc node graph with Not_found -> [] in
      let new_path = path @ [ node ] in
      List.fold_left
        (fun acc child -> dfs acc new_path child)
        (node :: visited) children
  in

  let nodes = List.map fst graph in
  let _ =
    List.fold_left
      (fun visited node ->
        if List.mem node visited then visited else dfs visited [] node)
      [] nodes
  in
  ()

(* Validate supervisor component *)
let validate_supervisor (supervisor : otp_component)
    (all_components : otp_component list) =
  match supervisor with
  | Supervisor { name; strategy; children } ->
      (* Validate supervisor name *)
      if not (is_valid_atom_name name) then
        raise (OtpValidationError (InvalidSupervisorName name));

      (* Validate strategy *)
      validate_supervisor_strategy strategy;

      (* Check that all children exist *)
      let component_names =
        List.map
          (function
            | Worker { name; _ } -> name | Supervisor { name; _ } -> name)
          all_components
      in

      List.iter
        (fun child ->
          if not (List.mem child component_names) then
            raise (OtpValidationError (UnknownChild (name, child))))
        children
  | Worker _ -> failwith "validate_supervisor called with worker component"

(* Main validation function *)
let validate_otp_components (components : otp_component list) =
  (* Validate each component individually *)
  List.iter
    (function
      | Worker _ as w -> validate_worker w
      | Supervisor _ as s -> validate_supervisor s components)
    components;

  (* Check for circular dependencies *)
  let dependency_graph = build_dependency_graph components in
  detect_circular_dependencies dependency_graph

(* Validate a complete program *)
let validate_program (program : program) =
  let components =
    List.filter_map
      (function OtpComponent comp -> Some comp | _ -> None)
      program.items
  in

  validate_otp_components components
