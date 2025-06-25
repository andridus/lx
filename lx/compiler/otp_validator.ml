(* OTP Validator for LX Language *)
(* Validates OTP callback functions according to gen_server specifications *)

open Ast

(* OTP validation errors *)
type otp_error =
  | MissingRequiredCallback of
      string * string * string option (* callback name, worker name, filename *)
  | InvalidCallbackArity of
      string * string * int * int * position option * string option
    (* callback name, worker name, expected, actual, position, filename *)
  | InvalidCallbackReturn of
      string
      * string
      * position option
      * string option (* callback name, worker name, position, filename *)
  | InvalidWorkerName of string
  | InvalidSupervisorName of string
  | CircularDependency of string list
  | UnknownChild of string * string
  | AmbiguousChildReference of
      string
      * string
      * position option
      * string option (* supervisor, child, position, filename *)

exception OtpValidationError of otp_error

(* Helper function to format position *)
let format_position pos filename =
  let file_prefix =
    match filename with
    | None -> (
        match pos with
        | Some p when p.filename <> None -> (
            match p.filename with Some f -> f ^ ":" | None -> "")
        | _ -> "")
    | Some f -> f ^ ":"
  in
  match pos with
  | None -> if file_prefix <> "" then " in " ^ file_prefix else ""
  | Some p ->
      Printf.sprintf " at %sline %d, column %d" file_prefix p.line p.column

(* Helper function to get example for callback *)
let get_callback_example = function
  | "init" -> "fun init(args) { .{:ok, initial_state} }"
  | "handle_call" ->
      "fun handle_call(request, from, state) { .{:reply, response, new_state} }"
  | "handle_cast" ->
      "fun handle_cast(request, state) { .{:noreply, new_state} }"
  | "handle_info" -> "fun handle_info(info, state) { .{:noreply, state} }"
  | "terminate" -> "fun terminate(reason, state) { .{:ok} }"
  | "code_change" -> "fun code_change(old_vsn, state, extra) { .{:ok, state} }"
  | "format_status" -> "fun format_status(status) { status }"
  | _ -> ""

(* Pretty printing for OTP errors *)
let string_of_otp_error = function
  | MissingRequiredCallback (callback, worker, filename) ->
      let file_prefix = match filename with Some f -> f ^ ": " | None -> "" in
      let example = get_callback_example callback in
      Printf.sprintf
        "%sOTP Error: Worker '%s' is missing required callback function '%s'\n\
        \  Expected: %s"
        file_prefix worker callback example
  | InvalidCallbackArity (callback, worker, expected, actual, pos, filename) ->
      let file_prefix = match filename with Some f -> f ^ ":" | None -> "" in
      let position_info =
        match pos with
        | Some p -> Printf.sprintf "%s%d:%d: " file_prefix p.line p.column
        | None -> if file_prefix <> "" then file_prefix ^ " " else ""
      in
      let param_names =
        match callback with
        | "init" -> "(args)"
        | "handle_call" -> "(request, from, state)"
        | "handle_cast" -> "(request, state)"
        | "handle_info" -> "(info, state)"
        | "terminate" -> "(reason, state)"
        | "code_change" -> "(old_vsn, state, extra)"
        | "format_status" -> "(status)"
        | _ -> ""
      in
      let example = get_callback_example callback in
      Printf.sprintf
        "%sOTP Error: Function '%s' in worker '%s' has incorrect number of \
         parameters\n\
        \  Found: %d parameters\n\
        \  Expected: %d parameters %s\n\
        \  Correct syntax: %s"
        position_info callback worker actual expected param_names example
  | InvalidCallbackReturn (callback, worker, pos, filename) ->
      let file_prefix = match filename with Some f -> f ^ ":" | None -> "" in
      let position_info =
        match pos with
        | Some p -> Printf.sprintf "%s%d:%d: " file_prefix p.line p.column
        | None -> if file_prefix <> "" then file_prefix ^ " " else ""
      in
      let return_example =
        match callback with
        | "init" -> ".{:ok, initial_state} or .{:stop, reason}"
        | "handle_call" ->
            ".{:reply, response, new_state} or .{:noreply, new_state}"
        | "handle_cast" ->
            ".{:noreply, new_state} or .{:stop, reason, new_state}"
        | "handle_info" ->
            ".{:noreply, new_state} or .{:stop, reason, new_state}"
        | "terminate" -> ".{:ok} (any value is acceptable)"
        | "code_change" -> ".{:ok, new_state} or .{:error, reason}"
        | _ -> "a tuple"
      in
      Printf.sprintf
        "%sOTP Error: Function '%s' in worker '%s' must return a tuple\n\
        \  Expected return format: %s"
        position_info callback worker return_example
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
  | AmbiguousChildReference (supervisor, child, pos, filename) ->
      let file_prefix = match filename with Some f -> f ^ ":" | None -> "" in
      let position_info =
        match pos with
        | Some p -> Printf.sprintf "%s%d:%d: " file_prefix p.line p.column
        | None -> if file_prefix <> "" then file_prefix ^ " " else ""
      in
      Printf.sprintf
        "%sOTP Error: Ambiguous reference '%s' in supervisor '%s'\n\
        \  Problem: '%s' is used for both worker and supervisor components\n\
        \  Solution: Use typed children syntax to specify the component type:\n\
        \    children {\n\
        \      worker [%s]     # if referring to the worker\n\
        \      supervisor [%s] # if referring to the supervisor\n\
        \    }"
        position_info child supervisor child child child

(* Check if an expression returns a tuple *)
let rec returns_tuple = function
  | Tuple _ -> true
  | If (_, then_expr, Some (SimpleElse else_expr)) ->
      returns_tuple then_expr && returns_tuple else_expr
  | If (_, then_expr, Some (ClauseElse clauses)) ->
      returns_tuple then_expr
      && List.for_all (fun (_, _, expr) -> returns_tuple expr) clauses
  | If (_, then_expr, None) -> returns_tuple then_expr
  | With (_, success_body, else_branch) -> (
      let success_returns_tuple = returns_tuple success_body in
      match else_branch with
      | Some (SimpleElse else_expr) ->
          success_returns_tuple && returns_tuple else_expr
      | Some (ClauseElse clauses) ->
          success_returns_tuple
          && List.for_all (fun (_, _, expr) -> returns_tuple expr) clauses
      | None -> success_returns_tuple)
  | Match (_, cases) ->
      List.for_all (fun (_, _, expr) -> returns_tuple expr) cases
  | Receive (clauses, timeout_opt) ->
      (* Check if all receive clauses return tuples *)
      let clauses_return_tuples =
        List.for_all (fun (_, _, expr) -> returns_tuple expr) clauses
      in
      let timeout_returns_tuple =
        match timeout_opt with
        | Some (_, timeout_body) -> returns_tuple timeout_body
        | None -> true (* No timeout clause, so we only check receive clauses *)
      in
      clauses_return_tuples && timeout_returns_tuple
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
    (worker_name : string) (filename : string option) : unit =
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
                ( callback_name,
                  worker_name,
                  expected_arity,
                  actual_arity,
                  clause.position,
                  filename ))))
    func.clauses;

  (* Check that all clauses return tuples (except format_status which can return anything) *)
  if callback <> FormatStatus then
    List.iter
      (fun (clause : function_clause) ->
        if not (returns_tuple clause.body) then
          raise
            (OtpValidationError
               (InvalidCallbackReturn
                  (callback_name, worker_name, clause.position, filename))))
      func.clauses

(* Validate worker component *)
let validate_worker (worker : otp_component) (filename : string option) =
  match worker with
  | Worker { name; functions; specs = _; position = _ } ->
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
        raise
          (OtpValidationError (MissingRequiredCallback ("init", name, filename)));

      (* Validate each OTP callback *)
      List.iter
        (fun (callback, func) ->
          validate_otp_callback callback func name filename)
        otp_callbacks
  | Supervisor _ -> failwith "validate_worker called with supervisor component"

(* Validate supervisor strategy *)
let validate_supervisor_strategy = function
  | OneForOne | OneForAll | RestForOne -> ()

(* Helper function to extract children list from children_spec *)
let get_children_list = function
  | SimpleChildren children -> children
  | TypedChildren { workers; supervisors } -> workers @ supervisors

(* Build dependency graph with type-aware dependencies *)
let build_dependency_graph (components : otp_component list) :
    (string * string list) list =
  List.fold_left
    (fun acc component ->
      match component with
      | Worker { name; _ } -> ("worker:" ^ name, []) :: acc
      | Supervisor { name; children; _ } ->
          let supervisor_name =
            match name with
            | Some n -> "supervisor:" ^ n
            | None -> "supervisor:main_supervisor"
          in
          (* Build dependencies based on children specification *)
          let dependencies =
            match children with
            | SimpleChildren child_list ->
                (* For simple children, we assume they could be either type *)
                (* Check what type they actually are *)
                List.map
                  (fun child ->
                    let is_worker =
                      List.exists
                        (function
                          | Worker { name; _ } -> name = child | _ -> false)
                        components
                    in
                    let is_supervisor =
                      List.exists
                        (function
                          | Supervisor { name = Some n; _ } -> n = child
                          | _ -> false)
                        components
                    in
                    if is_worker then "worker:" ^ child
                    else if is_supervisor then "supervisor:" ^ child
                    else
                      "unknown:" ^ child (* This will be caught by validation *))
                  child_list
            | TypedChildren { workers; supervisors } ->
                (* For typed children, we know exactly what type each dependency is *)
                List.map (fun w -> "worker:" ^ w) workers
                @ List.map (fun s -> "supervisor:" ^ s) supervisors
          in
          (supervisor_name, dependencies) :: acc)
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
    (all_components : otp_component list) (filename : string option) =
  match supervisor with
  | Supervisor { name; strategy; children; position } -> (
      (* Validate supervisor name if it exists *)
      (match name with
      | Some n ->
          if not (is_valid_atom_name n) then
            raise (OtpValidationError (InvalidSupervisorName n))
      | None -> () (* Anonymous supervisors are always valid *));

      (* Validate strategy *)
      validate_supervisor_strategy strategy;

      (* Check that all children exist with proper type checking *)
      let supervisor_name_for_error =
        match name with Some n -> n | None -> "main supervisor"
      in

      (* Validate children based on their specification type *)
      match children with
      | SimpleChildren child_list ->
          (* For simple children, check they exist as any component *)
          let component_names =
            List.map
              (function
                | Worker { name; _ } -> name
                | Supervisor { name; _ } -> (
                    match name with Some n -> n | None -> "main_supervisor"))
              all_components
          in

          (* Check for ambiguous names (same name used for both worker and supervisor) *)
          List.iter
            (fun child ->
              let is_worker =
                List.exists
                  (function Worker { name; _ } -> name = child | _ -> false)
                  all_components
              in
              let is_supervisor =
                List.exists
                  (function
                    | Supervisor { name = Some n; _ } -> n = child | _ -> false)
                  all_components
              in

              if is_worker && is_supervisor then
                (* Ambiguous reference - both worker and supervisor exist with same name *)
                raise
                  (OtpValidationError
                     (AmbiguousChildReference
                        (supervisor_name_for_error, child, position, filename)))
              else if not (List.mem child component_names) then
                raise
                  (OtpValidationError
                     (UnknownChild (supervisor_name_for_error, child))))
            child_list
      | TypedChildren { workers; supervisors } ->
          (* For typed children, validate each type separately *)
          let worker_names =
            List.filter_map
              (function Worker { name; _ } -> Some name | _ -> None)
              all_components
          in
          let supervisor_names =
            List.filter_map
              (function
                | Supervisor { name = Some n; _ } -> Some n
                | Supervisor { name = None; _ } -> Some "main_supervisor"
                | _ -> None)
              all_components
          in

          (* Validate worker children *)
          List.iter
            (fun worker ->
              if not (List.mem worker worker_names) then
                raise
                  (OtpValidationError
                     (UnknownChild
                        (supervisor_name_for_error, "worker " ^ worker))))
            workers;

          (* Validate supervisor children *)
          List.iter
            (fun sup ->
              if not (List.mem sup supervisor_names) then
                raise
                  (OtpValidationError
                     (UnknownChild
                        (supervisor_name_for_error, "supervisor " ^ sup))))
            supervisors)
  | Worker _ -> failwith "validate_supervisor called with worker component"

(* Main validation function *)
let validate_otp_components (components : otp_component list)
    (filename : string option) =
  (* Validate each component individually *)
  List.iter
    (function
      | Worker _ as w -> validate_worker w filename
      | Supervisor _ as s -> validate_supervisor s components filename)
    components;

  (* Check for circular dependencies *)
  let dependency_graph = build_dependency_graph components in
  detect_circular_dependencies dependency_graph

(* Validate a complete program *)
let validate_program (program : program) (filename : string option) =
  let components =
    List.filter_map
      (function OtpComponent comp -> Some comp | _ -> None)
      program.items
  in

  validate_otp_components components filename
