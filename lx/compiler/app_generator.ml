open Ast

(* Variable renaming context for scope simulation - copied from compiler.ml *)
type rename_context = {
  scope_hash : string;
  var_map : (string, string) Hashtbl.t;
  parent : rename_context option;
  used_hashes : (string, bool) Hashtbl.t;
  (* Track record types for variables *)
  var_record_types : (string, string) Hashtbl.t;
}

(* Helper functions copied from compiler.ml *)
let generate_random_hash () =
  let chars = "abcdefghijklmnopqrstuvwxyz0123456789" in
  let len = String.length chars in
  let hash_len = 3 in
  let result = Bytes.create hash_len in
  for i = 0 to hash_len - 1 do
    let idx = Random.int len in
    Bytes.set result i chars.[idx]
  done;
  Bytes.to_string result

let rec generate_unique_hash used_hashes =
  let hash = generate_random_hash () in
  if Hashtbl.mem used_hashes hash then generate_unique_hash used_hashes
  else (
    Hashtbl.replace used_hashes hash true;
    hash)

let create_scope parent =
  let parent_used_hashes =
    match parent with Some p -> p.used_hashes | None -> Hashtbl.create 16
  in
  let scope_hash = generate_unique_hash parent_used_hashes in
  {
    scope_hash;
    var_map = Hashtbl.create 16;
    parent;
    used_hashes =
      (match parent with Some p -> p.used_hashes | None -> Hashtbl.create 16);
    var_record_types = Hashtbl.create 16;
  }

let capitalize_var id =
  if String.length id > 0 then
    String.uppercase_ascii (String.sub id 0 1)
    ^ String.sub id 1 (String.length id - 1)
  else id

let is_ignored_var var_name = String.length var_name > 0 && var_name.[0] = '_'

let get_renamed_var ctx var_name =
  if is_ignored_var var_name then "_"
  else
    let rec lookup ctx =
      match Hashtbl.find_opt ctx.var_map var_name with
      | Some renamed -> renamed
      | None -> (
          match ctx.parent with
          | Some parent_ctx -> lookup parent_ctx
          | None -> capitalize_var var_name)
    in
    lookup ctx

let add_var_to_scope ctx var_name =
  if is_ignored_var var_name then (
    let renamed = "_" in
    Hashtbl.replace ctx.var_map var_name renamed;
    renamed)
  else
    let renamed = capitalize_var var_name ^ "_" ^ ctx.scope_hash in
    Hashtbl.replace ctx.var_map var_name renamed;
    renamed

(* Track the record type for a variable *)
let track_var_record_type ctx var_name record_type =
  if not (is_ignored_var var_name) then
    Hashtbl.replace ctx.var_record_types var_name record_type

(* Get the record type for a variable *)
let get_var_record_type ctx var_name =
  let rec lookup ctx =
    match Hashtbl.find_opt ctx.var_record_types var_name with
    | Some record_type -> Some record_type
    | None -> (
        match ctx.parent with
        | Some parent_ctx -> lookup parent_ctx
        | None -> None)
  in
  lookup ctx

let emit_literal (l : literal) : string =
  match l with
  | LInt n -> string_of_int n
  | LFloat f ->
      let s = string_of_float f in
      if String.ends_with ~suffix:"." s then s ^ "0"
      else if not (String.contains s '.') then s ^ ".0"
      else s
  | LString s -> "\"" ^ s ^ "\""
  | LBool true -> "true"
  | LBool false -> "false"
  | LAtom a -> a
  | LNil -> "nil"

(* Helper function to determine the record type name from an expression *)
let rec get_record_type_name ctx (expr : expr) : string =
  match expr with
  | Var var_name -> (
      (* First try to get the tracked record type *)
      match get_var_record_type ctx var_name with
      | Some record_type -> String.lowercase_ascii record_type
      | None -> (
          if
            (* Special case: if the variable is "record", it's a keyword used as variable *)
            var_name = "record"
          then
            (* For the keyword "record" used as variable, we can't infer the type from the name *)
            (* This should have been tracked when the variable was assigned *)
            "record" (* fallback - this indicates the tracking failed *)
          else
            (* Fallback: try to extract record type from variable name pattern *)
            let renamed_var = get_renamed_var ctx var_name in
            let parts = String.split_on_char '_' renamed_var in
            match parts with
            | record_name :: _
              when String.length record_name > 0
                   && String.get record_name 0 >= 'A'
                   && String.get record_name 0 <= 'Z' ->
                String.lowercase_ascii record_name
            | _ -> "record" (* fallback *)))
  | RecordCreate (record_name, _) -> String.lowercase_ascii record_name
  | RecordAccess (inner_expr, _) -> get_record_type_name ctx inner_expr
  | RecordUpdate (inner_expr, _) -> get_record_type_name ctx inner_expr
  | _ -> "record" (* fallback for unknown expressions *)

and emit_pattern ctx (p : pattern) : string =
  match p with
  | PWildcard -> "_"
  | PVar id -> if is_ignored_var id then "_" else get_renamed_var ctx id
  | PAtom a -> a
  | PLiteral l -> emit_literal l
  | PTuple ps -> "{" ^ String.concat ", " (List.map (emit_pattern ctx) ps) ^ "}"
  | PList ps -> "[" ^ String.concat ", " (List.map (emit_pattern ctx) ps) ^ "]"
  | PCons (head, tail) ->
      "[" ^ emit_pattern ctx head ^ " | " ^ emit_pattern ctx tail ^ "]"
  | PRecord (record_name, field_patterns) ->
      let record_name_lower = String.lowercase_ascii record_name in
      let fields_str =
        String.concat ", "
          (List.map
             (fun (field_name, field_pattern) ->
               field_name ^ " = " ^ emit_pattern ctx field_pattern)
             field_patterns)
      in
      "#" ^ record_name_lower ^ "{" ^ fields_str ^ "}"
  | PMap pattern_fields ->
      let emit_map_pattern_field_local ctx = function
        | AtomKeyPattern (key, pattern) ->
            key ^ " := " ^ emit_pattern ctx pattern
        | GeneralKeyPattern (key_expr, pattern) ->
            emit_expr ctx key_expr ^ " := " ^ emit_pattern ctx pattern
      in
      let fields_str =
        String.concat ", "
          (List.map (emit_map_pattern_field_local ctx) pattern_fields)
      in
      "#{" ^ fields_str ^ "}"
  | PBinary pattern_elements ->
      let emit_binary_pattern_element ctx = function
        | SimpleBinaryPattern pattern -> emit_pattern ctx pattern
        | SizedBinaryPattern (pattern, size_expr, spec_opt) ->
            let size_str = emit_expr ctx size_expr in
            let spec_str =
              match spec_opt with
              | Some spec -> "/" ^ emit_binary_spec spec
              | None -> ""
            in
            emit_pattern ctx pattern ^ ":" ^ size_str ^ spec_str
        | TypedBinaryPattern (pattern, spec) ->
            emit_pattern ctx pattern ^ "/" ^ emit_binary_spec spec
      in
      let elements_str =
        String.concat ", "
          (List.map (emit_binary_pattern_element ctx) pattern_elements)
      in
      "<<" ^ elements_str ^ ">>"

and emit_expr ctx (e : expr) : string =
  match e with
  | Literal l -> emit_literal l
  | Var id -> get_renamed_var ctx id
  | Assign (id, value, _pos) ->
      if is_ignored_var id then emit_expr ctx value
      else
        let renamed = add_var_to_scope ctx id in
        (* Track record type if the value is a record *)
        (match value with
        | RecordCreate (record_name, _) ->
            track_var_record_type ctx id record_name
        | RecordUpdate (inner_expr, _) ->
            (* When updating a record, inherit the type from the source *)
            let source_type = get_record_type_name ctx inner_expr in
            if source_type <> "record" then
              track_var_record_type ctx id source_type
        | _ -> ());
        renamed ^ " = " ^ emit_expr ctx value
  | PatternMatch (pattern, value, _pos, _unsafe) ->
      (* Generate Erlang pattern matching: pattern = value *)
      emit_pattern ctx pattern ^ " = " ^ emit_expr ctx value
  | Fun (params, body) ->
      let fun_ctx = create_scope (Some ctx) in
      let renamed_params = List.map (add_var_to_scope fun_ctx) params in
      "fun("
      ^ String.concat ", " renamed_params
      ^ ") -> " ^ emit_expr fun_ctx body ^ " end"
  | App (Var func_name, args) ->
      func_name ^ "(" ^ String.concat ", " (List.map (emit_expr ctx) args) ^ ")"
  | App (func, args) ->
      emit_expr ctx func ^ "("
      ^ String.concat ", " (List.map (emit_expr ctx) args)
      ^ ")"
  | ExternalCall (module_name, func_name, args, _pos_opt) ->
      module_name ^ ":" ^ func_name ^ "("
      ^ String.concat ", " (List.map (emit_expr ctx) args)
      ^ ")"
  | Tuple exprs ->
      "{" ^ String.concat ", " (List.map (emit_expr ctx) exprs) ^ "}"
  | List exprs ->
      "[" ^ String.concat ", " (List.map (emit_expr ctx) exprs) ^ "]"
  | If (cond, then_expr, else_branch) ->
      "case " ^ emit_expr ctx cond ^ " of true -> " ^ emit_expr ctx then_expr
      ^ (match else_branch with
        | Some (SimpleElse e) -> "; _ -> " ^ emit_expr ctx e
        | Some (ClauseElse clauses) ->
            "; _ -> case true of "
            ^ String.concat "; "
                (List.map
                   (fun (p, guard_opt, e) ->
                     let guard_str =
                       match guard_opt with
                       | Some guard -> " when " ^ emit_guard_expr ctx guard
                       | None -> ""
                     in
                     emit_pattern ctx p ^ guard_str ^ " -> " ^ emit_expr ctx e)
                   clauses)
            ^ " end"
        | None -> "; _ -> nil")
      ^ " end"
  | With (steps, success_body, else_branch) ->
      (* Generate nested case expressions for with steps *)
      let rec emit_with_steps remaining_steps =
        match remaining_steps with
        | [] -> emit_expr ctx success_body
        | (pattern, expr) :: rest ->
            "case " ^ emit_expr ctx expr ^ " of " ^ emit_pattern ctx pattern
            ^ " -> " ^ emit_with_steps rest ^ "; _ -> "
            ^ (match else_branch with
              | Some (SimpleElse e) -> emit_expr ctx e
              | Some (ClauseElse clauses) ->
                  "case true of "
                  ^ String.concat "; "
                      (List.map
                         (fun (p, guard_opt, e) ->
                           let guard_str =
                             match guard_opt with
                             | Some guard ->
                                 " when " ^ emit_guard_expr ctx guard
                             | None -> ""
                           in
                           emit_pattern ctx p ^ guard_str ^ " -> "
                           ^ emit_expr ctx e)
                         clauses)
                  ^ " end"
              | None -> "nil")
            ^ " end"
      in
      emit_with_steps steps
  | Match (value, cases) ->
      "case " ^ emit_expr ctx value ^ " of "
      ^ String.concat "; "
          (List.map
             (fun (p, _guard_opt, e) ->
               emit_pattern ctx p ^ " -> " ^ emit_expr ctx e)
             cases)
      ^ " end"
  | For (_, _, _) -> "% For expressions not yet implemented"
  | Sequence exprs ->
      let block_ctx = create_scope (Some ctx) in
      String.concat ",\n    " (List.map (emit_expr block_ctx) exprs)
  | Block exprs ->
      let block_ctx = create_scope (Some ctx) in
      String.concat ",\n    " (List.map (emit_expr block_ctx) exprs)
  | UnaryOp (op, operand) -> op ^ " " ^ emit_expr ctx operand
  | BinOp (left, op, right) ->
      emit_expr ctx left ^ " " ^ op ^ " " ^ emit_expr ctx right
  | Send (target, message) ->
      emit_expr ctx target ^ " ! " ^ emit_expr ctx message
  | RecordCreate (record_name, field_inits) ->
      let record_name_lower = String.lowercase_ascii record_name in
      let fields_str =
        String.concat ", "
          (List.map
             (fun (field_name, field_expr) ->
               field_name ^ " = " ^ emit_expr ctx field_expr)
             field_inits)
      in
      "#" ^ record_name_lower ^ "{" ^ fields_str ^ "}"
  | RecordAccess (record_expr, field_name) ->
      (* Try to infer the record type from the expression *)
      let record_type_name = get_record_type_name ctx record_expr in
      emit_expr ctx record_expr ^ "#" ^ record_type_name ^ "." ^ field_name
  | RecordUpdate (record_expr, field_updates) ->
      let updates_str =
        String.concat ", "
          (List.map
             (fun (field_name, update_expr) ->
               field_name ^ " = " ^ emit_expr ctx update_expr)
             field_updates)
      in
      let record_type_name = get_record_type_name ctx record_expr in
      emit_expr ctx record_expr ^ "#" ^ record_type_name ^ "{" ^ updates_str
      ^ "}"
  | MapCreate fields ->
      let emit_map_field_local ctx = function
        | AtomKeyField (key, value) -> key ^ " => " ^ emit_expr ctx value
        | GeneralKeyField (key, value) ->
            emit_expr ctx key ^ " => " ^ emit_expr ctx value
      in
      let fields_str =
        String.concat ", " (List.map (emit_map_field_local ctx) fields)
      in
      "#{" ^ fields_str ^ "}"
  | MapAccess (map_expr, key_expr) ->
      "maps:get(" ^ emit_expr ctx key_expr ^ ", " ^ emit_expr ctx map_expr ^ ")"
  | BinaryCreate elements ->
      let emit_binary_element ctx = function
        | SimpleBinaryElement expr -> emit_expr ctx expr
        | SizedBinaryElement (expr, size_expr, spec_opt) ->
            let size_str = emit_expr ctx size_expr in
            let spec_str =
              match spec_opt with
              | Some spec -> "/" ^ emit_binary_spec spec
              | None -> ""
            in
            emit_expr ctx expr ^ ":" ^ size_str ^ spec_str
        | TypedBinaryElement (expr, spec) ->
            emit_expr ctx expr ^ "/" ^ emit_binary_spec spec
      in
      let elements_str =
        String.concat ", " (List.map (emit_binary_element ctx) elements)
      in
      "<<" ^ elements_str ^ ">>"
  | Receive (clauses, timeout_opt) -> (
      let clauses_str =
        String.concat ";\n    " (List.map (emit_receive_clause ctx) clauses)
      in
      let receive_body = "receive\n    " ^ clauses_str in
      match timeout_opt with
      | Some (timeout_expr, timeout_body) ->
          receive_body ^ "\nafter\n    " ^ emit_expr ctx timeout_expr
          ^ " ->\n        " ^ emit_expr ctx timeout_body ^ "\nend"
      | None -> receive_body ^ "\nend")
  (* Fun expression types *)
  | FunExpression (params, body) ->
      let fun_ctx = create_scope (Some ctx) in
      let renamed_params = List.map (add_var_to_scope fun_ctx) params in
      "fun("
      ^ String.concat ", " renamed_params
      ^ ") -> " ^ emit_expr fun_ctx body ^ " end"
  | FunExpressionClauses clauses ->
      let emit_fun_clause ctx (params, guard_opt, body) =
        let clause_ctx = create_scope (Some ctx) in
        let renamed_params = List.map (add_var_to_scope clause_ctx) params in
        let params_str = String.concat ", " renamed_params in
        let guard_str =
          match guard_opt with
          | Some guard -> " when " ^ emit_guard_expr ctx guard
          | None -> ""
        in
        let body_str = emit_expr clause_ctx body in
        "(" ^ params_str ^ ")" ^ guard_str ^ " -> " ^ body_str
      in
      let clauses_str =
        String.concat ";\n    " (List.map (emit_fun_clause ctx) clauses)
      in
      "fun " ^ clauses_str ^ "\nend"

(* Helper function to emit receive clauses *)
and emit_receive_clause ctx (pattern, guard_opt, body) =
  let pattern_str = emit_pattern ctx pattern in
  let guard_str =
    match guard_opt with
    | Some guard -> " when " ^ emit_guard_expr ctx guard
    | None -> ""
  in
  let body_str = emit_expr ctx body in
  pattern_str ^ guard_str ^ " -> " ^ body_str

(* Guard expression emission *)
and emit_guard_expr ctx (guard : guard_expr) : string =
  match guard with
  | GuardAnd (g1, g2) -> emit_guard_expr ctx g1 ^ ", " ^ emit_guard_expr ctx g2
  | GuardOr (g1, g2) -> emit_guard_expr ctx g1 ^ "; " ^ emit_guard_expr ctx g2
  | GuardAndalso (g1, g2) ->
      emit_guard_expr ctx g1 ^ " andalso " ^ emit_guard_expr ctx g2
  | GuardOrelse (g1, g2) ->
      emit_guard_expr ctx g1 ^ " orelse " ^ emit_guard_expr ctx g2
  | GuardNot g -> "not " ^ emit_guard_expr ctx g
  | GuardBinOp (left, op, right) ->
      let erlang_op =
        match op with "!=" -> "/=" | "<=" -> "=<" | other -> other
      in
      emit_guard_value ctx left ^ " " ^ erlang_op ^ " "
      ^ emit_guard_value ctx right
  | GuardCall (func, args) ->
      let erlang_func =
        match func with
        | "is_atom" -> "is_atom"
        | "is_integer" -> "is_integer"
        | "is_float" -> "is_float"
        | "is_number" -> "is_number"
        | "is_boolean" -> "is_boolean"
        | "is_list" -> "is_list"
        | "is_tuple" -> "is_tuple"
        | other -> other
      in
      erlang_func ^ "("
      ^ String.concat ", " (List.map (emit_guard_value ctx) args)
      ^ ")"
  | GuardAtom atom -> emit_guard_atom ctx atom

and emit_guard_atom ctx (atom : guard_atom) : string =
  match atom with
  | GuardVar var -> get_renamed_var ctx var
  | GuardLiteral lit -> emit_literal lit
  | GuardCallAtom (func, args) ->
      func ^ "("
      ^ String.concat ", " (List.map (emit_guard_atom ctx) args)
      ^ ")"

and emit_guard_value ctx (value : guard_value) : string =
  match value with
  | GuardAtomValue atom -> emit_guard_atom ctx atom
  | GuardCallValue (func, args) ->
      let erlang_func =
        match func with
        | "is_atom" -> "is_atom"
        | "is_integer" -> "is_integer"
        | "is_float" -> "is_float"
        | "is_number" -> "is_number"
        | "is_boolean" -> "is_boolean"
        | "is_list" -> "is_list"
        | "is_tuple" -> "is_tuple"
        | other -> other
      in
      erlang_func ^ "("
      ^ String.concat ", " (List.map (emit_guard_value ctx) args)
      ^ ")"

(* Helper function to emit binary specifications *)
and emit_binary_spec = function
  | BinaryType typ -> typ
  | BinaryTypeWithEndian (typ, endian) -> typ ^ "-" ^ endian

(* Helper function to extract application name from filename *)
let extract_app_name filename =
  let basename = Filename.basename filename in
  let name_without_ext = Filename.remove_extension basename in
  (* Convert to lowercase and replace hyphens with underscores *)
  let normalized =
    String.map (function '-' -> '_' | c -> c) name_without_ext
  in
  String.lowercase_ascii normalized

(* Helper function to extract children list from children_spec *)
let get_children_list = function
  | SimpleChildren children -> children
  | TypedChildren { workers; supervisors } -> workers @ supervisors

(* Helper function to collect all modules from a program *)
let collect_modules program app_name =
  let modules = ref [] in

  (* Add the application module and supervisor module *)
  modules := (app_name ^ "_app") :: !modules;
  modules := (app_name ^ "_supervisor") :: !modules;

  (* Collect modules from OTP components and functions *)
  List.iter
    (function
      | Function { name = _; _ } ->
          (* Regular functions become part of the main module or separate modules *)
          ()
      | OtpComponent (Worker { name; _ }) ->
          (* Workers get app prefix and _worker suffix *)
          modules := (app_name ^ "_" ^ name ^ "_worker") :: !modules
      | OtpComponent (Supervisor { name; _ }) ->
          (* Supervisors get app prefix with _supervisor suffix *)
          let supervisor_name =
            match name with
            | Some n -> app_name ^ "_" ^ n ^ "_supervisor"
            | None ->
                app_name
                ^ "_supervisor" (* Anonymous supervisor gets _supervisor *)
          in
          if name <> None then modules := supervisor_name :: !modules
      | _ -> ())
    program.items;

  List.rev !modules

(* Helper function to collect registered processes *)
let collect_registered program app_name =
  let registered = ref [] in
  List.iter
    (function
      | OtpComponent (Supervisor { name; _ }) ->
          let supervisor_name =
            match name with
            | Some n -> app_name ^ "_" ^ n ^ "_supervisor"
            | None -> app_name ^ "_supervisor"
          in
          registered := supervisor_name :: !registered
      | _ -> ())
    program.items;
  List.rev !registered

(* Generate .app.src file content *)
let generate_app_src app_name program app_def =
  let modules = collect_modules program app_name in
  let auto_registered = collect_registered program app_name in

  (* Merge auto-detected registered with user-defined *)
  let all_registered =
    match app_def.registered with
    | None -> auto_registered
    | Some user_regs ->
        user_regs @ auto_registered |> List.sort_uniq String.compare
  in

  let applications =
    match app_def.applications with
    | None -> [ "kernel"; "stdlib" ]
    | Some apps -> apps
  in

  let modules_str = String.concat ", " modules in
  let registered_str = String.concat ", " all_registered in
  let applications_str = String.concat ", " applications in

  Printf.sprintf
    "{application, %s, [\n\
    \  {description, \"%s\"},\n\
    \  {vsn, \"%s\"},\n\
    \  {modules, [%s]},\n\
    \  {registered, [%s]},\n\
    \  {mod, {%s_app, []}},\n\
    \  {applications, [%s]},\n\
    \  {env, []}\n\
     ]}.\n"
    app_name app_def.description app_def.vsn modules_str registered_str app_name
    applications_str

(* Generate rebar.config file content *)
let generate_rebar_config () =
  "{erl_opts, [debug_info]}.\n\
   {deps, []}.\n\n\
   {applications, [kernel, stdlib]}.\n\n\
   {project_plugins, []}.\n\
   {sub_dirs, []}.\n"

(* Generate application module content *)
let generate_app_module app_name =
  Printf.sprintf
    "-module(%s_app).\n\
     -behaviour(application).\n\
     -export([start/2, stop/1]).\n\n\
     start(_Type, _Args) ->\n\
    \    %s_supervisor:start_link().\n\n\
     stop(_State) ->\n\
    \    ok.\n"
    app_name app_name

(* Generate supervisor module content *)
let generate_supervisor_module app_name program =
  (* Find the anonymous supervisor (main supervisor) from the program *)
  let main_supervisor = ref None in
  List.iter
    (function
      | OtpComponent (Supervisor { name = None; strategy; children; _ }) ->
          (* Use the anonymous supervisor as main supervisor *)
          main_supervisor := Some (strategy, children)
      | _ -> ())
    program.items;

  match !main_supervisor with
  | Some (strategy, children_spec) ->
      let strategy_str =
        match strategy with
        | OneForOne -> "one_for_one"
        | OneForAll -> "one_for_all"
        | RestForOne -> "rest_for_one"
      in
      let children = get_children_list children_spec in
      let children_specs =
        List.map
          (fun child ->
            (* Check if we have typed children information *)
            let is_worker_typed, is_supervisor_typed =
              match children_spec with
              | TypedChildren { workers; supervisors } ->
                  (List.mem child workers, List.mem child supervisors)
              | SimpleChildren _ -> (false, false)
              (* Need to infer from components *)
            in

            (* If we have typed information, use it; otherwise infer from components *)
            let is_worker =
              is_worker_typed
              || (not is_supervisor_typed)
                 && List.exists
                      (function
                        | OtpComponent (Worker { name; _ }) -> name = child
                        | _ -> false)
                      program.items
            in

            let is_supervisor =
              is_supervisor_typed
              || (not is_worker_typed)
                 && List.exists
                      (function
                        | OtpComponent (Supervisor { name = Some n; _ }) ->
                            n = child
                        | _ -> false)
                      program.items
            in

            if is_worker then
              let child_module = app_name ^ "_" ^ child ^ "_worker" in
              Printf.sprintf
                "    {%s, {%s, start_link, []}, permanent, 5000, worker, [%s]}"
                child_module child_module child_module
            else if is_supervisor then
              let child_module = app_name ^ "_" ^ child ^ "_supervisor" in
              Printf.sprintf
                "    {%s, {%s, start_link, []}, permanent, 5000, supervisor, \
                 [%s]}"
                child_module child_module child_module
            else
              (* Unknown child - this should be caught by validation *)
              let child_module = app_name ^ "_" ^ child in
              Printf.sprintf
                "    {%s, {%s, start_link, []}, permanent, 5000, worker, [%s]}"
                child_module child_module child_module)
          children
      in
      let children_str = String.concat ",\n" children_specs in

      Printf.sprintf
        "-module(%s_supervisor).\n\
         -behaviour(supervisor).\n\
         -export([start_link/0, init/1]).\n\n\
         start_link() ->\n\
        \    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n\
         init([]) ->\n\
        \    Children = [\n\
         %s\n\
        \    ],\n\
        \    {ok, {#{strategy => %s, intensity => 10, period => 60}, Children}}.\n"
        app_name children_str strategy_str
  | None ->
      (* Generate a default supervisor if no anonymous supervisor found *)
      Printf.sprintf
        "-module(%s_supervisor).\n\
         -behaviour(supervisor).\n\
         -export([start_link/0, init/1]).\n\n\
         start_link() ->\n\
        \    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n\
         init([]) ->\n\
        \    Children = [],\n\
        \    {ok, {#{strategy => one_for_one, intensity => 10, period => 60}, \
         Children}}.\n"
        app_name

(* Generate test suite content *)
let generate_test_suite app_name =
  Printf.sprintf
    "-module(%s_SUITE).\n\
     -export([all/0, basic_test/1]).\n\
     -include_lib(\"common_test/include/ct.hrl\").\n\n\
     all() ->\n\
    \    [basic_test].\n\n\
     basic_test(_Config) ->\n\
    \    ok.\n"
    app_name

(* Generate record definitions *)
let generate_record_definitions program =
  let record_defs =
    List.fold_left
      (fun acc item ->
        match item with RecordDef record_def -> record_def :: acc | _ -> acc)
      [] program.items
  in

  String.concat "\n"
    (List.map
       (fun record_def ->
         let record_name_lower =
           String.lowercase_ascii record_def.record_name
         in
         let fields_str =
           String.concat ", "
             (List.map
                (fun field ->
                  let default_str =
                    match field.default_value with
                    | Some expr ->
                        let ctx = create_scope None in
                        " = " ^ emit_expr ctx expr
                    | None -> ""
                  in
                  field.field_name ^ default_str)
                record_def.fields)
         in
         "-record(" ^ record_name_lower ^ ", {" ^ fields_str ^ "}).")
       record_defs)

(* Generate worker module content *)
let generate_worker_module app_name worker_name functions program =
  let module_name = app_name ^ "_" ^ worker_name ^ "_worker" in

  (* Generate record definitions for this module *)
  let record_definitions = generate_record_definitions program in

  (* Collect OTP callback functions and public functions *)
  let otp_callbacks = ref [] in
  let public_functions = ref [] in

  List.iter
    (fun (func : function_def) ->
      if is_otp_callback_name func.name then
        otp_callbacks := func.name :: !otp_callbacks
      else if func.visibility = Public then
        (* Calculate arity for public functions *)
        let arity =
          match func.clauses with
          | [] -> 0
          | clause :: _ -> List.length clause.params
        in
        public_functions :=
          (func.name ^ "/" ^ string_of_int arity) :: !public_functions)
    functions;

  (* Always include start_link and ALL required OTP callbacks *)
  let required_otp_exports =
    [
      "start_link/0";
      "init/1";
      "handle_call/3";
      "handle_cast/2";
      "handle_info/2";
      "terminate/2";
      "code_change/3";
    ]
  in

  let implemented_otp_exports =
    List.map
      (fun name ->
        let arity =
          match name with
          | "init" -> 1
          | "handle_call" -> 3
          | "handle_cast" -> 2
          | "handle_info" -> 2
          | "terminate" -> 2
          | "code_change" -> 3
          | "format_status" -> 1
          | _ -> 0
        in
        name ^ "/" ^ string_of_int arity)
      !otp_callbacks
  in

  let otp_exports =
    List.sort_uniq String.compare
      (required_otp_exports @ implemented_otp_exports)
  in

  let all_exports = otp_exports @ !public_functions in
  (* Remove duplicates from exports *)
  let unique_exports = List.sort_uniq String.compare all_exports in
  let exports_str = String.concat ", " unique_exports in

  let header =
    let record_section =
      if record_definitions = "" then "" else record_definitions ^ "\n\n"
    in
    Printf.sprintf
      "-module(%s).\n\
       -behaviour(gen_server).\n\
       -export([%s]).\n\n\
       %sstart_link() ->\n\
      \    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n"
      module_name exports_str record_section
  in

  (* Group functions by name to handle multiple clauses *)
  let group_functions_by_name (functions : function_def list) :
      (string * function_def list) list =
    let grouped = Hashtbl.create 16 in
    List.iter
      (fun (func : function_def) ->
        let existing =
          try Hashtbl.find grouped func.name with Not_found -> []
        in
        Hashtbl.replace grouped func.name (func :: existing))
      functions;
    Hashtbl.fold
      (fun name funcs acc -> (name, List.rev funcs) :: acc)
      grouped []
  in

  (* Generate function implementation with all clauses *)
  let emit_function_with_clauses (name, func_list) =
    match func_list with
    | [] -> Printf.sprintf "%s() ->\n    ok." name
    | funcs ->
        let emit_clause func =
          match func.clauses with
          | [] -> Printf.sprintf "%s() ->\n    ok" name
          | clause :: _ ->
              (* Generate function body using real compilation logic *)
              let ctx = create_scope None in
              (* Add pattern variables to scope *)
              List.iter
                (function
                  | PVar name -> ignore (add_var_to_scope ctx name) | _ -> ())
                clause.params;
              let pattern_strings = List.map (emit_pattern ctx) clause.params in
              name ^ "("
              ^ String.concat ", " pattern_strings
              ^ ") ->\n    " ^ emit_expr ctx clause.body
        in

        let clauses = List.map emit_clause funcs in
        String.concat ";\n\n" clauses ^ "."
  in

  let grouped_functions = group_functions_by_name functions in
  let function_impls = List.map emit_function_with_clauses grouped_functions in

  (* Generate default implementations for missing OTP callbacks *)
  let required_callbacks =
    [
      ("init", "init(_Args) ->\n    error(not_implemented).");
      ( "handle_call",
        "handle_call(_Request, _From, _State) ->\n    error(not_implemented)."
      );
      ( "handle_cast",
        "handle_cast(_Msg, _State) ->\n    error(not_implemented)." );
      ( "handle_info",
        "handle_info(_Info, _State) ->\n    error(not_implemented)." );
      ("terminate", "terminate(_Reason, _State) ->\n    ok.");
      ("code_change", "code_change(_OldVsn, State, _Extra) ->\n    {ok, State}.");
    ]
  in

  let implemented_callback_names = List.map fst grouped_functions in

  let default_implementations =
    List.fold_left
      (fun acc (callback_name, default_impl) ->
        if not (List.mem callback_name implemented_callback_names) then
          default_impl :: acc
        else acc)
      [] required_callbacks
  in

  let all_implementations = function_impls @ default_implementations in

  header ^ "\n" ^ String.concat "\n\n" all_implementations

(* Generate named supervisor module content *)
let generate_named_supervisor_module app_name supervisor_name strategy children
    =
  let module_name = app_name ^ "_" ^ supervisor_name ^ "_supervisor" in
  let header =
    Printf.sprintf
      "-module(%s).\n\
       -behaviour(supervisor).\n\
       -export([start_link/0, init/1]).\n\n\
       start_link() ->\n\
      \    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n"
      module_name
  in

  let strategy_str =
    match strategy with
    | OneForOne -> "one_for_one"
    | OneForAll -> "one_for_all"
    | RestForOne -> "rest_for_one"
  in

  let children_specs =
    List.map
      (fun child ->
        let child_module = app_name ^ "_" ^ child ^ "_worker" in
        Printf.sprintf
          "    {%s, {%s, start_link, []}, permanent, 5000, worker, [%s]}"
          child_module child_module child_module)
      (get_children_list children)
  in

  let supervisor_impl =
    Printf.sprintf
      "init([]) ->\n\
      \    Children = [\n\
       %s\n\
      \    ],\n\
      \    {ok, {#{strategy => %s, intensity => 10, period => 60}, Children}}."
      (String.concat ",\n" children_specs)
      strategy_str
  in

  header ^ "\n" ^ supervisor_impl

(* Clean up old files and directories *)
let cleanup_old_files output_dir app_name =
  let project_dir = Filename.concat output_dir app_name in

  (* Remove old project directory completely if it exists *)
  (if Sys.file_exists project_dir then
     let rec remove_dir dir =
       try
         if Sys.is_directory dir then (
           let files = Sys.readdir dir in
           Array.iter
             (fun file ->
               let full_path = Filename.concat dir file in
               remove_dir full_path)
             files;
           Unix.rmdir dir)
         else Sys.remove dir
       with
       | Sys_error _ ->
           (* Silently ignore Sys_error - these are usually "file not found" errors *)
           ()
       | Unix.Unix_error _ -> ()
       | exn ->
           Printf.eprintf "Warning: Unexpected error removing %s: %s\n" dir
             (Printexc.to_string exn)
     in

     (* Try to remove using system command as fallback *)
     let try_system_remove () =
       let cmd = Printf.sprintf "rm -rf %s" (Filename.quote project_dir) in
       let status = Unix.system cmd in
       match status with
       | WEXITED 0 ->
           (* Suppress success message *)
           true
       | _ -> false
     in

     (* First try OCaml removal, then fallback to system command *)
     try remove_dir project_dir (* Suppress success message *)
     with _ ->
       if not (try_system_remove ()) then
         Printf.eprintf
           "Warning: Could not completely remove old project directory: %s\n"
           project_dir);

  (* Also remove any .erl files in the output directory that match the app name pattern *)
  let app_pattern = app_name ^ "_" in
  let main_erl = app_name ^ ".erl" in

  if Sys.file_exists output_dir && Sys.is_directory output_dir then
    let files = Sys.readdir output_dir in
    Array.iter
      (fun file ->
        if
          String.ends_with ~suffix:".erl" file
          && (String.starts_with ~prefix:app_pattern file || file = main_erl)
        then
          let full_path = Filename.concat output_dir file in
          if Sys.file_exists full_path then (
            Sys.remove full_path;
            Printf.printf "Removed old file: %s\n" full_path))
      files

(* Main function to generate all application files *)
let generate_application_files ?(skip_rebar = false) output_dir filename program
    app_def =
  (* Type checking phase - must pass before any generation *)
  (try
     let _ = Typechecker.type_check_program program in
     ()
   with
  | Typechecker.TypeError error ->
      (* Create a compilation error to stop the process immediately *)
      let error_obj =
        match error with
        | Typechecker.PatternMatchError (_, _, Some pos, msg) ->
            (* For pattern match errors, use the special PatternMatchError type *)
            {
              Error.kind = Error.PatternMatchError msg;
              Error.position = pos;
              Error.message = msg;
              Error.suggestion = None;
              Error.context = None;
            }
        | _ ->
            Error.make_error_with_position
              (Error.TypeError ("Type checking failed", None))
              (Error.make_position 1 1)
              (Typechecker.string_of_type_error error)
      in
      raise (Error.CompilationError error_obj)
  | Error.CompilationError _ as e -> raise e);

  (* Linting phase - must pass before any generation *)
  (try Linter.lint_program program
   with Linter.LintError errors ->
     Printf.eprintf "Lint Errors:\n%s\n" (Linter.string_of_lint_errors errors);
     failwith "Linting failed - application generation aborted");

  let app_name = extract_app_name filename in

  (* Clean up old files before generating new ones *)
  cleanup_old_files output_dir app_name;

  (* Create project directory structure *)
  let project_dir = Filename.concat output_dir app_name in
  let src_dir = Filename.concat project_dir "src" in
  let test_dir = Filename.concat project_dir "test" in

  (* Create directories if they don't exist *)
  if not (Sys.file_exists project_dir) then Unix.mkdir project_dir 0o755;
  if not (Sys.file_exists src_dir) then Unix.mkdir src_dir 0o755;
  if not (Sys.file_exists test_dir) then Unix.mkdir test_dir 0o755;

  (* Generate .app.src file *)
  let app_src_content = generate_app_src app_name program app_def in
  let app_src_file = Filename.concat src_dir (app_name ^ ".app.src") in
  let oc = open_out app_src_file in
  output_string oc app_src_content;
  close_out oc;

  (* Generate rebar.config file *)
  let rebar_config_content = generate_rebar_config () in
  let rebar_config_file = Filename.concat project_dir "rebar.config" in
  let oc = open_out rebar_config_file in
  output_string oc rebar_config_content;
  close_out oc;

  (* Generate application module *)
  let app_module_content = generate_app_module app_name in
  let app_module_file = Filename.concat src_dir (app_name ^ "_app.erl") in
  let oc = open_out app_module_file in
  output_string oc app_module_content;
  close_out oc;

  (* Generate supervisor module *)
  let sup_module_content = generate_supervisor_module app_name program in
  let sup_module_file =
    Filename.concat src_dir (app_name ^ "_supervisor.erl")
  in
  let oc = open_out sup_module_file in
  output_string oc sup_module_content;
  close_out oc;

  (* Generate worker and supervisor modules directly *)
  List.iter
    (function
      | OtpComponent (Worker { name; functions; _ }) ->
          let module_name = app_name ^ "_" ^ name ^ "_worker" in
          let worker_content =
            generate_worker_module app_name name functions program
          in
          let dest_file = Filename.concat src_dir (module_name ^ ".erl") in
          let oc = open_out dest_file in
          output_string oc worker_content;
          close_out oc
      | OtpComponent (Supervisor { name; strategy; children; _ }) -> (
          (* Only generate named supervisors, anonymous ones are handled by the main supervisor *)
          match name with
          | Some n ->
              let module_name = app_name ^ "_" ^ n ^ "_supervisor" in
              let supervisor_content =
                generate_named_supervisor_module app_name n strategy children
              in
              let dest_file = Filename.concat src_dir (module_name ^ ".erl") in
              let oc = open_out dest_file in
              output_string oc supervisor_content;
              close_out oc
          | None -> () (* Anonymous supervisor is handled by main supervisor *))
      | _ -> ())
    program.items;

  (* Generate test suite *)
  let test_suite_content = generate_test_suite app_name in
  let test_suite_file = Filename.concat test_dir (app_name ^ "_SUITE.erl") in
  let oc = open_out test_suite_file in
  output_string oc test_suite_content;
  close_out oc;

  (* Remove main module file if it exists (since we organize everything in src/) *)
  let main_module_file = Filename.concat output_dir (app_name ^ ".erl") in
  if Sys.file_exists main_module_file then Sys.remove main_module_file;

  (* Suppress generated structure output *)
  (* Printf.printf "Generated OTP application structure:\n";
  Printf.printf "  - %s/\n" project_dir;
  Printf.printf "    ├── rebar.config\n";
  Printf.printf "    ├── src/\n";
  Printf.printf "    │   ├── %s.app.src\n" app_name;
  Printf.printf "    │   ├── %s_app.erl\n" app_name;
  Printf.printf "    │   ├── %s_supervisor.erl\n" app_name;
  List.iter (function
    | OtpComponent (Worker { name; _ }) ->
        Printf.printf "    │   ├── %s_%s_worker.erl\n" app_name name
    | OtpComponent (Supervisor { name; _ }) ->
        (match name with
          | Some n -> Printf.printf "    │   ├── %s_%s_supervisor.erl\n" app_name n
          | None -> () (* Anonymous supervisor is the main supervisor *)
        )
    | _ -> ()
  ) program.items;
  Printf.printf "    └── test/\n";
  Printf.printf "        └── %s_SUITE.erl\n" app_name; *)

  (* Compile the generated project with rebar3 only if not skipped *)
  if not skip_rebar then (
    Printf.printf "\n";
    try Rebar_manager.compile_project project_dir with
    | Error.CompilationError _ as e -> raise e
    | exn ->
        Printf.eprintf "Warning: Failed to compile with rebar3: %s\n"
          (Printexc.to_string exn);
        Printf.eprintf
          "You can manually compile the project by running 'rebar3 compile' in \
           %s\n"
          project_dir)

(* Find application definition in the program *)
let find_application_def program =
  let rec find_app = function
    | [] -> None
    | Application app_def :: _ -> Some app_def
    | _ :: rest -> find_app rest
  in
  find_app program.items

(* Get all module names from OTP components *)
let get_all_modules (program : program) (app_name : string) : string list =
  let otp_modules =
    List.fold_left
      (fun acc item ->
        match item with
        | OtpComponent (Worker { name; _ }) ->
            (app_name ^ "_" ^ name ^ "_worker") :: acc
        | OtpComponent (Supervisor { name = Some name; _ }) ->
            (app_name ^ "_" ^ name ^ "_supervisor") :: acc
        | OtpComponent (Supervisor { name = None; _ }) ->
            (app_name ^ "_supervisor") :: acc
        | _ -> acc)
      [] program.items
  in
  otp_modules

(* Get registered processes from supervisors *)
let get_registered_processes (program : program) : string list =
  List.fold_left
    (fun acc item ->
      match item with
      | OtpComponent (Supervisor { name = Some name; _ }) -> name :: acc
      | OtpComponent (Supervisor { name = None; _ }) -> acc
      | _ -> acc)
    [] program.items
