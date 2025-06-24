open Ast
open Beam_analyzer

(* Exception types for dependency errors *)
exception DependencyError of string

(* Module registry for loaded dependencies *)
type module_registry = (string * beam_module) list

let registry = ref []
let global_config : project_config option ref = ref None

(* Load global configuration *)
let load_global_config (config_path : string) : project_config option =
  try
    if Sys.file_exists config_path then (
      (* Parse config file - for now, return None as placeholder *)
      (* In a real implementation, this would parse the lx.config file *)
      Printf.printf "Loading global config from: %s\n" config_path;
      None)
    else None
  with Sys_error _ -> None (* No global config file *)

(* Resolve dependency source to actual path *)
let resolve_dependency_path (dep : dependency) : string =
  match dep with
  | Simple name -> find_beam_file name (* Look in standard locations *)
  | Version (name, version) ->
      (* Look in hex cache or download - placeholder *)
      Printf.printf "Resolving versioned dependency: %s (%s)\n" name version;
      find_beam_file name
  | GitHub (name, repo_path) ->
      (* Clone/fetch from GitHub and find BEAM - placeholder *)
      Printf.printf "Resolving GitHub dependency: %s from %s\n" name repo_path;
      find_beam_file name
  | Path (name, local_path) ->
      (* Look in specified local path *)
      Printf.printf "Resolving local path dependency: %s at %s\n" name
        local_path;
      let beam_path = Filename.concat local_path (name ^ ".beam") in
      if Sys.file_exists beam_path then beam_path else find_beam_file name
  | Hex (name, version) ->
      (* Download from hex.pm - placeholder *)
      Printf.printf "Resolving hex dependency: %s (%s)\n" name version;
      find_beam_file name

(* Get effective dependencies for a module *)
let get_effective_dependencies (module_deps : dependency list option) :
    dependency list =
  let global_deps =
    match !global_config with Some config -> config.deps | None -> []
  in
  let local_deps = match module_deps with Some deps -> deps | None -> [] in
  (* Merge global and local deps, local overrides global *)
  let all_deps = global_deps @ local_deps in
  (* Remove duplicates, keeping last occurrence (local override) *)
  List.fold_left
    (fun acc dep ->
      let dep_name =
        match dep with
        | Simple name
        | Version (name, _)
        | GitHub (name, _)
        | Path (name, _)
        | Hex (name, _) ->
            name
      in
      (* Remove any existing dep with same name, then add new one *)
      let filtered =
        List.filter
          (fun existing_dep ->
            let existing_name =
              match existing_dep with
              | Simple name
              | Version (name, _)
              | GitHub (name, _)
              | Path (name, _)
              | Hex (name, _) ->
                  name
            in
            existing_name <> dep_name)
          acc
      in
      filtered @ [ dep ])
    [] all_deps

(* Load dependencies *)
let load_dependencies (deps : dependency list) : unit =
  (* First load built-in modules *)
  let builtin_modules = get_builtin_modules () in
  registry := builtin_modules;

  (* Then load specified dependencies *)
  List.iter
    (fun dep ->
      try
        let dep_name =
          match dep with
          | Simple name
          | Version (name, _)
          | GitHub (name, _)
          | Path (name, _)
          | Hex (name, _) ->
              name
        in

        (* Check if it's already a built-in module *)
        if List.mem_assoc dep_name !registry then
          Printf.printf "Using built-in module: %s\n" dep_name
        else
          let beam_file = resolve_dependency_path dep in
          let module_info = analyze_beam_file beam_file in
          registry := (dep_name, module_info) :: !registry;
          Printf.printf "Loaded dependency: %s\n" dep_name
      with
      | BeamAnalysisError msg ->
          Printf.printf "Warning: Could not load dependency %s: %s\n"
            (string_of_dependency dep) msg
      | Failure msg ->
          Printf.printf "Warning: Failed to load dependency %s: %s\n"
            (string_of_dependency dep) msg)
    deps

(* Validate external function call *)
let validate_external_call (module_name : string) (func_name : string)
    (args : expr list) : external_function_info =
  try
    let module_info = List.assoc module_name !registry in
    let arity = List.length args in
    match find_function module_info func_name arity with
    | Some func_info ->
        {
          module_name;
          function_name = func_name;
          arity;
          param_types =
            (match func_info.type_info with
            | Some (params, _) -> Some params
            | None -> None);
          return_type =
            (match func_info.type_info with
            | Some (_, ret) -> Some ret
            | None -> None);
        }
    | None ->
        (* Function not found - this might be acceptable for some modules *)
        Printf.printf
          "Warning: Function %s:%s/%d not found in module definition\n"
          module_name func_name arity;
        {
          module_name;
          function_name = func_name;
          arity;
          param_types = None;
          return_type = None;
        }
  with Not_found ->
    raise
      (DependencyError
         ("Module not loaded: " ^ module_name
        ^ ". Add it to your deps declaration."))

(* Get available functions for a module *)
let get_module_functions (module_name : string) : beam_function list =
  try
    let module_info = List.assoc module_name !registry in
    List.filter (fun f -> f.exported) module_info.functions
  with Not_found -> []

(* Check if module is loaded *)
let is_module_loaded (module_name : string) : bool =
  List.mem_assoc module_name !registry

(* Get all loaded modules *)
let get_loaded_modules () : string list = List.map fst !registry

(* Initialize with standard modules *)
let initialize_dependencies () : unit =
  let stdlib_modules = get_builtin_modules () in
  registry := stdlib_modules;
  Printf.printf "Initialized with %d built-in modules\n"
    (List.length stdlib_modules)

(* Clear registry (for testing) *)
let clear_registry () : unit = registry := []

(* Print loaded modules (for debugging) *)
let print_loaded_modules () : unit =
  Printf.printf "Loaded modules:\n";
  List.iter
    (fun (name, module_info) ->
      Printf.printf "  - %s (%d functions)\n" name
        (List.length module_info.functions))
    !registry
