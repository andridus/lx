open Ast

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
     -compile(export_all).\n\n\
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
         -compile(export_all).\n\n\
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
         -compile(export_all).\n\n\
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
     -compile(export_all).\n\
     -include_lib(\"common_test/include/ct.hrl\").\n\n\
     all() ->\n\
    \    [basic_test].\n\n\
     basic_test(_Config) ->\n\
    \    ok.\n"
    app_name

(* Generate worker module content *)
let generate_worker_module app_name worker_name functions =
  let module_name = app_name ^ "_" ^ worker_name ^ "_worker" in
  let header =
    Printf.sprintf
      "-module(%s).\n\
       -behaviour(gen_server).\n\
       -compile(export_all).\n\n\
       start_link() ->\n\
      \    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n"
      module_name
  in

  (* Generate function implementations - simplified for now *)
  let function_impls =
    List.map
      (fun (func : function_def) ->
        match func.name with
        | "init" -> Printf.sprintf "init(_) ->\n    {ok, []}."
        | "handle_call" ->
            Printf.sprintf
              "handle_call(Request, _From, State) ->\n    {reply, ok, State}."
        | "handle_cast" ->
            Printf.sprintf
              "handle_cast(_Request, State) ->\n    {noreply, State}."
        | "handle_info" ->
            Printf.sprintf "handle_info(_Info, State) ->\n    {noreply, State}."
        | "terminate" -> Printf.sprintf "terminate(_Reason, _State) ->\n    ok."
        | "code_change" ->
            Printf.sprintf
              "code_change(_OldVsn, State, _Extra) ->\n    {ok, State}."
        | _ -> Printf.sprintf "%s() ->\n    ok." func.name)
      functions
  in

  header ^ "\n" ^ String.concat "\n\n" function_impls

(* Generate named supervisor module content *)
let generate_named_supervisor_module app_name supervisor_name strategy children
    =
  let module_name = app_name ^ "_" ^ supervisor_name ^ "_supervisor" in
  let header =
    Printf.sprintf
      "-module(%s).\n\
       -behaviour(supervisor).\n\
       -compile(export_all).\n\n\
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
          let worker_content = generate_worker_module app_name name functions in
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
