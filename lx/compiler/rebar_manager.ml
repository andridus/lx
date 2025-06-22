open Unix
open Error

(* Rebar3 management and compilation integration *)

(* Helper function to check if a string contains a substring *)
let string_contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let get_home_dir () = try Sys.getenv "HOME" with Not_found -> "/tmp"

let get_lx_dir () =
  let home = get_home_dir () in
  Filename.concat home ".lx"

let get_rebar_path () =
  let lx_dir = get_lx_dir () in
  Filename.concat lx_dir "rebar3"

(* Check if rebar3 exists in system PATH *)
let check_system_rebar () =
  try
    (* Redirect output to /dev/null to suppress output *)
    let devnull = Unix.openfile "/dev/null" [ O_WRONLY ] 0o644 in
    let _ =
      Unix.create_process "which" [| "which"; "rebar3" |] Unix.stdin devnull
        devnull
    in
    Unix.close devnull;
    let _, status = Unix.wait () in
    match status with WEXITED 0 -> Some "rebar3" | _ -> None
  with _ -> None

(* Download rebar3 to ~/.lx/ *)
let download_rebar () =
  let lx_dir = get_lx_dir () in
  let rebar_path = get_rebar_path () in

  (* Create .lx directory if it doesn't exist *)
  if not (Sys.file_exists lx_dir) then Unix.mkdir lx_dir 0o755;

  Printf.printf "Downloading rebar3 to %s...\n" rebar_path;

  (* Download rebar3 using curl or wget *)
  let download_cmd =
    let curl_available =
      try
        let devnull = Unix.openfile "/dev/null" [ O_WRONLY ] 0o644 in
        let _ =
          Unix.create_process "which" [| "which"; "curl" |] Unix.stdin devnull
            devnull
        in
        Unix.close devnull;
        let _, status = Unix.wait () in
        match status with WEXITED 0 -> true | _ -> false
      with _ -> false
    in
    if curl_available then
      Printf.sprintf
        "curl -L -o %s \
         https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3"
        rebar_path
    else
      Printf.sprintf
        "wget -O %s \
         https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3"
        rebar_path
  in

  let status = Unix.system download_cmd in
  match status with
  | WEXITED 0 ->
      (* Make rebar3 executable *)
      Unix.chmod rebar_path 0o755;
      Printf.printf "Successfully downloaded rebar3\n";
      true
  | _ ->
      Printf.eprintf "Failed to download rebar3\n";
      false

(* Get rebar3 executable path, downloading if necessary *)
let get_rebar_executable () =
  (* First check system PATH *)
  match check_system_rebar () with
  | Some path -> Some path
  | None ->
      (* Check if we have it in ~/.lx/ *)
      let local_rebar = get_rebar_path () in
      if Sys.file_exists local_rebar then Some local_rebar
      else if
        (* Download it *)
        download_rebar ()
      then Some local_rebar
      else None

(* Parse rebar3 error output and convert to LX error format *)
let parse_rebar_error output =
  let lines = String.split_on_char '\n' output in
  let errors = ref [] in

  List.iter
    (fun line ->
      let line = String.trim line in
      if String.length line > 0 then
        if
          (* Look for common rebar3 error patterns *)
          String.contains line ':'
        then
          try
            (* Try to parse file:line:column: error format *)
            let parts = String.split_on_char ':' line in
            match parts with
            | file :: line_str :: col_str :: error_parts
              when String.ends_with ~suffix:".erl" file
                   && String.for_all
                        (function '0' .. '9' -> true | _ -> false)
                        (String.trim line_str)
                   && String.for_all
                        (function '0' .. '9' | ' ' -> true | _ -> false)
                        (String.trim col_str) ->
                let line_num = int_of_string (String.trim line_str) in
                let col_num = int_of_string (String.trim col_str) in
                let error_msg = String.concat ":" error_parts |> String.trim in
                let position =
                  { line = line_num; column = col_num; filename = Some file }
                in
                let error =
                  {
                    kind = ParseError ("Rebar3 compilation error: " ^ error_msg);
                    position;
                    message = error_msg;
                    suggestion =
                      Some "Check the generated Erlang code for syntax errors";
                    context =
                      Some
                        "This error occurred during rebar3 compilation of \
                         generated Erlang code";
                  }
                in
                errors := error :: !errors
            | _ ->
                (* Generic error format *)
                if
                  string_contains_substring line "error"
                  || string_contains_substring line "Error"
                then
                  let position = { line = 0; column = 0; filename = None } in
                  let error =
                    {
                      kind = ParseError ("Rebar3 error: " ^ line);
                      position;
                      message = line;
                      suggestion =
                        Some "Check the rebar3 output for more details";
                      context =
                        Some "This error occurred during rebar3 compilation";
                    }
                  in
                  errors := error :: !errors
          with _ ->
            (* If parsing fails, treat as generic error *)
            if
              string_contains_substring line "error"
              || string_contains_substring line "Error"
            then
              let position = { line = 0; column = 0; filename = None } in
              let error =
                {
                  kind = ParseError ("Rebar3 error: " ^ line);
                  position;
                  message = line;
                  suggestion = Some "Check the rebar3 output for more details";
                  context = Some "This error occurred during rebar3 compilation";
                }
              in
              errors := error :: !errors)
    lines;

  List.rev !errors

(* Run rebar3 command and capture output *)
let run_rebar_command rebar_path project_dir command =
  let full_command =
    Printf.sprintf "cd %s && %s %s 2>&1" project_dir rebar_path command
  in

  try
    (* Use Unix.open_process_in for simpler output capture *)
    let ic = Unix.open_process_in full_command in
    let output = ref "" in

    let rec read_all () =
      try
        let line = input_line ic in
        output := !output ^ line ^ "\n";
        read_all ()
      with End_of_file -> ()
    in

    read_all ();
    let status = Unix.close_process_in ic in

    match status with
    | WEXITED 0 ->
        (* Suppress success message, only show rebar3 output *)
        Printf.printf "%s" !output;
        (true, !output)
    | WEXITED code ->
        Printf.eprintf "Rebar3 compilation failed with exit code %d\n" code;
        Printf.eprintf "%s\n" !output;
        (false, !output)
    | WSIGNALED signal ->
        Printf.eprintf "Rebar3 process killed by signal %d\n" signal;
        (false, !output)
    | WSTOPPED signal ->
        Printf.eprintf "Rebar3 process stopped by signal %d\n" signal;
        (false, !output)
  with
  | Unix.Unix_error (err, func, arg) ->
      let error_msg =
        Printf.sprintf "System error in %s(%s): %s" func arg
          (Unix.error_message err)
      in
      Printf.eprintf "%s\n" error_msg;
      (false, error_msg)
  | exn ->
      let error_msg =
        Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "%s\n" error_msg;
      (false, error_msg)

(* Compile project with rebar3 *)
let compile_project project_dir =
  Printf.printf "Compiling project...\n";

  (* Ensure project directory exists *)
  if not (Sys.file_exists project_dir) then (
    let error_msg =
      Printf.sprintf "Project directory does not exist: %s" project_dir
    in
    Printf.eprintf "%s\n" error_msg;
    let position = { line = 0; column = 0; filename = None } in
    let error =
      {
        kind = ParseError error_msg;
        position;
        message = error_msg;
        suggestion = Some "Make sure the project was generated correctly";
        context = Some "Project directory is required for rebar3 compilation";
      }
    in
    raise (CompilationError error));

  (* Ensure project directory is actually a directory *)
  if not (Sys.is_directory project_dir) then (
    let error_msg =
      Printf.sprintf "Project path is not a directory: %s" project_dir
    in
    Printf.eprintf "%s\n" error_msg;
    let position = { line = 0; column = 0; filename = None } in
    let error =
      {
        kind = ParseError error_msg;
        position;
        message = error_msg;
        suggestion =
          Some "Remove any file with the same name as the project directory";
        context = Some "Project directory is required for rebar3 compilation";
      }
    in
    raise (CompilationError error));

  match get_rebar_executable () with
  | None ->
      let error_msg =
        "Could not find or download rebar3. Please install rebar3 manually."
      in
      Printf.eprintf "%s\n" error_msg;
      let position = { line = 0; column = 0; filename = None } in
      let error =
        {
          kind = ParseError error_msg;
          position;
          message = error_msg;
          suggestion =
            Some
              "Install rebar3 from https://rebar3.org or your package manager";
          context =
            Some "Rebar3 is required to compile generated Erlang applications";
        }
      in
      raise (CompilationError error)
  | Some rebar_path ->
      (* Suppress rebar3 path message *)
      (* Printf.printf "Using rebar3 at: %s\n" rebar_path; *)
      let success, output =
        run_rebar_command rebar_path project_dir "compile"
      in

      if not success then
        (* Parse errors and raise them in LX format *)
        let errors = parse_rebar_error output in
        match errors with
        | [] ->
            (* No parsed errors, create a generic one *)
            let position = { line = 0; column = 0; filename = None } in
            let error =
              {
                kind = ParseError "Rebar3 compilation failed";
                position;
                message = "Rebar3 compilation failed with unknown error";
                suggestion = Some "Check the rebar3 output above for details";
                context =
                  Some
                    "Error occurred during rebar3 compilation of generated \
                     Erlang code";
              }
            in
            raise (CompilationError error)
        | error :: _ ->
            (* Raise the first error *)
            raise (CompilationError error)
      else Printf.printf "Project compiled successfully with rebar3\n"

(* Run tests with rebar3 *)
let run_tests project_dir =
  Printf.printf "Running tests with rebar3...\n";

  match get_rebar_executable () with
  | None ->
      Printf.eprintf "Could not find rebar3 for running tests\n";
      false
  | Some rebar_path ->
      let success, output = run_rebar_command rebar_path project_dir "ct" in
      if success then Printf.printf "Tests completed successfully\n"
      else Printf.eprintf "Tests failed\n%s\n" output;
      success

(* Clean project with rebar3 *)
let clean_project project_dir =
  Printf.printf "Cleaning project with rebar3...\n";

  match get_rebar_executable () with
  | None ->
      Printf.eprintf "Could not find rebar3 for cleaning\n";
      false
  | Some rebar_path ->
      let success, _ = run_rebar_command rebar_path project_dir "clean" in
      if success then Printf.printf "Project cleaned successfully\n"
      else Printf.eprintf "Failed to clean project\n";
      success
