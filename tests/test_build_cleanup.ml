open Alcotest

(* Helper function to create a temporary directory *)
let create_temp_dir prefix =
  let temp_dir = Filename.temp_file prefix "" in
  Sys.remove temp_dir;
  Unix.mkdir temp_dir 0o755;
  temp_dir

(* Helper function to recursively remove directory *)
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
  | Sys_error _ -> ()
  | Unix.Unix_error _ -> ()

(* Helper function to check if directory exists *)
let dir_exists path = try Sys.is_directory path with Sys_error _ -> false

(* Helper function to check if file exists *)
let file_exists path =
  try (not (Sys.is_directory path)) && Sys.file_exists path
  with Sys_error _ -> false

(* Helper function to count files in directory *)
let count_files_in_dir dir =
  if dir_exists dir then
    try
      let files = Sys.readdir dir in
      Array.length files
    with _ -> 0
  else 0

(* Helper function to write content to file *)
let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let test_cleanup_application_to_non_application () =
  let temp_dir = create_temp_dir "lx_test_cleanup_" in

  let cleanup () = remove_dir temp_dir in

  try
    (* Create a test file with application *)
    let test_file = Filename.concat temp_dir "test_app.lx" in
    let app_content =
      {|
application {
  description "Test cleanup application"
  vsn "1.0.0"
}

pub fun hello() {
  "Hello from app!"
}
|}
    in
    write_file test_file app_content;

    (* Compile with application - should create application structure *)
    Compiler.compile_file ~skip_rebar:true test_file;

    let build_dir = Filename.concat temp_dir "_build" in
    let project_dir = Filename.concat build_dir "test_app" in
    let src_dir = Filename.concat project_dir "src" in

    (* Verify application structure was created *)
    check bool "build directory exists after app compilation" true
      (dir_exists build_dir);
    check bool "project directory exists after app compilation" true
      (dir_exists project_dir);
    check bool "src directory exists after app compilation" true
      (dir_exists src_dir);
    check bool "rebar.config exists" true
      (file_exists (Filename.concat project_dir "rebar.config"));

    (* Now change to non-application *)
    let non_app_content =
      {|
pub fun hello() {
  "Hello from module!"
}

pub fun add(a, b) {
  a + b
}
|}
    in
    write_file test_file non_app_content;

    (* Compile without application - should cleanup and create simple structure *)
    Compiler.compile_file ~skip_rebar:true test_file;

    (* Verify old application structure was cleaned up *)
    check bool "src directory cleaned up" false (dir_exists src_dir);
    check bool "rebar.config cleaned up" false
      (file_exists (Filename.concat project_dir "rebar.config"));

    (* Verify new simple structure exists *)
    check bool "project directory still exists" true (dir_exists project_dir);
    check bool "erl file exists" true
      (file_exists (Filename.concat project_dir "test_app.erl"));

    (* Count files - should be minimal now *)
    let file_count = count_files_in_dir project_dir in
    check bool "minimal files in project dir" true (file_count <= 2);

    (* .erl file and maybe _build *)
    cleanup ()
  with exn ->
    cleanup ();
    failwith ("Test failed with exception: " ^ Printexc.to_string exn)

let test_cleanup_non_application_to_application () =
  let temp_dir = create_temp_dir "lx_test_cleanup_" in

  let cleanup () = remove_dir temp_dir in

  try
    (* Create a test file without application *)
    let test_file = Filename.concat temp_dir "test_module.lx" in
    let non_app_content =
      {|
pub fun greet(name) {
  name
}

pub fun calculate(x, y) {
  x + y
}
|}
    in
    write_file test_file non_app_content;

    (* Compile without application - should create simple structure *)
    Compiler.compile_file ~skip_rebar:true test_file;

    let build_dir = Filename.concat temp_dir "_build" in
    let project_dir = Filename.concat build_dir "test_module" in

    (* Verify simple structure was created *)
    check bool "build directory exists" true (dir_exists build_dir);
    check bool "project directory exists" true (dir_exists project_dir);
    check bool "erl file exists" true
      (file_exists (Filename.concat project_dir "test_module.erl"));

    let initial_file_count = count_files_in_dir project_dir in

    (* Now change to application *)
    let app_content =
      {|
application {
  description "Test module converted to app"
  vsn "2.0.0"
}

pub fun greet(name) {
  name
}

pub fun calculate(x, y) {
  x + y
}
|}
    in
    write_file test_file app_content;

    (* Compile with application - should cleanup and create application structure *)
    Compiler.compile_file ~skip_rebar:true test_file;

    (* Verify old simple structure was cleaned up *)
    check bool "old erl file cleaned up" false
      (file_exists (Filename.concat project_dir "test_module.erl"));

    (* Verify new application structure exists *)
    let src_dir = Filename.concat project_dir "src" in
    let test_dir = Filename.concat project_dir "test" in

    check bool "src directory created" true (dir_exists src_dir);
    check bool "test directory created" true (dir_exists test_dir);
    check bool "rebar.config created" true
      (file_exists (Filename.concat project_dir "rebar.config"));
    check bool "app.src created" true
      (file_exists (Filename.concat src_dir "test_module.app.src"));

    let final_file_count = count_files_in_dir project_dir in
    check bool "more files after app conversion" true
      (final_file_count > initial_file_count);

    cleanup ()
  with exn ->
    cleanup ();
    failwith ("Test failed with exception: " ^ Printexc.to_string exn)

let test_cleanup_same_type_recompilation () =
  let temp_dir = create_temp_dir "lx_test_cleanup_" in

  let cleanup () = remove_dir temp_dir in

  try
    (* Create a test file with application *)
    let test_file = Filename.concat temp_dir "test_same.lx" in
    let app_content_v1 =
      {|
application {
  description "Version 1"
  vsn "1.0.0"
}

pub fun version() {
  "v1"
}
|}
    in
    write_file test_file app_content_v1;

    (* First compilation *)
    Compiler.compile_file ~skip_rebar:true test_file;

    let build_dir = Filename.concat temp_dir "_build" in
    let project_dir = Filename.concat build_dir "test_same" in
    let src_dir = Filename.concat project_dir "src" in

    (* Verify structure exists *)
    check bool "structure exists after first compilation" true
      (dir_exists src_dir);

    (* Modify the application and recompile *)
    let app_content_v2 =
      {|
application {
  description "Version 2"
  vsn "2.0.0"
}

pub fun version() {
  "v2"
}

pub fun new_feature() {
  "added in v2"
}
|}
    in
    write_file test_file app_content_v2;

    (* Second compilation - should cleanup and regenerate *)
    Compiler.compile_file ~skip_rebar:true test_file;

    (* Verify structure still exists and was properly regenerated *)
    check bool "structure exists after second compilation" true
      (dir_exists src_dir);
    check bool "rebar.config exists after recompilation" true
      (file_exists (Filename.concat project_dir "rebar.config"));

    cleanup ()
  with
  | Compiler.Error.CompilationError err ->
      cleanup ();
      Printf.printf "[DEBUG] CompilationError: %s\n"
        (Compiler.Error.string_of_error err);
      failwith
        ("Test failed with CompilationError: "
        ^ Compiler.Error.string_of_error err)
  | exn ->
      cleanup ();
      Printf.printf "[DEBUG] Exception: %s\n" (Printexc.to_string exn);
      failwith ("Test failed with exception: " ^ Printexc.to_string exn)

let test_cleanup_handles_missing_directory () =
  let temp_dir = create_temp_dir "lx_test_cleanup_" in

  let cleanup () = remove_dir temp_dir in

  try
    (* Create a test file *)
    let test_file = Filename.concat temp_dir "test_missing.lx" in
    let content = {|
pub fun test() {
  "test"
}
|} in
    write_file test_file content;

    (* Compile - should work even when no previous build directory exists *)
    Compiler.compile_file ~skip_rebar:true test_file;

    let build_dir = Filename.concat temp_dir "_build" in
    let project_dir = Filename.concat build_dir "test_missing" in

    (* Verify structure was created successfully *)
    check bool "build directory created" true (dir_exists build_dir);
    check bool "project directory created" true (dir_exists project_dir);
    check bool "erl file created" true
      (file_exists (Filename.concat project_dir "test_missing.erl"));

    cleanup ()
  with exn ->
    cleanup ();
    failwith ("Test failed with exception: " ^ Printexc.to_string exn)

let tests =
  [
    ( "cleanup application to non-application",
      `Quick,
      test_cleanup_application_to_non_application );
    ( "cleanup non-application to application",
      `Quick,
      test_cleanup_non_application_to_application );
    ( "cleanup same type recompilation",
      `Quick,
      test_cleanup_same_type_recompilation );
    ( "cleanup handles missing directory",
      `Quick,
      test_cleanup_handles_missing_directory );
  ]
