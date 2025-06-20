let () =
  try
    if Array.length Sys.argv < 2 then (
      Printf.eprintf "Usage: %s <filename.lx>\n" Sys.argv.(0);
      exit 1);
    let filename = Sys.argv.(1) in
    Compiler.compile_file filename
  with
  | Failure msg when String.contains msg '\'' ->
      (* Check if it's a reserved word error *)
      if
        String.contains msg 't' && String.contains msg 'e'
        && String.contains msg 's'
      then
        Printf.eprintf
          "Error: 'test' is a reserved word and cannot be used as a function \
           name.\n\
           Reserved words include: test, spec, describe, worker, supervisor, \
           etc.\n\
           Try using a different name like 'test_func' or 'my_test'.\n"
      else Printf.eprintf "Compilation error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1
  | exn ->
      let error_msg = Printexc.to_string exn in
      if
        String.contains error_msg 'M'
        && String.contains error_msg 'e'
        && String.contains error_msg 'n'
      then
        Printf.eprintf
          "Syntax error: Unable to parse the input. This might be caused by:\n\
           - Using reserved words like 'test', 'spec', 'describe' as function \
           names\n\
           - Incorrect syntax in your LX code\n\n\
           Try using different function names or check your syntax.\n"
      else Printf.eprintf "Fatal error: exception %s\n" error_msg;
      exit 2
