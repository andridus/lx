let enhance_and_print_error error =
  let enhanced = Compiler.Error.enhance_ocaml_error error in
  Printf.eprintf "%s\n" enhanced

let () =
  try
    if Array.length Sys.argv < 2 then (
      Printf.eprintf "Usage: %s [--type-check] <filename.lx>\n" Sys.argv.(0);
      exit 1);

    let type_check_only, filename =
      if Array.length Sys.argv = 3 && Sys.argv.(1) = "--type-check" then
        (true, Sys.argv.(2))
      else (false, Sys.argv.(1))
    in

    if type_check_only then
      let _ = Compiler.type_check_file filename in
      Printf.printf "Type checking completed successfully for %s\n" filename
    else Compiler.compile_file filename
  with
  | Compiler.Error.CompilationError error ->
      Printf.eprintf "%s\n" (Compiler.Error.string_of_error error);
      exit 1
  | Compiler.Typechecker.TypeError error ->
      Printf.eprintf "Type Error: %s\n"
        (Compiler.Typechecker.string_of_type_error error);
      exit 1
  | Compiler.Otp_validator.OtpValidationError error ->
      Printf.eprintf "OTP Validation Error: %s\n"
        (Compiler.Otp_validator.string_of_otp_error error);
      exit 1
  | Failure msg ->
      let string_contains_substring s sub =
        let len_s = String.length s in
        let len_sub = String.length sub in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = sub then true
          else search (i + 1)
        in
        search 0
      in
      if string_contains_substring msg "Enhanced:" then
        Printf.eprintf "%s\n" msg
      else enhance_and_print_error msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1
  | exn ->
      let error_msg = Printexc.to_string exn in
      let string_contains_substring s sub =
        let len_s = String.length s in
        let len_sub = String.length sub in
        let rec search i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = sub then true
          else search (i + 1)
        in
        search 0
      in
      if
        string_contains_substring error_msg "belongs to the type"
        || string_contains_substring error_msg "record field"
        || string_contains_substring error_msg "Unbound"
      then enhance_and_print_error error_msg
      else Printf.eprintf "Fatal error: %s\n" error_msg;
      exit 2
