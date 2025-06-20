let () =
  let filename = Sys.argv.(1) in
  Compiler.compile_file filename
