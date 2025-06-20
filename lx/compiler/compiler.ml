(* Module aliases for external access *)
module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Typechecker = Typechecker
module Otp_validator = Otp_validator
module Error = Error
open Ast

(* Check for reserved words in function names *)
let reserved_words =
  [
    "test";
    "spec";
    "describe";
    "worker";
    "supervisor";
    "strategy";
    "children";
    "init";
    "call";
    "cast";
    "info";
    "terminate";
    "requires";
    "ensures";
    "assert";
    "case";
    "if";
    "then";
    "else";
    "for";
    "when";
    "let";
    "in";
    "fun";
    "matches";
  ]

let is_reserved_word word = List.mem word reserved_words

let check_function_name name =
  if is_reserved_word name then
    let message =
      Printf.sprintf
        "'%s' is a reserved word and cannot be used as a function name.\n\
         Reserved words include: test, spec, describe, worker, supervisor, etc.\n\
         Try using a different name like '%s_func' or 'my_%s'."
        name name name
    in
    failwith message

let capitalize_var id =
  if String.length id > 0 then
    String.uppercase_ascii (String.sub id 0 1)
    ^ String.sub id 1 (String.length id - 1)
  else id

let emit_literal (l : literal) : string =
  match l with
  | LInt n -> string_of_int n
  | LFloat f -> string_of_float f
  | LString s -> "\"" ^ s ^ "\""
  | LBool true -> "true"
  | LBool false -> "false"
  | LAtom a -> a
  | LNil -> "nil"

let rec emit_pattern (p : pattern) : string =
  match p with
  | PWildcard -> "_"
  | PVar id -> capitalize_var id
  | PAtom a -> a
  | PLiteral l -> emit_literal l
  | PTuple ps -> "{" ^ String.concat ", " (List.map emit_pattern ps) ^ "}"
  | PList ps -> "[" ^ String.concat ", " (List.map emit_pattern ps) ^ "]"
  | PCons (head, tail) ->
      "[" ^ emit_pattern head ^ " | " ^ emit_pattern tail ^ "]"

let rec emit_expr (e : expr) : string =
  match e with
  | Literal l -> emit_literal l
  | Var id -> capitalize_var id
  | Let (id, value, body) ->
      "(" ^ capitalize_var id ^ " = " ^ emit_expr value ^ ", " ^ emit_expr body
      ^ ")"
  | Fun (params, body) ->
      "fun("
      ^ String.concat ", " (List.map capitalize_var params)
      ^ ") -> " ^ emit_expr body ^ " end"
  | App (Var func_name, args) ->
      (* Function calls should not capitalize the function name *)
      func_name ^ "(" ^ String.concat ", " (List.map emit_expr args) ^ ")"
  | App (func, args) ->
      (* For complex function expressions *)
      emit_expr func ^ "(" ^ String.concat ", " (List.map emit_expr args) ^ ")"
  | ExternalCall (module_name, func_name, args) ->
      (* Generate Erlang external call: module:function(args) *)
      module_name ^ ":" ^ func_name ^ "(" ^ String.concat ", " (List.map emit_expr args) ^ ")"
  | Tuple exprs -> "{" ^ String.concat ", " (List.map emit_expr exprs) ^ "}"
  | List exprs -> "[" ^ String.concat ", " (List.map emit_expr exprs) ^ "]"
  | If (cond, then_expr, else_expr) ->
      "case " ^ emit_expr cond ^ " of true -> " ^ emit_expr then_expr
      ^ (match else_expr with
        | Some e -> "; _ -> " ^ emit_expr e
        | None -> "; _ -> nil")
      ^ " end"
  | Match (value, cases) ->
      "case " ^ emit_expr value ^ " of "
      ^ String.concat "; "
          (List.map (fun (p, e) -> emit_pattern p ^ " -> " ^ emit_expr e) cases)
      ^ " end"
  | For (_, _, _) -> "% For expressions not yet implemented"
  | Sequence exprs ->
      (* In Erlang, sequence of expressions is separated by commas *)
      String.concat ",\n    " (List.map emit_expr exprs)

let emit_function_clause (func_name : string) (clause : function_clause) : string =
  func_name ^ "("
  ^ String.concat ", " (List.map capitalize_var clause.params)
  ^ ") ->\n    " ^ emit_expr clause.body

let emit_function_def (func : function_def) : string =
  check_function_name func.name;
  let clauses_code = List.map (emit_function_clause func.name) func.clauses in
  String.concat ";\n" clauses_code ^ "."

let emit_spec (spec : spec) : string =
  let emit_spec_expr expr =
    match expr with
    | App (Var "in", [ Var var; list_expr ]) ->
        var ^ " in " ^ emit_expr list_expr
    | App (Var "matches", [ Var result; pattern_expr ]) ->
        result ^ " matches " ^ emit_expr pattern_expr
    | _ -> emit_expr expr
  in
  let requires_str =
    if List.length spec.requires > 0 then
      "\n%% @requires "
      ^ String.concat ", " (List.map emit_spec_expr spec.requires)
    else ""
  in
  let ensures_str =
    if List.length spec.ensures > 0 then
      "\n%% @ensures "
      ^ String.concat ", " (List.map emit_spec_expr spec.ensures)
    else ""
  in
  "%% Spec for " ^ spec.name ^ requires_str ^ ensures_str

let emit_test_def (test : test_def) : string =
  "test_"
  ^ String.map (function ' ' -> '_' | c -> c) test.name
  ^ "() ->\n    " ^ emit_expr test.body ^ "."

let emit_describe_block (desc : describe_block) : string =
  let test_functions = List.map emit_test_def desc.tests in
  "% Test suite: " ^ desc.name ^ "\n" ^ String.concat "\n\n" test_functions

(* Generate OTP component code *)
let emit_otp_component (base_module_name : string) (component : otp_component) :
    string * string =
  match component with
  | Worker { name; handlers; functions; specs } ->
      let module_name = base_module_name ^ "_" ^ name in
      let header =
        "-module(" ^ module_name
        ^ ").\n-behaviour(gen_server).\n-compile(export_all).\n\n"
      in

      (* Generate handler functions *)
      let handler_code =
        List.map
          (fun (handler, func) ->
            let handler_name =
              match handler with
              | Init -> "init"
              | Call -> "handle_call"
              | Cast -> "handle_cast"
              | Info -> "handle_info"
              | Terminate -> "terminate"
            in
            (* For handlers, we need to handle the new function_def structure *)
            match func.clauses with
            | [clause] ->
                handler_name ^ "("
                ^ String.concat ", " (List.map capitalize_var clause.params)
                ^ ") ->\n    " ^ emit_expr clause.body ^ "."
            | _ ->
                (* Multiple clauses for handlers - generate separate clauses *)
                let clauses_code = List.map (emit_function_clause handler_name) func.clauses in
                String.concat ";\n" clauses_code ^ ".")
          handlers
      in

      (* Generate regular functions *)
      let function_code = List.map emit_function_def functions in

      (* Generate specs *)
      let spec_code = List.map emit_spec specs in

      let all_code = spec_code @ handler_code @ function_code in
      (module_name, header ^ String.concat "\n\n" all_code)
  | Supervisor { name; strategy; children } ->
      let module_name = base_module_name ^ "_" ^ name in
      let header =
        "-module(" ^ module_name
        ^ ").\n-behaviour(supervisor).\n-compile(export_all).\n\n"
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
            "#{id => " ^ child ^ ", start => {" ^ base_module_name ^ "_" ^ child
            ^ ", start_link, []}}")
          children
      in

      let supervisor_code =
        "init([]) ->\n" ^ "    Children = ["
        ^ String.concat ",\n                " children_specs
        ^ "],\n" ^ "    {ok, {#{strategy => " ^ strategy_str
        ^ ", intensity => 10, period => 60}, Children}}."
      in

      (module_name, header ^ supervisor_code)

let emit_module_item (item : module_item) : string =
  match item with
  | Function func -> emit_function_def func
  | OtpComponent _ -> "% OTP component handled separately"
  | Spec spec -> emit_spec spec
  | Test desc -> emit_describe_block desc

let parse_string ?(filename = None) (content : string) : program =
  let lexbuf = Lexing.from_string content in
  Lexer.set_filename filename;
  try Parser.main Lexer.read lexbuf with
  | Error.CompilationError _ as e -> raise e
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let token = Lexing.lexeme lexbuf in
      let message =
        Printf.sprintf "Unexpected token '%s' at line %d, column %d" token line
          col
      in
      Error.parse_error ~filename lexbuf message
  | exn ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let message =
        Printf.sprintf "Parse error at line %d, column %d: %s" line col
          (Printexc.to_string exn)
      in
      Error.parse_error ~filename lexbuf message

let parse_file (filename : string) : program =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexer.set_filename (Some filename);
  try
    let result = Parser.main Lexer.read lexbuf in
    close_in ic;
    result
  with
  | Error.CompilationError _ as e ->
      close_in ic;
      raise e
  | Parser.Error ->
      close_in ic;
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let token = Lexing.lexeme lexbuf in
      let message =
        Printf.sprintf "Unexpected token '%s' at line %d, column %d" token line
          col
      in
      Error.parse_error ~filename:(Some filename) lexbuf message
  | exn ->
      close_in ic;
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let message =
        Printf.sprintf "Parse error at line %d, column %d: %s" line col
          (Printexc.to_string exn)
      in
      Error.parse_error ~filename:(Some filename) lexbuf message

let compile_to_string_with_module_name (program : program)
    (base_module_name : string) : (string * string) list =
  (* Type checking phase *)
  (try
     let type_env = Typechecker.type_check_program program in
     (* OTP validation phase *)
     try Otp_validator.validate_program program type_env
     with Otp_validator.OtpValidationError error ->
       Printf.eprintf "OTP Validation Error: %s\n"
         (Otp_validator.string_of_otp_error error);
       failwith "OTP validation failed"
   with Typechecker.TypeError error ->
     Printf.eprintf "Type Error: %s\n" (Typechecker.string_of_type_error error);
     failwith "Type checking failed");

  (* Separate OTP components from regular items *)
  let otp_components, regular_items =
    List.partition
      (function OtpComponent _ -> true | _ -> false)
      program.items
  in

  (* Generate main module *)
  let main_header =
    "-module(" ^ base_module_name ^ ").\n-compile(export_all).\n\n"
  in
  let main_items = List.map emit_module_item regular_items in
  let main_module =
    (base_module_name, main_header ^ String.concat "\n\n" main_items)
  in

  (* Generate OTP component modules *)
  let otp_modules =
    List.map
      (function
        | OtpComponent component ->
            emit_otp_component base_module_name component
        | _ -> failwith "Expected OTP component")
      otp_components
  in

  main_module :: otp_modules

let compile_to_string (program : program) : string =
  let modules = compile_to_string_with_module_name program "generated" in
  (* Return only the main module for backward compatibility *)
  snd (List.hd modules)

(* Type check a program without compilation *)
let type_check_program (program : program) : Typechecker.type_env =
  try
    let type_env = Typechecker.type_check_program program in
    Printf.printf "Type checking completed successfully.\n";

    (* Print inferred types for functions *)
    Printf.printf "\nInferred types:\n";
    List.iter
      (fun item ->
        match item with
        | Function func -> (
            try
              let func_type = List.assoc func.name type_env in
              Printf.printf "  %s : %s\n" func.name
                (Typechecker.string_of_type func_type)
            with Not_found ->
              Printf.printf "  %s : <type not found>\n" func.name)
        | _ -> ())
      program.items;

    (* Also run OTP validation *)
    (try
       Otp_validator.validate_program program type_env;
       Printf.printf "\nOTP validation completed successfully.\n"
     with Otp_validator.OtpValidationError error ->
       Printf.eprintf "OTP Validation Error: %s\n"
         (Otp_validator.string_of_otp_error error);
       failwith "OTP validation failed");

    type_env
  with Typechecker.TypeError error ->
    Printf.eprintf "Type Error: %s\n" (Typechecker.string_of_type_error error);
    failwith "Type checking failed"

(* Type check a file *)
let type_check_file (filename : string) : Typechecker.type_env =
  let program = parse_file filename in
  type_check_program program

let compile_file (filename : string) : unit =
  let program = parse_file filename in
  let base_name = Filename.remove_extension (Filename.basename filename) in
  let source_dir = Filename.dirname filename in
  let modules = compile_to_string_with_module_name program base_name in

  (* Write each module to its own file in the same directory as the source *)
  List.iter
    (fun (module_name, module_content) ->
      let output_filename = Filename.concat source_dir (module_name ^ ".erl") in
      let oc = open_out output_filename in
      output_string oc module_content;
      close_out oc;
      Printf.printf "Generated module: %s\n" output_filename)
    modules;

  Printf.printf "Compiled %s -> %d modules\n" filename (List.length modules)
