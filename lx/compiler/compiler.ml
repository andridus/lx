(* Module aliases for external access *)
module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Typechecker = Typechecker
module Otp_validator = Otp_validator
module Linter = Linter
module Error = Error
module App_generator = App_generator
module Rebar_manager = Rebar_manager
open Ast

(* Initialize random number generator *)
let () = Random.self_init ()

let capitalize_var id =
  if String.length id > 0 then
    String.uppercase_ascii (String.sub id 0 1)
    ^ String.sub id 1 (String.length id - 1)
  else id

(* Variable renaming context for scope simulation *)
type rename_context = {
  scope_hash : string;
  var_map : (string, string) Hashtbl.t;
  parent : rename_context option;
  used_hashes : (string, bool) Hashtbl.t;
}

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
  }

let is_ignored_var var_name = String.length var_name > 0 && var_name.[0] = '_'

let get_renamed_var ctx var_name =
  if is_ignored_var var_name then
    (* Always return underscore for ignored variables *)
    "_"
  else
    let rec lookup ctx =
      match Hashtbl.find_opt ctx.var_map var_name with
      | Some renamed -> renamed
      | None -> (
          match ctx.parent with
          | Some parent_ctx -> lookup parent_ctx
          | None -> capitalize_var var_name (* fallback to original name *))
    in
    lookup ctx

let add_var_to_scope ctx var_name =
  if is_ignored_var var_name then (
    (* For ignored variables, use underscore in Erlang *)
    let renamed = "_" in
    Hashtbl.replace ctx.var_map var_name renamed;
    renamed)
  else
    let renamed = capitalize_var var_name ^ "_" ^ ctx.scope_hash in
    Hashtbl.replace ctx.var_map var_name renamed;
    renamed

(* Check for reserved words in function names *)
let reserved_words =
  [
    "case";
    "if";
    "else";
    "for";
    "when";
    "true";
    "false";
    "nil";
    "worker";
    "supervisor";
    "strategy";
    "children";
    "one_for_one";
    "one_for_all";
    "rest_for_one";
    "spec";
    "requires";
    "ensures";
    "matches";
    "describe";
    "assert";
  ]

let is_reserved_word word = List.mem word reserved_words

let check_function_name name =
  if is_reserved_word name then
    let message =
      Printf.sprintf
        "Enhanced:'%s' is a reserved word and cannot be used as a function \
         name|Suggestion:Try using a different name like '%s_func' or \
         'my_%s'|Context:Reserved words include: spec, worker, supervisor, \
         etc."
        name name name
    in
    failwith message

let emit_literal (l : literal) : string =
  match l with
  | LInt n -> string_of_int n
  | LFloat f ->
      let s = string_of_float f in
      (* Ensure float always has proper format for Erlang *)
      if String.ends_with ~suffix:"." s then s ^ "0"
      else if not (String.contains s '.') then s ^ ".0"
      else s
  | LString s -> "\"" ^ s ^ "\""
  | LBool true -> "true"
  | LBool false -> "false"
  | LAtom a -> a (* Atoms in Erlang don't need colon prefix *)
  | LNil -> "nil"

let rec emit_pattern ctx (p : pattern) : string =
  match p with
  | PWildcard -> "_"
  | PVar id -> if is_ignored_var id then "_" else get_renamed_var ctx id
  | PAtom a -> a (* Atoms in Erlang don't need colon prefix *)
  | PLiteral l -> emit_literal l
  | PTuple ps -> "{" ^ String.concat ", " (List.map (emit_pattern ctx) ps) ^ "}"
  | PList ps -> "[" ^ String.concat ", " (List.map (emit_pattern ctx) ps) ^ "]"
  | PCons (head, tail) ->
      "[" ^ emit_pattern ctx head ^ " | " ^ emit_pattern ctx tail ^ "]"

(* Emit block expressions inline with proper variable scoping *)
and emit_block_inline ctx var_name renamed_var exprs =
  let block_ctx = create_scope (Some ctx) in

  (* Process all expressions in the block *)
  let rec process_exprs acc = function
    | [] -> (acc, "nil")
    | [ last_expr ] ->
        (* Last expression determines the block result *)
        let last_code = emit_expr block_ctx last_expr in
        (acc, last_code)
    | expr :: rest ->
        let expr_code = emit_expr block_ctx expr in
        process_exprs (acc @ [ expr_code ]) rest
  in

  let statements, result_expr = process_exprs [] exprs in

  (* Generate the inline block code *)
  let block_name =
    String.capitalize_ascii var_name ^ "_" ^ block_ctx.scope_hash
  in
  let block_comment = "% start block " ^ block_name in
  let end_comment = "% end block " ^ block_name in

  match statements with
  | [] -> renamed_var ^ " = " ^ result_expr
  | _ ->
      (* Create the block with comments and inline statements, then assign result *)
      block_comment ^ "\n    "
      ^ String.concat ",\n    " statements
      ^ ",\n    " ^ end_comment ^ "\n    " ^ renamed_var ^ " = " ^ result_expr

(* Guard expression emission *)
and emit_guard_expr ctx (guard : guard_expr) : string =
  match guard with
  | GuardAnd (g1, g2) ->
      emit_guard_expr ctx g1 ^ ", " ^ emit_guard_expr ctx g2
  | GuardOr (g1, g2) ->
      emit_guard_expr ctx g1 ^ "; " ^ emit_guard_expr ctx g2
  | GuardAndalso (g1, g2) ->
      emit_guard_expr ctx g1 ^ " andalso " ^ emit_guard_expr ctx g2
  | GuardOrelse (g1, g2) ->
      emit_guard_expr ctx g1 ^ " orelse " ^ emit_guard_expr ctx g2
  | GuardNot g ->
      "not " ^ emit_guard_expr ctx g
  | GuardBinOp (left, op, right) ->
      let erlang_op = match op with
        | "!=" -> "/="
        | "<=" -> "=<"
        | other -> other
      in
      emit_guard_value ctx left ^ " " ^ erlang_op ^ " " ^ emit_guard_value ctx right
  | GuardCall (func, args) ->
      let erlang_func = match func with
        | "is_atom" -> "is_atom"
        | "is_integer" -> "is_integer"
        | "is_float" -> "is_float"
        | "is_number" -> "is_number"
        | "is_boolean" -> "is_boolean"
        | "is_list" -> "is_list"
        | "is_tuple" -> "is_tuple"
        | other -> other
      in
      erlang_func ^ "(" ^ String.concat ", " (List.map (emit_guard_value ctx) args) ^ ")"
  | GuardAtom atom ->
      emit_guard_atom ctx atom

and emit_guard_atom ctx (atom : guard_atom) : string =
  match atom with
  | GuardVar var -> get_renamed_var ctx var
  | GuardLiteral lit -> emit_literal lit
  | GuardCallAtom (func, args) ->
      func ^ "(" ^ String.concat ", " (List.map (emit_guard_atom ctx) args) ^ ")"

and emit_guard_value ctx (value : guard_value) : string =
  match value with
  | GuardAtomValue atom -> emit_guard_atom ctx atom
  | GuardCallValue (func, args) ->
      let erlang_func = match func with
        | "is_atom" -> "is_atom"
        | "is_integer" -> "is_integer"
        | "is_float" -> "is_float"
        | "is_number" -> "is_number"
        | "is_boolean" -> "is_boolean"
        | "is_list" -> "is_list"
        | "is_tuple" -> "is_tuple"
        | other -> other
      in
      erlang_func ^ "(" ^ String.concat ", " (List.map (emit_guard_value ctx) args) ^ ")"

and emit_expr ctx (e : expr) : string =
  (* Check for invalid colon syntax patterns in any expression *)
  (match e with
  | Sequence exprs | Block exprs ->
      (* Check for the invalid pattern: module_var followed by function call with atom *)
      let rec check_pattern = function
        | Var module_name :: App (Literal (LAtom func_name), _) :: _ ->
            failwith
              ("Enhanced:Invalid module call syntax detected - use '.' instead \
                of ':' for module calls|Suggestion:Change '" ^ module_name ^ ":"
             ^ func_name ^ "(...)' to '" ^ module_name ^ "." ^ func_name
             ^ "(...)'|Context:Lx uses dot notation for module calls, not \
                colon notation")
        | Var module_name :: Literal (LAtom func_name) :: _ :: _ ->
            failwith
              ("Enhanced:Invalid module call syntax detected - use '.' instead \
                of ':' for module calls|Suggestion:Change '" ^ module_name ^ ":"
             ^ func_name ^ "(...)' to '" ^ module_name ^ "." ^ func_name
             ^ "(...)'|Context:Lx uses dot notation for module calls, not \
                colon notation")
        | _ :: rest -> check_pattern rest
        | [] -> ()
      in
      check_pattern exprs
  | _ -> ());

  match e with
  | Literal l -> emit_literal l
  | Var id -> get_renamed_var ctx id
  | Assign (id, value, _pos) -> (
      if
        (* Check if this is an ignored variable assignment *)
        is_ignored_var id
      then
        (* For ignored variables, just evaluate the right side for side effects *)
        emit_expr ctx value
      else
        (* Normal assignment handling *)
        let renamed = add_var_to_scope ctx id in
        match value with
        (* Special handling for block assignments *)
        | Block exprs -> emit_block_inline ctx id renamed exprs
        (* If assigning another assignment, we can optimize by using the value directly *)
        | Assign (_, inner_value, _) ->
            (* For single assignment in block, use the inner value directly *)
            renamed ^ " = " ^ emit_expr ctx inner_value
        | _ -> renamed ^ " = " ^ emit_expr ctx value)
  | Fun (params, body) ->
      let fun_ctx = create_scope (Some ctx) in
      let renamed_params = List.map (add_var_to_scope fun_ctx) params in
      "fun("
      ^ String.concat ", " renamed_params
      ^ ") -> " ^ emit_expr fun_ctx body ^ " end"
  | App (Var func_name, args) ->
      (* Function calls should not capitalize the function name *)
      func_name ^ "(" ^ String.concat ", " (List.map (emit_expr ctx) args) ^ ")"
  | App (func, args) ->
      (* For complex function expressions *)
      emit_expr ctx func ^ "("
      ^ String.concat ", " (List.map (emit_expr ctx) args)
      ^ ")"
  | ExternalCall (module_name, func_name, args) ->
      (* Generate Erlang external call: module:function(args) *)
      module_name ^ ":" ^ func_name ^ "("
      ^ String.concat ", " (List.map (emit_expr ctx) args)
      ^ ")"
  | Tuple exprs ->
      "{" ^ String.concat ", " (List.map (emit_expr ctx) exprs) ^ "}"
  | List exprs ->
      "[" ^ String.concat ", " (List.map (emit_expr ctx) exprs) ^ "]"
  | If (cond, then_expr, else_expr) ->
      "case " ^ emit_expr ctx cond ^ " of true -> " ^ emit_expr ctx then_expr
      ^ (match else_expr with
        | Some e -> "; _ -> " ^ emit_expr ctx e
        | None -> "; _ -> nil")
      ^ " end"
  | Match (value, cases) ->
      "case " ^ emit_expr ctx value ^ " of "
      ^ String.concat "; "
          (List.map
             (fun (p, guard_opt, e) ->
               let guard_str = match guard_opt with
                 | Some guard -> " when " ^ emit_guard_expr ctx guard
                 | None -> ""
               in
               emit_pattern ctx p ^ guard_str ^ " -> " ^ emit_expr ctx e)
             cases)
      ^ " end"
  | For (_, _, _) -> "% For expressions not yet implemented"
  | Sequence exprs ->
      (* Function body sequences - detect external calls pattern *)
      let block_ctx = create_scope (Some ctx) in
      let transformed_exprs = detect_and_transform_external_calls exprs in
      String.concat ",\n    " (List.map (emit_expr block_ctx) transformed_exprs)
  | Block exprs ->
      (* This should not be called directly - blocks are handled in assignments *)
      let block_ctx = create_scope (Some ctx) in
      let transformed_exprs = detect_and_transform_external_calls exprs in
      String.concat ",\n    " (List.map (emit_expr block_ctx) transformed_exprs)
  | UnaryOp (op, operand) ->
      op ^ " " ^ emit_expr ctx operand
  | BinOp (left, op, right) ->
      let erlang_op =
        match op with
        | "!=" -> "/=" (* Erlang uses /= for not equal *)
        | "<=" -> "=<" (* Erlang uses =< for less than or equal *)
        | other -> other (* Keep other operators as is *)
      in
      emit_expr ctx left ^ " " ^ erlang_op ^ " " ^ emit_expr ctx right
  | Send (target, message) ->
      emit_expr ctx target ^ " ! " ^ emit_expr ctx message

(* Helper function to detect and transform external call patterns *)
and detect_and_transform_external_calls exprs =
  let rec transform acc = function
    | [] -> List.rev acc
    | [ expr ] -> List.rev (expr :: acc)
    | Var module_name :: Literal (LAtom func_name) :: App (_, _) :: _rest ->
        (* Pattern: module_var, :func_atom, function_call -> This is invalid syntax! *)
        failwith
          ("Enhanced:Invalid module call syntax detected - use '.' instead of \
            ':' for module calls|Suggestion:Change '" ^ module_name ^ ":"
         ^ func_name ^ "()' to '" ^ module_name ^ "." ^ func_name
         ^ "()'|Context:Lx uses dot notation for module calls, not colon \
            notation")
    | Var module_name :: Literal (LAtom func_name) :: _arg1 :: _arg2 :: _rest ->
        (* Pattern: module_var, :func_atom, arg1, arg2, ... -> This is also invalid syntax! *)
        failwith
          ("Enhanced:Invalid module call syntax detected - use '.' instead of \
            ':' for module calls|Suggestion:Change '" ^ module_name ^ ":"
         ^ func_name ^ "(...)' to '" ^ module_name ^ "." ^ func_name
         ^ "(...)'|Context:Lx uses dot notation for module calls, not colon \
            notation")
    | expr :: rest -> transform (expr :: acc) rest
  in
  transform [] exprs

let emit_function_clause (func_name : string) (clause : function_clause) :
    string =
  let ctx = create_scope None in
  (* Add pattern variables to scope *)
  List.iter
    (function PVar name -> ignore (add_var_to_scope ctx name) | _ -> ())
    clause.params;
  let pattern_strings = List.map (emit_pattern ctx) clause.params in
  let guard_str = match clause.guard with
    | Some guard -> " when " ^ emit_guard_expr ctx guard
    | None -> ""
  in
  func_name ^ "("
  ^ String.concat ", " pattern_strings
  ^ ")" ^ guard_str ^ " ->\n    " ^ emit_expr ctx clause.body

let emit_function_def (func : function_def) : string =
  check_function_name func.name;
  let clauses_code = List.map (emit_function_clause func.name) func.clauses in
  String.concat ";\n" clauses_code ^ "."

let emit_spec (spec : spec) : string =
  let ctx = create_scope None in
  let emit_spec_expr expr =
    match expr with
    | App (Var "in", [ Var var; list_expr ]) ->
        var ^ " in " ^ emit_expr ctx list_expr
    | App (Var "matches", [ Var result; pattern_expr ]) ->
        result ^ " matches " ^ emit_expr ctx pattern_expr
    | _ -> emit_expr ctx expr
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
  let ctx = create_scope None in
  "test_"
  ^ String.map (function ' ' -> '_' | c -> c) test.name
  ^ "() ->\n    " ^ emit_expr ctx test.body ^ "."

let emit_describe_block (desc : describe_block) : string =
  let test_functions = List.map emit_test_def desc.tests in
  "% Test suite: " ^ desc.name ^ "\n" ^ String.concat "\n\n" test_functions

(* Helper function to extract children list from children_spec *)
let get_children_list = function
  | SimpleChildren children -> children
  | TypedChildren { workers; supervisors } -> workers @ supervisors

(* Generate OTP component code *)
let emit_otp_component (component : otp_component) (base_module_name : string) :
    string * string =
  match component with
  | Worker { name; functions; specs; position = _ } ->
      let module_name = base_module_name ^ "_" ^ name ^ "_worker" in

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

      (* Always include start_link and OTP callbacks *)
      let otp_exports =
        [ "start_link/0" ]
        @ List.map
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

      let all_exports = otp_exports @ !public_functions in
      (* Remove duplicates from exports *)
      let unique_exports = List.sort_uniq String.compare all_exports in
      let exports_str = String.concat ", " unique_exports in

      let header =
        "-module(" ^ module_name ^ ").\n-behaviour(gen_server).\n-export(["
        ^ exports_str ^ "]).\n\n"
        ^ "start_link() ->\n\
          \    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n\n"
      in

      (* Generate all functions, including OTP callbacks *)
      let function_code = List.map emit_function_def functions in

      (* Generate specs *)
      let spec_code = List.map emit_spec specs in

      let all_code = spec_code @ function_code in
      (module_name, header ^ String.concat "\n\n" all_code)
  | Supervisor { name; strategy; children; position = _ } ->
      let module_name =
        match name with
        | Some n -> base_module_name ^ "_" ^ n ^ "_supervisor"
        | None -> base_module_name ^ "_supervisor"
      in
      let header =
        "-module(" ^ module_name
        ^ ").\n-behaviour(supervisor).\n-export([start_link/0, init/1]).\n\n"
        ^ "start_link() ->\n\
          \    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n"
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
            let child_module = base_module_name ^ "_" ^ child ^ "_worker" in
            "#{id => " ^ child_module ^ ", start => {" ^ child_module
            ^ ", start_link, []}}")
          (get_children_list children)
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
  | Application _ -> "% Application definition handled separately"

(* Helper function to check if string contains substring *)
let string_contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec search i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else search (i + 1)
  in
  search 0

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
  | Failure msg when string_contains_substring msg "Enhanced:" ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      (* Parse enhanced error format directly *)
      let parts = String.split_on_char '|' msg in
      let parse_part part =
        match String.split_on_char ':' part with
        | key :: value_parts ->
            (String.trim key, String.concat ":" value_parts |> String.trim)
        | _ -> ("", part)
      in
      let parsed_parts = List.map parse_part parts in
      let get_value key =
        try Some (List.assoc key parsed_parts) with Not_found -> None
      in
      let message =
        match get_value "Enhanced" with Some m -> m | None -> msg
      in
      let suggestion = get_value "Suggestion" in
      let context = get_value "Context" in
      let position = Error.make_position ~filename line col in
      let error =
        Error.make_error_with_position ~suggestion ~context
          (Error.SyntaxError message) position message
      in
      raise (Error.CompilationError error)
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
  | Failure msg when string_contains_substring msg "Enhanced:" ->
      close_in ic;
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      (* Parse enhanced error format directly *)
      let parts = String.split_on_char '|' msg in
      let parse_part part =
        match String.split_on_char ':' part with
        | key :: value_parts ->
            (String.trim key, String.concat ":" value_parts |> String.trim)
        | _ -> ("", part)
      in
      let parsed_parts = List.map parse_part parts in
      let get_value key =
        try Some (List.assoc key parsed_parts) with Not_found -> None
      in
      let message =
        match get_value "Enhanced" with Some m -> m | None -> msg
      in
      let suggestion = get_value "Suggestion" in
      let context = get_value "Context" in
      let position = Error.make_position ~filename:(Some filename) line col in
      let error =
        Error.make_error_with_position ~suggestion ~context
          (Error.SyntaxError message) position message
      in
      raise (Error.CompilationError error)
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
    (base_module_name : string) ?(filename : string option = None) () :
    (string * string) list =
  (* Linting phase - must pass before any other validation *)
  (try Linter.lint_program program
   with Linter.LintError errors ->
     Printf.eprintf "Lint Errors:\n%s\n" (Linter.string_of_lint_errors errors);
     failwith "Linting failed - compilation aborted");

  (* Type checking phase *)
  (try
     let _ = Typechecker.type_check_program program in
     (* OTP validation phase *)
     try Otp_validator.validate_program program filename
     with Otp_validator.OtpValidationError error ->
       Printf.eprintf "%s\n" (Otp_validator.string_of_otp_error error);
       failwith "OTP validation failed"
   with
  | Error.CompilationError _ as e -> raise e
  | Typechecker.TypeError error ->
      Printf.eprintf "Type Error: %s\n" (Typechecker.string_of_type_error error);
      failwith "Type checking failed");

  (* Separate OTP components from regular items *)
  let otp_components, regular_items =
    List.partition
      (function OtpComponent _ -> true | _ -> false)
      program.items
  in

  (* Generate main module *)
  (* Collect public functions from regular items *)
  let public_functions = ref [] in
  List.iter
    (function
      | Function func when func.visibility = Public ->
          let arity =
            match func.clauses with
            | [] -> 0
            | clause :: _ -> List.length clause.params
          in
          public_functions :=
            (func.name ^ "/" ^ string_of_int arity) :: !public_functions
      | _ -> ())
    regular_items;

  let main_header =
    if List.length !public_functions > 0 then
      let exports_str = String.concat ", " !public_functions in
      "-module(" ^ base_module_name ^ ").\n-export([" ^ exports_str ^ "]).\n\n"
    else "-module(" ^ base_module_name ^ ").\n\n"
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
            emit_otp_component component base_module_name
        | _ -> failwith "Expected OTP component")
      otp_components
  in

  main_module :: otp_modules

let compile_to_string (program : program) : string =
  let modules = compile_to_string_with_module_name program "generated" () in
  (* Return only the main module for backward compatibility *)
  snd (List.hd modules)

(* Type check a program without compilation *)
let type_check_program (program : program) : Typechecker.type_env =
  (* Linting phase - must pass before type checking *)
  (try Linter.lint_program program
   with Linter.LintError errors ->
     Printf.eprintf "Lint Errors:\n%s\n" (Linter.string_of_lint_errors errors);
     failwith "Linting failed - type checking aborted");

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
       Otp_validator.validate_program program None;
       Printf.printf "\nOTP validation completed successfully.\n"
     with Otp_validator.OtpValidationError error ->
       Printf.eprintf "%s\n" (Otp_validator.string_of_otp_error error);
       failwith "OTP validation failed");

    type_env
  with
  | Error.CompilationError _ as e -> raise e
  | Typechecker.TypeError error ->
      Printf.eprintf "Type Error: %s\n" (Typechecker.string_of_type_error error);
      failwith "Type checking failed"

(* Type check a file *)
let type_check_file (filename : string) : Typechecker.type_env =
  let program = parse_file filename in
  type_check_program program

(* Clean up any existing build artifacts for the given file *)
let cleanup_build_artifacts build_dir base_name =
  let project_build_dir = Filename.concat build_dir base_name in

  (* Remove project directory if it exists (handles both app and non-app structures) *)
  if Sys.file_exists project_build_dir then (
    Printf.printf "Cleaning up old build artifacts for %s...\n" base_name;

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
      | Sys_error _ -> () (* Silently ignore file not found errors *)
      | Unix.Unix_error _ -> ()
      | exn ->
          Printf.eprintf "Warning: Unexpected error removing %s: %s\n" dir
            (Printexc.to_string exn)
    in

    (* Try to remove using system command as more reliable fallback *)
    let try_system_remove () =
      let cmd = Printf.sprintf "rm -rf %s" (Filename.quote project_build_dir) in
      let status = Unix.system cmd in
      match status with WEXITED 0 -> true | _ -> false
    in

    (* Use system command first for more reliable cleanup, then OCaml as fallback *)
    if not (try_system_remove ()) then
      try remove_dir project_build_dir
      with _ ->
        Printf.eprintf
          "Warning: Could not completely remove old build directory: %s\n"
          project_build_dir)

let compile_file ?(skip_rebar = false) (filename : string) : unit =
  let program = parse_file filename in
  let base_name = Filename.remove_extension (Filename.basename filename) in
  let source_dir = Filename.dirname filename in

  (* Check if there's an application definition *)
  let has_application = App_generator.find_application_def program <> None in

  (* Create _build directory in the source directory *)
  let build_dir = Filename.concat source_dir "_build" in
  if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o755;

  (* Clean up any existing build artifacts before compilation *)
  cleanup_build_artifacts build_dir base_name;

  if has_application then
    (* If there's an application definition, let App_generator handle everything *)
    match App_generator.find_application_def program with
    | Some app_def ->
        (* Generate application files inside _build directory *)
        App_generator.generate_application_files ~skip_rebar build_dir filename
          program app_def;
        Printf.printf "Generated application files for %s in _build/%s\n"
          base_name base_name
    | None -> () (* This case should not happen *)
  else
    (* No application definition, generate individual modules in _build/filename/ *)
    let project_build_dir = Filename.concat build_dir base_name in
    if not (Sys.file_exists project_build_dir) then
      Unix.mkdir project_build_dir 0o755;

    let modules =
      compile_to_string_with_module_name program base_name
        ~filename:(Some filename) ()
    in

    (* Write each module to its own file in the build directory *)
    List.iter
      (fun (module_name, module_content) ->
        let output_filename =
          Filename.concat project_build_dir (module_name ^ ".erl")
        in
        let oc = open_out output_filename in
        output_string oc module_content;
        close_out oc;
        Printf.printf "Generated module: %s\n" output_filename)
      modules;

    Printf.printf "Compiled %s in _build/%s/\n" filename base_name
