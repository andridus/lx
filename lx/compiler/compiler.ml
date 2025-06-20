(* Module aliases for external access *)
module Ast = Ast
module Lexer = Lexer
module Parser = Parser

open Ast

(* Check for reserved words in function names *)
let reserved_words = [
  "test"; "spec"; "describe"; "worker"; "supervisor"; "strategy"; "children";
  "init"; "call"; "cast"; "info"; "terminate"; "requires"; "ensures"; "assert";
  "case"; "if"; "then"; "else"; "for"; "when"; "let"; "in"; "fun"; "matches"
]

let is_reserved_word word = List.mem word reserved_words

let check_function_name name =
  if is_reserved_word name then
    failwith ("Error: '" ^ name ^ "' is a reserved word and cannot be used as a function name.\nReserved words include: test, spec, describe, worker, supervisor, etc.\nTry using a different name like '" ^ name ^ "_func' or 'my_" ^ name ^ "'.")

let capitalize_var id =
  if String.length id > 0 then
    String.uppercase_ascii (String.sub id 0 1) ^ String.sub id 1 (String.length id - 1)
  else
    id

let emit_literal (l : literal) : string = match l with
  | LInt n -> string_of_int n
  | LFloat f -> string_of_float f
  | LString s -> "\"" ^ s ^ "\""
  | LBool true -> "true"
  | LBool false -> "false"
  | LAtom a -> a

let rec emit_pattern (p : pattern) : string = match p with
  | PWildcard -> "_"
  | PVar id -> capitalize_var id
  | PAtom a -> a
  | PLiteral l -> emit_literal l
  | PTuple ps -> "{" ^ String.concat ", " (List.map emit_pattern ps) ^ "}"
  | PList ps -> "[" ^ String.concat ", " (List.map emit_pattern ps) ^ "]"
  | PCons (head, tail) -> "[" ^ emit_pattern head ^ " | " ^ emit_pattern tail ^ "]"

let rec emit_expr (e : expr) : string = match e with
  | Literal l -> emit_literal l
  | Var id -> capitalize_var id
  | Let (id, value, body) ->
      "(" ^ capitalize_var id ^ " = " ^ emit_expr value ^ ", " ^ emit_expr body ^ ")"
  | Fun (params, body) ->
      "fun(" ^ String.concat ", " (List.map capitalize_var params) ^ ") -> " ^ emit_expr body ^ " end"
  | App (func, args) ->
      emit_expr func ^ "(" ^ String.concat ", " (List.map emit_expr args) ^ ")"
  | Tuple exprs ->
      "{" ^ String.concat ", " (List.map emit_expr exprs) ^ "}"
  | List exprs ->
      "[" ^ String.concat ", " (List.map emit_expr exprs) ^ "]"
  | If (cond, then_expr, else_expr) ->
      "case " ^ emit_expr cond ^ " of true -> " ^ emit_expr then_expr ^
      (match else_expr with
       | Some e -> "; _ -> " ^ emit_expr e
       | None -> "; _ -> ok") ^ " end"
  | Match (value, cases) ->
      "case " ^ emit_expr value ^ " of " ^
      String.concat "; " (List.map (fun (p, e) -> emit_pattern p ^ " -> " ^ emit_expr e) cases) ^
      " end"
  | For (_, _, _) -> "% For expressions not yet implemented"

let emit_function_def (func : function_def) : string =
  check_function_name func.name;
  func.name ^ "(" ^ String.concat ", " (List.map capitalize_var func.params) ^ ") ->\n    " ^
  emit_expr func.body ^ "."

let emit_spec (spec : spec) : string =
  let emit_spec_expr expr =
    match expr with
    | App (Var "in", [Var var; list_expr]) ->
        var ^ " in " ^ emit_expr list_expr
    | App (Var "matches", [Var result; pattern_expr]) ->
        result ^ " matches " ^ emit_expr pattern_expr
    | _ -> emit_expr expr
  in
  let requires_str =
    if List.length spec.requires > 0 then
      "\n%% @requires " ^ String.concat ", " (List.map emit_spec_expr spec.requires)
    else ""
  in
  let ensures_str =
    if List.length spec.ensures > 0 then
      "\n%% @ensures " ^ String.concat ", " (List.map emit_spec_expr spec.ensures)
    else ""
  in
  "%% Spec for " ^ spec.name ^ requires_str ^ ensures_str

let emit_test_def (test : test_def) : string =
  "test_" ^ String.map (function ' ' -> '_' | c -> c) test.name ^ "() ->\n    " ^
  emit_expr test.body ^ "."

let emit_describe_block (desc : describe_block) : string =
  let test_functions = List.map emit_test_def desc.tests in
  "% Test suite: " ^ desc.name ^ "\n" ^ String.concat "\n\n" test_functions

let emit_module_item (item : module_item) : string = match item with
  | Function func -> emit_function_def func
  | OtpComponent _ -> "% OTP components not yet implemented"
  | Spec spec -> emit_spec spec
  | Test desc -> emit_describe_block desc

let parse_string (content : string) : program =
  let lexbuf = Lexing.from_string content in
  Parser.main Lexer.read lexbuf

let parse_file (filename : string) : program =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let result = Parser.main Lexer.read lexbuf in
  close_in ic;
  result

let compile_to_string (program : program) : string =
  let module_name = "generated" in
  let header = "-module(" ^ module_name ^ ").\n-compile(export_all).\n\n" in
  let items = List.map emit_module_item program.items in
  header ^ String.concat "\n\n" items

let compile_file (filename : string) : unit =
  let program = parse_file filename in
  let output_content = compile_to_string program in
  let output_filename = Filename.remove_extension filename ^ ".erl" in
  let oc = open_out output_filename in
  output_string oc output_content;
  close_out oc;
  Printf.printf "Compiled %s -> %s\n" filename output_filename