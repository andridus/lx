open Ast

(* BEAM file analysis structures *)
type beam_function = {
  name : string;
  arity : int;
  exported : bool;
  type_info : (type_expr list * type_expr) option; (* (params, return) *)
}

type beam_module = {
  name : string;
  functions : beam_function list;
  attributes : (string * string) list;
}

(* Exception for BEAM analysis errors *)
exception BeamAnalysisError of string

(* Built-in module definitions for common Erlang modules *)
let erlang_module =
  {
    name = "erlang";
    functions =
      [
        {
          name = "system_time";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "integer");
        };
        {
          name = "system_time";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "atom" ], TypeName "integer");
        };
        {
          name = "unique_integer";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "integer");
        };
        {
          name = "unique_integer";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeList (TypeName "atom") ], TypeName "integer");
        };
        {
          name = "byte_size";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "binary" ], TypeName "integer");
        };
        {
          name = "length";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeList (TypeName "any") ], TypeName "integer");
        };
        {
          name = "node";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "atom");
        };
        {
          name = "self";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "pid");
        };
        {
          name = "spawn";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "fun" ], TypeName "pid");
        };
        {
          name = "spawn";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "atom"; TypeList (TypeName "any") ],
                TypeName "pid" );
        };
        {
          name = "send";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "pid"; TypeName "any" ], TypeName "any");
        };
        {
          name = "exit";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "no_return");
        };
        {
          name = "exit";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "pid"; TypeName "any" ], TypeName "boolean");
        };
        {
          name = "process_flag";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "atom"; TypeName "any" ], TypeName "any");
        };
        {
          name = "register";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "atom"; TypeName "pid" ], TypeName "boolean");
        };
        {
          name = "unregister";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "atom" ], TypeName "boolean");
        };
        {
          name = "whereis";
          arity = 1;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom" ],
                TypeUnion (TypeName "pid", TypeName "undefined") );
        };
        {
          name = "is_atom";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_binary";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_boolean";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_float";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_function";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_integer";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_list";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_number";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_pid";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "is_tuple";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
      ];
    attributes = [];
  }

let lists_module =
  {
    name = "lists";
    functions =
      [
        { name = "map"; arity = 2; exported = true; type_info = None };
        (* Complex polymorphic type *)
        { name = "filter"; arity = 2; exported = true; type_info = None };
        { name = "foldl"; arity = 3; exported = true; type_info = None };
        { name = "foldr"; arity = 3; exported = true; type_info = None };
        {
          name = "reverse";
          arity = 1;
          exported = true;
          type_info =
            Some ([ TypeList (TypeName "any") ], TypeList (TypeName "any"));
        };
        {
          name = "append";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeList (TypeName "any"); TypeList (TypeName "any") ],
                TypeList (TypeName "any") );
        };
        {
          name = "member";
          arity = 2;
          exported = true;
          type_info =
            Some
              ([ TypeName "any"; TypeList (TypeName "any") ], TypeName "boolean");
        };
        {
          name = "keyfind";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [
                  TypeName "any";
                  TypeName "integer";
                  TypeList (TypeTuple [ TypeName "any" ]);
                ],
                TypeUnion (TypeTuple [ TypeName "any" ], TypeName "false") );
        };
        {
          name = "sort";
          arity = 1;
          exported = true;
          type_info =
            Some ([ TypeList (TypeName "any") ], TypeList (TypeName "any"));
        };
        {
          name = "usort";
          arity = 1;
          exported = true;
          type_info =
            Some ([ TypeList (TypeName "any") ], TypeList (TypeName "any"));
        };
        {
          name = "flatten";
          arity = 1;
          exported = true;
          type_info =
            Some ([ TypeList (TypeName "any") ], TypeList (TypeName "any"));
        };
        {
          name = "zip";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeList (TypeName "any"); TypeList (TypeName "any") ],
                TypeList (TypeTuple [ TypeName "any"; TypeName "any" ]) );
        };
        {
          name = "unzip";
          arity = 1;
          exported = true;
          type_info =
            Some
              ( [ TypeList (TypeTuple [ TypeName "any"; TypeName "any" ]) ],
                TypeTuple
                  [ TypeList (TypeName "any"); TypeList (TypeName "any") ] );
        };
      ];
    attributes = [];
  }

let crypto_module =
  {
    name = "crypto";
    functions =
      [
        {
          name = "strong_rand_bytes";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "integer" ], TypeName "binary");
        };
        {
          name = "rand_bytes";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "integer" ], TypeName "binary");
        };
        {
          name = "hash";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "atom"; TypeName "binary" ], TypeName "binary");
        };
        {
          name = "hmac";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "binary"; TypeName "binary" ],
                TypeName "binary" );
        };
        {
          name = "block_encrypt";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "binary"; TypeName "binary" ],
                TypeName "binary" );
        };
        {
          name = "block_decrypt";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "binary"; TypeName "binary" ],
                TypeName "binary" );
        };
      ];
    attributes = [];
  }

let mnesia_module =
  {
    name = "mnesia";
    functions =
      [
        {
          name = "transaction";
          arity = 1;
          exported = true;
          type_info =
            Some
              ([ TypeName "fun" ], TypeTuple [ TypeName "atom"; TypeName "any" ]);
        };
        {
          name = "read";
          arity = 1;
          exported = true;
          type_info =
            Some
              ( [ TypeTuple [ TypeName "atom"; TypeName "any" ] ],
                TypeList (TypeTuple [ TypeName "any" ]) );
        };
        {
          name = "write";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeTuple [ TypeName "any" ] ], TypeName "ok");
        };
        {
          name = "delete";
          arity = 1;
          exported = true;
          type_info =
            Some
              ([ TypeTuple [ TypeName "atom"; TypeName "any" ] ], TypeName "ok");
        };
        {
          name = "create_schema";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeList (TypeName "atom") ], TypeName "ok");
        };
        {
          name = "start";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "ok");
        };
        {
          name = "stop";
          arity = 0;
          exported = true;
          type_info = Some ([], TypeName "stopped");
        };
        {
          name = "create_table";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [
                  TypeName "atom";
                  TypeList (TypeTuple [ TypeName "atom"; TypeName "any" ]);
                ],
                TypeTuple [ TypeName "atom"; TypeName "any" ] );
        };
      ];
    attributes = [];
  }

let gen_server_module =
  {
    name = "gen_server";
    functions =
      [
        {
          name = "start_link";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "any"; TypeList (TypeName "any") ],
                TypeTuple [ TypeName "atom"; TypeName "pid" ] );
        };
        {
          name = "start_link";
          arity = 4;
          exported = true;
          type_info =
            Some
              ( [
                  TypeName "atom";
                  TypeName "atom";
                  TypeName "any";
                  TypeList (TypeName "any");
                ],
                TypeTuple [ TypeName "atom"; TypeName "pid" ] );
        };
        {
          name = "call";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "pid"; TypeName "any" ], TypeName "any");
        };
        {
          name = "call";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "pid"; TypeName "any"; TypeName "integer" ],
                TypeName "any" );
        };
        {
          name = "cast";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "pid"; TypeName "any" ], TypeName "ok");
        };
        {
          name = "reply";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "any"; TypeName "any" ], TypeName "ok");
        };
        {
          name = "stop";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "pid" ], TypeName "ok");
        };
      ];
    attributes = [];
  }

let supervisor_module =
  {
    name = "supervisor";
    functions =
      [
        {
          name = "start_link";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "any" ],
                TypeTuple [ TypeName "atom"; TypeName "pid" ] );
        };
        {
          name = "start_link";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "atom"; TypeName "atom"; TypeName "any" ],
                TypeTuple [ TypeName "atom"; TypeName "pid" ] );
        };
        {
          name = "start_child";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "pid"; TypeName "any" ],
                TypeTuple [ TypeName "atom"; TypeName "any" ] );
        };
        {
          name = "terminate_child";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "pid"; TypeName "any" ], TypeName "ok");
        };
        {
          name = "restart_child";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "pid"; TypeName "any" ],
                TypeTuple [ TypeName "atom"; TypeName "any" ] );
        };
        {
          name = "delete_child";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "pid"; TypeName "any" ], TypeName "ok");
        };
        {
          name = "which_children";
          arity = 1;
          exported = true;
          type_info =
            Some
              ( [ TypeName "pid" ],
                TypeList
                  (TypeTuple
                     [
                       TypeName "any";
                       TypeName "pid";
                       TypeName "atom";
                       TypeList (TypeName "atom");
                     ]) );
        };
      ];
    attributes = [];
  }

let timer_module =
  {
    name = "timer";
    functions =
      [
        {
          name = "sleep";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "integer" ], TypeName "ok");
        };
        {
          name = "send_after";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "integer"; TypeName "pid"; TypeName "any" ],
                TypeTuple [ TypeName "ok"; TypeName "any" ] );
        };
        {
          name = "send_interval";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "integer"; TypeName "pid"; TypeName "any" ],
                TypeTuple [ TypeName "ok"; TypeName "any" ] );
        };
        {
          name = "cancel";
          arity = 1;
          exported = true;
          type_info =
            Some
              ([ TypeName "any" ], TypeTuple [ TypeName "ok"; TypeName "any" ]);
        };
        {
          name = "now_diff";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [
                  TypeTuple
                    [
                      TypeName "integer"; TypeName "integer"; TypeName "integer";
                    ];
                  TypeTuple
                    [
                      TypeName "integer"; TypeName "integer"; TypeName "integer";
                    ];
                ],
                TypeName "integer" );
        };
      ];
    attributes = [];
  }

let ets_module =
  {
    name = "ets";
    functions =
      [
        {
          name = "new";
          arity = 2;
          exported = true;
          type_info =
            Some
              ([ TypeName "atom"; TypeList (TypeName "atom") ], TypeName "any");
        };
        {
          name = "insert";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "any"; TypeName "any" ], TypeName "boolean");
        };
        {
          name = "lookup";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "any"; TypeName "any" ], TypeList (TypeName "any"));
        };
        {
          name = "delete";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "boolean");
        };
        {
          name = "delete";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "any"; TypeName "any" ], TypeName "boolean");
        };
        {
          name = "select";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "any"; TypeList (TypeName "any") ],
                TypeList (TypeName "any") );
        };
        {
          name = "tab2list";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeList (TypeName "any"));
        };
      ];
    attributes = [];
  }

let io_module =
  {
    name = "io";
    functions =
      [
        {
          name = "format";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "string" ], TypeName "ok");
        };
        {
          name = "format";
          arity = 2;
          exported = true;
          type_info =
            Some
              ([ TypeName "string"; TypeList (TypeName "any") ], TypeName "ok");
        };
        {
          name = "write";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "ok");
        };
        {
          name = "put_chars";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "string" ], TypeName "ok");
        };
        {
          name = "get_line";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "string" ], TypeName "string");
        };
        {
          name = "read";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "any"; TypeName "string" ], TypeName "any");
        };
      ];
    attributes = [];
  }

let file_module =
  {
    name = "file";
    functions =
      [
        {
          name = "open";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "string"; TypeList (TypeName "atom") ],
                TypeTuple [ TypeName "atom"; TypeName "any" ] );
        };
        {
          name = "close";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "any" ], TypeName "ok");
        };
        {
          name = "read";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "any"; TypeName "integer" ],
                TypeTuple [ TypeName "atom"; TypeName "binary" ] );
        };
        {
          name = "write";
          arity = 2;
          exported = true;
          type_info = Some ([ TypeName "any"; TypeName "binary" ], TypeName "ok");
        };
        {
          name = "read_file";
          arity = 1;
          exported = true;
          type_info =
            Some
              ( [ TypeName "string" ],
                TypeTuple [ TypeName "atom"; TypeName "binary" ] );
        };
        {
          name = "write_file";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "string"; TypeName "binary" ], TypeName "ok");
        };
      ];
    attributes = [];
  }

let string_module =
  {
    name = "string";
    functions =
      [
        {
          name = "length";
          arity = 1;
          exported = true;
          type_info = Some ([ TypeName "string" ], TypeName "integer");
        };
        {
          name = "concat";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "string"; TypeName "string" ], TypeName "string");
        };
        {
          name = "substr";
          arity = 2;
          exported = true;
          type_info =
            Some ([ TypeName "string"; TypeName "integer" ], TypeName "string");
        };
        {
          name = "substr";
          arity = 3;
          exported = true;
          type_info =
            Some
              ( [ TypeName "string"; TypeName "integer"; TypeName "integer" ],
                TypeName "string" );
        };
        {
          name = "tokens";
          arity = 2;
          exported = true;
          type_info =
            Some
              ( [ TypeName "string"; TypeName "string" ],
                TypeList (TypeName "string") );
        };
      ];
    attributes = [];
  }

(* Get built-in modules *)
let get_builtin_modules () =
  [
    ("erlang", erlang_module);
    ("lists", lists_module);
    ("crypto", crypto_module);
    ("mnesia", mnesia_module);
    ("gen_server", gen_server_module);
    ("supervisor", supervisor_module);
    ("timer", timer_module);
    ("ets", ets_module);
    ("io", io_module);
    ("file", file_module);
    ("string", string_module);
  ]

(* Extract information from BEAM file - placeholder implementation *)
let analyze_beam_file (beam_path : string) : beam_module =
  (* For now, we'll use a simple approach that checks if the file exists *)
  if not (Sys.file_exists beam_path) then
    raise (BeamAnalysisError ("BEAM file not found: " ^ beam_path))
  else
    (* Return a minimal module structure - in a real implementation,
       this would parse the BEAM file format *)
    let module_name =
      Filename.basename beam_path |> Filename.remove_extension
    in
    {
      name = module_name;
      functions = [];
      (* Would be populated from BEAM analysis *)
      attributes = [];
    }

(* Find function in module *)
let find_function (module_info : beam_module) (func_name : string) (arity : int)
    : beam_function option =
  let functions = module_info.functions in
  List.find_opt
    (fun (func : beam_function) ->
      func.name = func_name && func.arity = arity && func.exported)
    functions

(* Find BEAM file in standard locations *)
let find_beam_file (module_name : string) : string =
  let possible_paths =
    [
      (* Standard Erlang/OTP installation paths *)
      "/usr/lib/erlang/lib/kernel/ebin/" ^ module_name ^ ".beam";
      "/usr/lib/erlang/lib/stdlib/ebin/" ^ module_name ^ ".beam";
      "/usr/lib/erlang/lib/crypto/ebin/" ^ module_name ^ ".beam";
      "/usr/lib/erlang/lib/mnesia/ebin/" ^ module_name ^ ".beam";
      (* Local project paths *)
      "_build/default/lib/" ^ module_name ^ "/ebin/" ^ module_name ^ ".beam";
      "ebin/" ^ module_name ^ ".beam";
      (* Rebar3 paths *)
      "_build/default/lib/*/ebin/" ^ module_name ^ ".beam";
    ]
  in

  let rec find_existing = function
    | [] ->
        raise
          (BeamAnalysisError ("BEAM file not found for module: " ^ module_name))
    | path :: rest -> if Sys.file_exists path then path else find_existing rest
  in

  try find_existing possible_paths
  with BeamAnalysisError _ ->
    (* If not found in standard locations, assume it's a built-in module *)
    "/virtual/builtin/" ^ module_name ^ ".beam"

(* Load standard library modules *)
let load_stdlib_modules () : (string * beam_module) list =
  get_builtin_modules ()

(* Helper function to convert dependency to string for error messages *)
let string_of_dependency = function
  | Simple name -> name
  | Version (name, version) -> name ^ " (" ^ version ^ ")"
  | GitHub (name, repo) -> name ^ " (github:" ^ repo ^ ")"
  | Path (name, path) -> name ^ " (path:" ^ path ^ ")"
  | Hex (name, version) -> name ^ " (hex:" ^ version ^ ")"
