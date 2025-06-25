open Compiler.Ast

(* Helper function to parse programs *)
let parse_program_string input =
  try Compiler.parse_string input
  with exn ->
    failwith ("Parse error in: " ^ input ^ " - " ^ Printexc.to_string exn)

(* Helper function to compile programs *)
let compile_program program = Compiler.compile_to_string_for_tests program

(* Test basic map creation with atom keys *)
let test_map_creation_atom_keys () =
  let input = "def test() do %{ name: \"Alice\", age: 25, active: true } end" in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map creation with atom keys: %s\n" compiled;
    (* Check if the compiled output contains map syntax *)
    if String.contains compiled '#' then
      Printf.printf "  Contains map syntax ✓\n"
    else failwith ("Compiled output should contain map syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map creation with atom keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map creation with atom keys"

(* Test map creation with general keys *)
let test_map_creation_general_keys () =
  let input =
    "def test() do %{ \"database_url\" => \"localhost\", \"port\" => 5432 } end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map creation with general keys: %s\n" compiled;
    (* Check if the compiled output contains map syntax with => *)
    if String.contains compiled '#' then
      Printf.printf "  Contains map syntax ✓\n"
    else failwith ("Compiled output should contain map syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map creation with general keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map creation with general keys"

(* Test mixed key types in maps *)
let test_map_mixed_keys () =
  let input =
    "def test() do %{ :type => \"user\", \"created_at\" => \"2023-01-01\", 1 \
     => \"first\" } end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map creation with mixed keys: %s\n" compiled;
    if String.contains compiled '#' then
      Printf.printf "  Contains map syntax ✓\n"
    else failwith ("Compiled output should contain map syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map creation with mixed keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map creation with mixed keys"

(* Test empty map creation *)
let test_empty_map () =
  let input = "def test() do %{} end" in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Empty map creation: %s\n" compiled;
    if String.contains compiled '#' then
      Printf.printf "  Contains map syntax ✓\n"
    else failwith ("Compiled output should contain map syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Empty map creation failed: %s\n" (Printexc.to_string exn);
    failwith "Failed to parse/compile empty map"

(* Test map access with atom keys *)
let test_map_access_atom_keys () =
  let input =
    "def test() do user = %{ name: \"Alice\", age: 25 }; user[:name] end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map access with atom keys: %s\n" compiled;
    (* Check if the compiled output contains map access syntax *)
    if String.contains compiled '#' then
      Printf.printf "  Contains map access syntax ✓\n"
    else
      failwith ("Compiled output should contain map access syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map access with atom keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map access with atom keys"

(* Test map access with general keys *)
let test_map_access_general_keys () =
  let input =
    "def test() do config = %{ \"port\" => 5432 }; config[\"port\"] end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map access with general keys: %s\n" compiled;
    if String.contains compiled '#' then
      Printf.printf "  Contains map access syntax ✓\n"
    else
      failwith ("Compiled output should contain map access syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map access with general keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map access with general keys"

(* Test basic map pattern matching *)
let test_map_pattern_matching_basic () =
  let input =
    "def test() do user = %{ name: \"Alice\", age: 25 }; %{ name: user_name, \
     age: _user_age } <- user; user_name end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Basic map pattern matching: %s\n" compiled;
    (* Check if the compiled output contains pattern matching syntax *)
    if String.contains compiled ':' then
      Printf.printf "  Contains pattern matching syntax ✓\n"
    else
      failwith
        ("Compiled output should contain pattern matching syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Basic map pattern matching failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile basic map pattern matching"

(* Test map pattern matching with mixed keys *)
let test_map_pattern_matching_mixed () =
  let input =
    "def test() do data = %{ :status => \"ok\", \"message\" => \"success\" }; \
     %{ :status => status, \"message\" => _msg } <- data; status end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map pattern matching with mixed keys: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains pattern matching syntax ✓\n"
    else
      failwith
        ("Compiled output should contain pattern matching syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map pattern matching with mixed keys failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map pattern matching with mixed keys"

(* Test partial map pattern matching *)
let test_map_pattern_matching_partial () =
  let input =
    "def test() do user = %{ name: \"Alice\", age: 25, city: \"NYC\" }; %{ \
     name: user_name } <- user; user_name end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Partial map pattern matching: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains pattern matching syntax ✓\n"
    else
      failwith
        ("Compiled output should contain pattern matching syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Partial map pattern matching failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile partial map pattern matching"

(* Test map pattern matching in function parameters *)
let test_map_pattern_in_function () =
  let input =
    "def process_user(%{ name: user_name, age: user_age }) do if user_age >= \
     18 do %{ name: user_name, status: :adult } else %{ name: user_name, \
     status: :minor } end end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map pattern in function parameters: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains pattern matching syntax ✓\n"
    else
      failwith
        ("Compiled output should contain pattern matching syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map pattern in function parameters failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map pattern in function parameters"

(* Test map pattern matching in case expressions *)
let test_map_pattern_in_case () =
  let input =
    "def handle_response(response) do case response do %{ status: :ok, data: \
     data } -> data %{ status: :error, message: msg } -> .{:error, msg} _ -> \
     :unknown end end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map pattern in case expressions: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains pattern matching syntax ✓\n"
    else
      failwith
        ("Compiled output should contain pattern matching syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map pattern in case expressions failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map pattern in case expressions"

(* Test nested map patterns *)
let test_nested_map_patterns () =
  let input =
    "def test() do data = %{ user: %{ name: \"Alice\", profile: %{ age: 25 } } \
     }; %{ user: %{ name: user_name, profile: %{ age: _user_age } } } <- data; \
     user_name end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Nested map patterns: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains nested pattern syntax ✓\n"
    else
      failwith
        ("Compiled output should contain nested pattern syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Nested map patterns failed: %s\n" (Printexc.to_string exn);
    failwith "Failed to parse/compile nested map patterns"

(* Test map pattern matching with guards *)
let test_map_pattern_with_guards () =
  let input =
    "def validate_user(user) do case user do %{ age: age, name: name } when \
     age >= 18 -> %{ name: name, status: :adult } %{ age: age, name: name } \
     when age < 18 -> %{ name: name, status: :minor } _ -> :invalid end end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Map pattern with guards: %s\n" compiled;
    if String.contains compiled ':' then
      Printf.printf "  Contains guard pattern syntax ✓\n"
    else
      failwith
        ("Compiled output should contain guard pattern syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Map pattern with guards failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile map pattern with guards"

(* Test complex map with lists and tuples *)
let test_complex_map_structures () =
  let input =
    "def test() do %{ users: [%{ name: \"Alice\" }, %{ name: \"Bob\" }], \
     config: %{ timeout: 5000 }, metadata: .{:version, \"1.0.0\"} } end"
  in
  try
    let program = parse_program_string input in
    let compiled = compile_program program in
    Printf.printf "✓ Complex map structures: %s\n" compiled;
    if String.contains compiled '#' then
      Printf.printf "  Contains complex map syntax ✓\n"
    else
      failwith ("Compiled output should contain complex map syntax: " ^ compiled)
  with exn ->
    Printf.printf "✗ Complex map structures failed: %s\n"
      (Printexc.to_string exn);
    failwith "Failed to parse/compile complex map structures"

(* Test AST structure for map creation *)
let test_map_ast_structure () =
  let input = "def test() do %{ name: \"Alice\", age: 25 } end" in
  try
    let program = parse_program_string input in
    match program.items with
    | [ Function { clauses = [ { body = MapCreate _; _ } ]; _ } ] ->
        Printf.printf "✓ Map AST structure is correct\n"
    | _ ->
        Printf.printf "✗ Expected MapCreate AST structure\n";
        failwith "Map AST structure is incorrect"
  with exn ->
    Printf.printf "✗ Map AST test failed: %s\n" (Printexc.to_string exn);
    failwith "Failed to parse map AST structure test"

(* Test AST structure for map pattern matching *)
let test_map_pattern_ast_structure () =
  let input = "def test() do %{ name: user_name } <- user_map; user_name end" in
  try
    let program = parse_program_string input in
    match program.items with
    | [
     Function
       {
         clauses =
           [ { body = Sequence [ PatternMatch (PMap _, _, _, _); _ ]; _ } ];
         _;
       };
    ] ->
        Printf.printf "✓ Map pattern AST structure is correct\n"
    | _ ->
        Printf.printf "✗ Expected PMap pattern AST structure\n";
        failwith "Map pattern AST structure is incorrect"
  with exn ->
    Printf.printf "✗ Map pattern AST test failed: %s\n" (Printexc.to_string exn);
    failwith "Failed to parse map pattern AST structure test"

(* Export tests for main test runner *)
let tests =
  [
    ( "Map Creation - Atom Keys",
      `Quick,
      fun () -> test_map_creation_atom_keys () );
    ( "Map Creation - General Keys",
      `Quick,
      fun () -> test_map_creation_general_keys () );
    ("Map Creation - Mixed Keys", `Quick, fun () -> test_map_mixed_keys ());
    ("Empty Map Creation", `Quick, fun () -> test_empty_map ());
    ("Map Access - Atom Keys", `Quick, fun () -> test_map_access_atom_keys ());
    ( "Map Access - General Keys",
      `Quick,
      fun () -> test_map_access_general_keys () );
    ( "Map Pattern Matching - Basic",
      `Quick,
      fun () -> test_map_pattern_matching_basic () );
    ( "Map Pattern Matching - Mixed Keys",
      `Quick,
      fun () -> test_map_pattern_matching_mixed () );
    ( "Map Pattern Matching - Partial",
      `Quick,
      fun () -> test_map_pattern_matching_partial () );
    ( "Map Pattern in Functions",
      `Quick,
      fun () -> test_map_pattern_in_function () );
    ("Map Pattern in Case", `Quick, fun () -> test_map_pattern_in_case ());
    ("Nested Map Patterns", `Quick, fun () -> test_nested_map_patterns ());
    ( "Map Pattern with Guards",
      `Quick,
      fun () -> test_map_pattern_with_guards () );
    ("Complex Map Structures", `Quick, fun () -> test_complex_map_structures ());
    ("Map AST Structure", `Quick, fun () -> test_map_ast_structure ());
    ( "Map Pattern AST Structure",
      `Quick,
      fun () -> test_map_pattern_ast_structure () );
  ]
