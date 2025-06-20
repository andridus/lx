(* Tests for the OTP Validator *)
open Alcotest
open Compiler.Ast
open Compiler.Typechecker
open Compiler.Otp_validator

let test_valid_worker () =
  let init_handler = make_single_clause_function "init" [ "args" ]
    (App (Var "reply", [ Tuple [ Var "ok"; Literal (LAtom "state") ] ])) in

  let call_handler = make_single_clause_function "handle_call" [ "msg"; "from"; "state" ]
    (App (Var "reply", [ Var "msg"; Var "state" ])) in

  let worker =
    Worker
      {
        name = "test_worker";
        handlers = [ (Init, init_handler); (Call, call_handler) ];
        functions = [];
        specs = [];
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    let env = type_check_program program in
    validate_program program env;
    ()
  with
  | OtpValidationError error ->
      fail ("Valid worker test failed: " ^ string_of_otp_error error)
  | TypeError error ->
      fail
        ("Valid worker test failed (type error): " ^ string_of_type_error error)

let test_missing_handler () =
  let init_handler = make_single_clause_function "init" [ "args" ]
    (App (Var "reply", [ Tuple [ Var "ok"; Literal (LAtom "state") ] ])) in

  let worker =
    Worker
      {
        name = "incomplete_worker";
        handlers = [ (Init, init_handler) ];
        (* Missing Call handler *)
        functions = [];
        specs = [];
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    let env = type_check_program program in
    validate_program program env;
    fail "Missing handler test failed: should have thrown error"
  with
  | OtpValidationError (MissingRequiredHandler (Call, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Missing handler test failed: wrong error: "
       ^ string_of_otp_error error)
  | TypeError _ -> fail "Missing handler test failed: type error occurred"

let test_invalid_worker_name () =
  let init_handler = make_single_clause_function "init" [ "args" ]
    (App (Var "reply", [ Tuple [ Var "ok"; Literal (LAtom "state") ] ])) in

  let call_handler = make_single_clause_function "handle_call" [ "msg"; "from"; "state" ]
    (App (Var "reply", [ Var "msg"; Var "state" ])) in

  let worker =
    Worker
      {
        name = "Invalid-Worker-Name";
        (* Invalid atom name *)
        handlers = [ (Init, init_handler); (Call, call_handler) ];
        functions = [];
        specs = [];
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    let env = type_check_program program in
    validate_program program env;
    fail "Invalid worker name test failed: should have thrown error"
  with
  | OtpValidationError (InvalidWorkerName _) -> ()
  | OtpValidationError error ->
      fail
        ("Invalid worker name test failed: wrong error: "
       ^ string_of_otp_error error)
  | TypeError _ -> fail "Invalid worker name test failed: type error occurred"

let test_valid_supervisor () =
  let supervisor =
    Supervisor
      {
        name = "test_supervisor";
        strategy = OneForOne;
        children = [ "test_worker" ];
      }
  in

  let init_handler = make_single_clause_function "init" [ "args" ]
    (App (Var "reply", [ Tuple [ Var "ok"; Literal (LAtom "state") ] ])) in

  let call_handler = make_single_clause_function "handle_call" [ "msg"; "from"; "state" ]
    (App (Var "reply", [ Var "msg"; Var "state" ])) in

  let worker =
    Worker
      {
        name = "test_worker";
        handlers = [ (Init, init_handler); (Call, call_handler) ];
        functions = [];
        specs = [];
      }
  in

  let program = { items = [ OtpComponent supervisor; OtpComponent worker ] } in

  try
    let env = type_check_program program in
    validate_program program env;
    ()
  with
  | OtpValidationError error ->
      fail ("Valid supervisor test failed: " ^ string_of_otp_error error)
  | TypeError error ->
      fail
        ("Valid supervisor test failed (type error): "
       ^ string_of_type_error error)

let test_unknown_child () =
  let supervisor =
    Supervisor
      {
        name = "test_supervisor";
        strategy = OneForOne;
        children = [ "unknown_worker" ];
        (* Worker doesn't exist *)
      }
  in

  let program = { items = [ OtpComponent supervisor ] } in

  try
    let env = type_check_program program in
    validate_program program env;
    fail "Unknown child test failed: should have thrown error"
  with
  | OtpValidationError (UnknownChild (_, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Unknown child test failed: wrong error: " ^ string_of_otp_error error)
  | TypeError _ -> fail "Unknown child test failed: type error occurred"

let tests =
  [
    ("valid worker", `Quick, test_valid_worker);
    ("missing handler", `Quick, test_missing_handler);
    ("invalid worker name", `Quick, test_invalid_worker_name);
    ("valid supervisor", `Quick, test_valid_supervisor);
    ("unknown child", `Quick, test_unknown_child);
  ]