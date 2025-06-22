(* Tests for the OTP Validator *)
open Alcotest
open Compiler.Ast
open Compiler.Otp_validator

let test_valid_worker_with_init () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    ()
  with OtpValidationError error ->
    fail ("Valid worker test failed: " ^ string_of_otp_error error)

let test_worker_missing_init () =
  let some_func =
    make_single_clause_function "other_function" [ "x" ] (Var "x")
  in

  let worker =
    Worker
      {
        name = "incomplete_worker";
        functions = [ some_func ];
        (* Missing init function *)
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    fail "Missing init test failed: should have thrown error"
  with
  | OtpValidationError (MissingRequiredCallback ("init", _, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Missing init test failed: wrong error: " ^ string_of_otp_error error)

let test_handle_call_invalid_arity () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let handle_call_func =
    make_single_clause_function "handle_call"
      [ "request"; "from" ] (* Wrong arity: should be 3 *)
      (Tuple
         [
           Literal (LAtom "reply");
           Literal (LAtom "ok");
           Literal (LAtom "state");
         ])
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func; handle_call_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    fail "Invalid handle_call arity test failed: should have thrown error"
  with
  | OtpValidationError (InvalidCallbackArity ("handle_call", _, 3, 2, _, _)) ->
      ()
  | OtpValidationError error ->
      fail
        ("Invalid handle_call arity test failed: wrong error: "
       ^ string_of_otp_error error)

let test_handle_call_valid_arity () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let handle_call_func =
    make_single_clause_function "handle_call"
      [ "request"; "from"; "state" ] (* Correct arity: 3 *)
      (Tuple
         [
           Literal (LAtom "reply");
           Literal (LAtom "ok");
           Literal (LAtom "state");
         ])
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func; handle_call_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    ()
  with OtpValidationError error ->
    fail ("Valid handle_call test failed: " ^ string_of_otp_error error)

let test_handle_cast_invalid_arity () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let handle_cast_func =
    make_single_clause_function "handle_cast"
      [ "request" ] (* Wrong arity: should be 2 *)
      (Tuple [ Literal (LAtom "noreply"); Literal (LAtom "state") ])
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func; handle_cast_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    fail "Invalid handle_cast arity test failed: should have thrown error"
  with
  | OtpValidationError (InvalidCallbackArity ("handle_cast", _, 2, 1, _, _)) ->
      ()
  | OtpValidationError error ->
      fail
        ("Invalid handle_cast arity test failed: wrong error: "
       ^ string_of_otp_error error)

let test_callback_non_tuple_return () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Literal (LAtom "not_a_tuple");
            position = None;
          };
        ];
      position = None;
    }
    (* Should return a tuple *)
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    fail "Non-tuple return test failed: should have thrown error"
  with
  | OtpValidationError (InvalidCallbackReturn ("init", _, _, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Non-tuple return test failed: wrong error: "
       ^ string_of_otp_error error)

let test_format_status_any_return () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let format_status_func =
    make_single_clause_function "format_status" [ "status" ]
      (Literal (LAtom "any_return_allowed"))
    (* format_status can return anything *)
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func; format_status_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    ()
  with OtpValidationError error ->
    fail ("Format status test failed: " ^ string_of_otp_error error)

let test_invalid_worker_name () =
  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let worker =
    Worker
      {
        name = "Invalid-Worker-Name";
        (* Invalid atom name *)
        functions = [ init_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent worker ] } in

  try
    validate_program program None;
    fail "Invalid worker name test failed: should have thrown error"
  with
  | OtpValidationError (InvalidWorkerName _) -> ()
  | OtpValidationError error ->
      fail
        ("Invalid worker name test failed: wrong error: "
       ^ string_of_otp_error error)

let test_valid_supervisor () =
  let supervisor =
    Supervisor
      {
        name = Some "test_supervisor";
        strategy = OneForOne;
        children = SimpleChildren [ "test_worker" ];
        position = None;
      }
  in

  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func ];
        specs = [];
        position = None;
      }
  in

  let program = { items = [ OtpComponent supervisor; OtpComponent worker ] } in

  try
    validate_program program None;
    ()
  with OtpValidationError error ->
    fail ("Valid supervisor test failed: " ^ string_of_otp_error error)

let test_unknown_child () =
  let supervisor =
    Supervisor
      {
        name = Some "test_supervisor";
        strategy = OneForOne;
        children = SimpleChildren [ "unknown_worker" ];
        position = None;
      }
  in

  let program = { items = [ OtpComponent supervisor ] } in

  try
    validate_program program None;
    fail "Unknown child test failed: should have thrown error"
  with
  | OtpValidationError (UnknownChild (_, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Unknown child test failed: wrong error: " ^ string_of_otp_error error)

let test_unknown_child_with_nonexistent_worker () =
  let supervisor =
    Supervisor
      {
        name = Some "test_supervisor";
        strategy = OneForOne;
        children = SimpleChildren [ "nonexistent_worker" ];
        position = None;
      }
  in

  let init_func =
    {
      name = "init";
      clauses =
        [
          {
            params = [ PVar "args" ];
            body = Tuple [ Literal (LAtom "ok"); Var "state" ];
            position = None;
          };
        ];
      position = None;
    }
  in

  let worker =
    Worker
      {
        name = "test_worker";
        functions = [ init_func ];
        specs = [];
        position = None;
      }
  in

  let supervisor2 =
    Supervisor
      {
        name = Some "test_supervisor2";
        strategy = OneForOne;
        children = SimpleChildren [ "another_nonexistent" ];
        position = None;
      }
  in

  let program =
    {
      items =
        [
          OtpComponent supervisor; OtpComponent worker; OtpComponent supervisor2;
        ];
    }
  in

  try
    validate_program program None;
    fail "Unknown child test failed: should have thrown error"
  with
  | OtpValidationError (UnknownChild (_, _)) -> ()
  | OtpValidationError error ->
      fail
        ("Unknown child test failed: wrong error: " ^ string_of_otp_error error)

let tests =
  [
    ("valid worker with init", `Quick, test_valid_worker_with_init);
    ("worker missing init", `Quick, test_worker_missing_init);
    ("handle_call invalid arity", `Quick, test_handle_call_invalid_arity);
    ("handle_call valid arity", `Quick, test_handle_call_valid_arity);
    ("handle_cast invalid arity", `Quick, test_handle_cast_invalid_arity);
    ("callback non-tuple return", `Quick, test_callback_non_tuple_return);
    ("format_status any return", `Quick, test_format_status_any_return);
    ("invalid worker name", `Quick, test_invalid_worker_name);
    ("valid supervisor", `Quick, test_valid_supervisor);
    ("unknown child", `Quick, test_unknown_child);
    ( "unknown child with nonexistent worker",
      `Quick,
      test_unknown_child_with_nonexistent_worker );
  ]
