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
            guard = None;
          };
        ];
      visibility = Private;
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

let tests =
  [
    ("valid worker with init", `Quick, test_valid_worker_with_init);
    ("worker missing init", `Quick, test_worker_missing_init);
  ]
