open Alcotest
open Compiler.Ast

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

(* Helper function to check if string matches regex pattern *)
let string_matches_pattern s pattern =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

(* Test successful record update with normal variable name *)
let test_record_update_normal_variable () =
  let record_def =
    {
      record_name = "Person";
      fields =
        [
          {
            field_name = "name";
            field_type = TypeName "string";
            default_value = None;
          };
          {
            field_name = "age";
            field_type = TypeName "integer";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let person_create =
    RecordCreate
      ( "Person",
        [ ("name", Literal (LString "John")); ("age", Literal (LInt 30)) ] )
  in

  let person_update =
    RecordUpdate (Var "person", [ ("name", Literal (LString "Jane")) ])
  in

  let assign1 = Assign ("person", person_create, None) in
  let assign2 = Assign ("person2", person_update, None) in

  let func =
    make_single_clause_function "test_person_update" []
      (Sequence [ assign1; assign2; Var "person2" ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  (* Should generate correct record type in update *)
  check bool "Should use #person in record update" true
    (string_matches_pattern result
       "Person2_[a-z0-9]+ = Person_[a-z0-9]+#person{name = \"Jane\"}");

  (* Should not use incorrect #record *)
  check bool "Should not use #record in update" false
    (string_contains_substring result "#record{")

(* Test successful record update with 'record' keyword as variable name *)
let test_record_update_record_keyword_variable () =
  let record_def =
    {
      record_name = "User";
      fields =
        [
          {
            field_name = "email";
            field_type = TypeName "string";
            default_value = None;
          };
          {
            field_name = "active";
            field_type = TypeName "boolean";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let user_create =
    RecordCreate
      ( "User",
        [
          ("email", Literal (LString "user@example.com"));
          ("active", Literal (LBool true));
        ] )
  in

  let user_update =
    RecordUpdate (Var "record", [ ("active", Literal (LBool false)) ])
  in

  let assign1 = Assign ("record", user_create, None) in
  let assign2 = Assign ("record1", user_update, None) in

  let func =
    make_single_clause_function "test_record_keyword" []
      (Sequence [ assign1; assign2; Var "record1" ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  (* Should generate correct record type even when variable is 'record' *)
  check bool "Should use #user in record update" true
    (string_matches_pattern result
       "Record1_[a-z0-9]+ = Record_[a-z0-9]+#user{active = false}");

  (* Should not use incorrect #record *)
  check bool "Should not use #record in update" false
    (string_contains_substring result "#record{")

(* Test chained record updates *)
let test_record_update_chained () =
  let record_def =
    {
      record_name = "Product";
      fields =
        [
          {
            field_name = "name";
            field_type = TypeName "string";
            default_value = None;
          };
          {
            field_name = "price";
            field_type = TypeName "float";
            default_value = None;
          };
          {
            field_name = "stock";
            field_type = TypeName "integer";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let product_create =
    RecordCreate
      ( "Product",
        [
          ("name", Literal (LString "Widget"));
          ("price", Literal (LFloat 9.99));
          ("stock", Literal (LInt 100));
        ] )
  in

  let update1 =
    RecordUpdate (Var "product", [ ("price", Literal (LFloat 8.99)) ])
  in

  let update2 =
    RecordUpdate (Var "product1", [ ("stock", Literal (LInt 50)) ])
  in

  let assign1 = Assign ("product", product_create, None) in
  let assign2 = Assign ("product1", update1, None) in
  let assign3 = Assign ("product2", update2, None) in

  let func =
    make_single_clause_function "test_chained_updates" []
      (Sequence [ assign1; assign2; assign3; Var "product2" ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  (* All updates should use correct record type *)
  check bool "First update should use #product" true
    (string_matches_pattern result
       "Product1_[a-z0-9]+ = Product_[a-z0-9]+#product{price = 8.99}");

  check bool "Second update should use #product" true
    (string_matches_pattern result
       "Product2_[a-z0-9]+ = Product1_[a-z0-9]+#product{stock = 50}")

(* Test record update with multiple fields *)
let test_record_update_multiple_fields () =
  let record_def =
    {
      record_name = "Employee";
      fields =
        [
          {
            field_name = "name";
            field_type = TypeName "string";
            default_value = None;
          };
          {
            field_name = "salary";
            field_type = TypeName "float";
            default_value = None;
          };
          {
            field_name = "department";
            field_type = TypeName "string";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let employee_create =
    RecordCreate
      ( "Employee",
        [
          ("name", Literal (LString "Alice"));
          ("salary", Literal (LFloat 50000.0));
          ("department", Literal (LString "Engineering"));
        ] )
  in

  let employee_update =
    RecordUpdate
      ( Var "emp",
        [
          ("salary", Literal (LFloat 55000.0));
          ("department", Literal (LString "Senior Engineering"));
        ] )
  in

  let assign1 = Assign ("emp", employee_create, None) in
  let assign2 = Assign ("emp_updated", employee_update, None) in

  let func =
    make_single_clause_function "test_multiple_fields" []
      (Sequence [ assign1; assign2; Var "emp_updated" ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  (* Should update multiple fields with correct record type *)
  check bool "Should use #employee with multiple fields" true
    (string_matches_pattern result
       "Emp_updated_[a-z0-9]+ = Emp_[a-z0-9]+#employee{salary = 55000\\.0, \
        department = \"Senior Engineering\"}")

(* Test record access after update *)
let test_record_access_after_update () =
  let record_def =
    {
      record_name = "Config";
      fields =
        [
          {
            field_name = "debug";
            field_type = TypeName "boolean";
            default_value = None;
          };
          {
            field_name = "timeout";
            field_type = TypeName "integer";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let config_create =
    RecordCreate
      ( "Config",
        [ ("debug", Literal (LBool false)); ("timeout", Literal (LInt 30)) ] )
  in

  let config_update =
    RecordUpdate (Var "config", [ ("debug", Literal (LBool true)) ])
  in

  let config_access = RecordAccess (Var "config_new", "timeout") in

  let assign1 = Assign ("config", config_create, None) in
  let assign2 = Assign ("config_new", config_update, None) in

  let func =
    make_single_clause_function "test_access_after_update" []
      (Sequence [ assign1; assign2; config_access ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in
  let result = Compiler.compile_to_string_for_tests program in

  (* Should use correct record type in both update and access *)
  check bool "Should use #config in update" true
    (string_matches_pattern result
       "Config_new_[a-z0-9]+ = Config_[a-z0-9]+#config{debug = true}");

  check bool "Should use #config in access" true
    (string_matches_pattern result "Config_new_[a-z0-9]+#config\\.timeout")

(* Test error case: record update on undefined record type *)
let test_record_update_undefined_type_error () =
  let undefined_update =
    RecordUpdate (Var "unknown", [ ("field", Literal (LString "value")) ])
  in

  let func = make_single_clause_function "test_undefined" [] undefined_update in
  let program = { items = [ Function func ] } in

  try
    ignore (Compiler.compile_to_string_for_tests program);
    fail "Should have raised error for undefined record type"
  with _ -> () (* Expected - should fail compilation *)

(* Test error case: record update with non-existent field *)
let test_record_update_nonexistent_field_error () =
  let record_def =
    {
      record_name = "Simple";
      fields =
        [
          {
            field_name = "value";
            field_type = TypeName "string";
            default_value = None;
          };
        ];
      position = None;
    }
  in

  let simple_create =
    RecordCreate ("Simple", [ ("value", Literal (LString "test")) ])
  in

  let invalid_update =
    RecordUpdate (Var "simple", [ ("nonexistent", Literal (LString "error")) ])
  in

  let assign1 = Assign ("simple", simple_create, None) in

  let func =
    make_single_clause_function "test_invalid_field" []
      (Sequence [ assign1; invalid_update ])
  in

  let program = { items = [ RecordDef record_def; Function func ] } in

  try
    ignore (Compiler.compile_to_string_for_tests program);
    fail "Should have raised error for non-existent field"
  with _ -> () (* Expected - should fail compilation *)

(* Test error case: record update on non-record expression *)
let test_record_update_non_record_error () =
  let invalid_update =
    RecordUpdate (Literal (LInt 42), [ ("field", Literal (LString "value")) ])
  in

  let func = make_single_clause_function "test_non_record" [] invalid_update in
  let program = { items = [ Function func ] } in

  try
    ignore (Compiler.compile_to_string_for_tests program);
    fail "Should have raised error for updating non-record"
  with _ -> () (* Expected - should fail compilation *)

let tests =
  [
    test_case "record update with normal variable" `Quick
      test_record_update_normal_variable;
    test_case "record update with 'record' keyword variable" `Quick
      test_record_update_record_keyword_variable;
    test_case "chained record updates" `Quick test_record_update_chained;
    test_case "record update with multiple fields" `Quick
      test_record_update_multiple_fields;
    test_case "record access after update" `Quick
      test_record_access_after_update;
    test_case "record update undefined type error" `Quick
      test_record_update_undefined_type_error;
    test_case "record update nonexistent field error" `Quick
      test_record_update_nonexistent_field_error;
    test_case "record update non-record error" `Quick
      test_record_update_non_record_error;
  ]
