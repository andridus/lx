module main

fn test_commented_tests() {
}

// // =============================================================================
// // MODULE CONTEXT TESTS
// // =============================================================================

// // fn test_module_context_accepts_structural_declarations() {
// // 	lx_code := '
// // record User {
// //   name :: string,
// //   age :: integer
// // }

// // type UserId :: integer

// // def get_user(id) do
// //   User{name: "John", age: 30}
// // end

// // defp validate_user(user) do
// //   user.age > 0
// // end

// // spec get_user(UserId) :: User

// // worker UserWorker do
// //   def init() do
// //     "initialized"
// //   end
// // end

// // supervisor UserSupervisor do
// //   def init() do
// //     "supervisor_init"
// //   end
// // end

// // describe "User tests" do
// //   test "user creation" do
// //     user = User{name: "Test", age: 25}
// //     assert user.name == "Test"
// //   end
// // end'

// // 	result := generates_erlang_result(lx_code)
// // 	assert result.code.contains('-record(user, {name, age})')
// // 	assert result.code.contains('-type user_id() :: integer()')
// // 	assert result.code.contains('get_user(Id)')
// // 	assert result.code.contains('validate_user(User)')
// // }

// fn test_module_context_rejects_expressions() {
// 	// Test that expressions are rejected at module level
// 	lx_code := '
// def test_func() do
//   1 + 2
// end

// 1 + 2  # This should be rejected
// '

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// 	assert result.errors[0].contains('expression') || result.errors[0].contains('statement')
// }

// fn test_module_context_rejects_assignments() {
// 	// Test that assignments are rejected at module level
// 	lx_code := '
// def test_func() do
//   x = 42
//   x
// end

// x = 42  # This should be rejected at module level
// '

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// }

// fn test_module_context_rejects_function_calls() {
// 	// Test that function calls are rejected at module level
// 	lx_code := '
// def helper() do
//   "helper"
// end

// helper()  # This should be rejected at module level
// '

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// }

// // =============================================================================
// // FUNCTION CONTEXT TESTS
// // =============================================================================

// fn test_function_context_accepts_expressions() {
// 	lx_code := '
// def test_expressions() do
//   x = 42
//   y = x + 10
//   z = y * 2
//   result = if x > 0 do
//     "positive"
//   else
//     "negative"
//   end
//   {x, y, z, result}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('X = 42')
// 	assert result.code.contains('Y = X + 10')
// 	assert result.code.contains('Z = Y * 2')
// 	assert result.code.contains('case X > 0 of')
// }

// fn test_function_context_control_flow_as_expressions() {
// 	lx_code := '
// def test_control_flow(x) do
//   result1 = case x do
//     1 -> "one"
//     2 -> "two"
//     _ -> "other"
//   end

//   result2 = if x > 0 do
//     "positive"
//   else
//     "negative"
//   end

//   result3 = with {:ok, value} <- get_value(x) do
//     value * 2
//   else
//     0
//   end

//   {result1, result2, result3}
// end

// def get_value(x) do
//   {:ok, x}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('case X of')
// 	assert result.code.contains('case X > 0 of')
// 	assert result.code.contains('case get_value(X) of')
// }

// fn test_function_context_pattern_matching() {
// 	lx_code := '
// def test_pattern_matching(input) do
//   {a, b} = input
//   [head | tail] = [1, 2, 3]
//   %{key: value} = %{key: "test"}
//   result = a + b + head + String.length(value)
//   result
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('{A, B} = Input')
// 	assert result.code.contains('[Head | Tail] = [1, 2, 3]')
// 	assert result.code.contains('#{key := Value} = #{key => "test"}')
// }

// fn test_function_context_data_structures() {
// 	lx_code := '
// def test_data_structures() do
//   tuple = {1, 2, "three"}
//   list = [1, 2, 3, 4]
//   map = %{name: "John", age: 30}
//   updated_map = %{map | age: 31}

//   record_val = User{name: "Jane", age: 25}
//   updated_record = %{record_val | age: 26}

//   {tuple, list, map, updated_map, record_val, updated_record}
// end

// record User {
//   name :: string,
//   age :: integer
// }'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('Tuple = {1, 2, "three"}')
// 	assert result.code.contains('List = [1, 2, 3, 4]')
// 	assert result.code.contains('Map = #{name => "John", age => 30}')
// 	assert result.code.contains('UpdatedMap = Map#{age := 31}')
// 	assert result.code.contains('#user{name = "Jane", age = 25}')
// }

// // =============================================================================
// // WORKER/SUPERVISOR CONTEXT TESTS
// // =============================================================================

// fn test_worker_context_follows_module_rules() {
// 	lx_code := '
// worker UserWorker do
//   def init() do
//     "worker_initialized"
//   end

//   def handle_call(request, from, state) do
//     {:reply, "ok", state}
//   end

//   defp internal_helper() do
//     "helper"
//   end

//   spec init() :: string
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('init() ->')
// 	assert result.code.contains('handle_call(Request, From, State)')
// 	assert result.code.contains('internal_helper() ->')
// }

// fn test_worker_context_rejects_expressions() {
// 	lx_code := '
// worker UserWorker do
//   def init() do
//     "initialized"
//   end

//   1 + 2  // This should be rejected in worker context
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// }

// fn test_supervisor_context_follows_module_rules() {
// 	lx_code := '
// supervisor UserSupervisor do
//   def init() do
//     children = [
//       %{id: :user_worker, start: {UserWorker, :start_link, []}}
//     ]
//     {:ok, children}
//   end

//   def start_link() do
//     GenServer.start_link(__MODULE__, [], name: __MODULE__)
//   end

//   spec init() :: {:ok, [map]}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('init() ->')
// 	assert result.code.contains('start_link() ->')
// }

// fn test_supervisor_context_rejects_expressions() {
// 	lx_code := '
// supervisor UserSupervisor do
//   def init() do
//     "supervisor_init"
//   end

//   start_link()  // This should be rejected in supervisor context
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// }

// // =============================================================================
// // DESCRIBE/TEST CONTEXT TESTS
// // =============================================================================

// fn test_describe_context_follows_module_rules() {
// 	lx_code := '
// describe "User functionality" do
//   def setup() do
//     %{user: User{name: "Test", age: 25}}
//   end

//   test "user creation" do
//     user = User{name: "John", age: 30}
//     assert user.name == "John"
//     assert user.age == 30
//   end

//   test "user validation" do
//     user = User{name: "Jane", age: 0}
//     assert !validate_user(user)
//   end

//   defp validate_user(user) do
//     user.age > 0
//   end
// end

// record User {
//   name :: string,
//   age :: integer
// }'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('setup() ->')
// 	assert result.code.contains('validate_user(User)')
// }

// fn test_describe_context_rejects_expressions() {
// 	lx_code := '
// describe "User tests" do
//   test "basic test" do
//     assert true
//   end

//   User{name: "Test", age: 25}  // This should be rejected in describe context
// end

// record User {
//   name :: string,
//   age :: integer
// }'

// 	result := generates_erlang_result(lx_code)
// 	assert !result.success
// 	assert result.errors.len > 0
// }

// // =============================================================================
// // TYPE CONTEXT TESTS
// // =============================================================================

// fn test_type_context_separation() {
// 	lx_code := '
// type UserId :: integer
// type UserName :: string
// type UserStatus :: :active | :inactive | :pending

// type User :: %{
//   id: UserId,
//   name: UserName,
//   status: UserStatus
// }

// type Result(T) :: {:ok, T} | {:error, string}

// def get_user(id :: UserId) :: Result(User) do
//   user = %{id: id, name: "John", status: :active}
//   {:ok, user}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-type user_id() :: integer()')
// 	assert result.code.contains('-type user_name() :: string()')
// 	assert result.code.contains('-type user_status() :: active | inactive | pending')
// 	assert result.code.contains('-type user() :: #{id := user_id(), name := user_name(), status := user_status()}')
// 	assert result.code.contains('-type result(T) :: {ok, T} | {error, string()}')
// }

// // =============================================================================
// // NESTED STRUCTURE TESTS
// // =============================================================================

// fn test_nested_worker_in_supervisor() {
// 	lx_code := '
// supervisor MainSupervisor do
//   def init() do
//     "supervisor_init"
//   end

//   worker ChildWorker do
//     def init() do
//       "child_worker_init"
//     end

//     def handle_call(request, from, state) do
//       {:reply, "ok", state}
//     end
//   end

//   def start_link() do
//     "start_supervisor"
//   end
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('init() ->')
// 	assert result.code.contains('handle_call(Request, From, State)')
// 	assert result.code.contains('start_link() ->')
// }

// fn test_nested_describe_in_describe() {
// 	lx_code := '
// describe "Main functionality" do
//   def setup() do
//     "main_setup"
//   end

//   describe "User operations" do
//     def setup() do
//       "user_setup"
//     end

//     test "user creation" do
//       user = %{name: "Test"}
//       assert user.name == "Test"
//     end
//   end

//   test "main test" do
//     assert true
//   end
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('setup() ->')
// 	assert result.code.contains('user_setup()')
// 	assert result.code.contains('test "user creation"')
// 	assert result.code.contains('assert user.name == "Test"')
// 	assert result.code.contains('test "main test"')
// 	assert result.code.contains('assert true')
// }

// // =============================================================================
// // EXPRESSION BLOCK TESTS
// // =============================================================================

// fn test_expression_block_returns_last_value() {
// 	lx_code := '
// def test_block_return() do
//   x = 10
//   y = 20
//   z = x + y
//   result = if z > 25 do
//     "large"
//   else
//     "small"
//   end
//   result  // This should be the return value
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_block_return() ->')
// 	assert result.code.contains('X = 10')
// 	assert result.code.contains('Y = 20')
// 	assert result.code.contains('Z = X + Y')
// 	assert result.code.contains('result = if Z > 25 do')
// }

// fn test_expression_block_with_complex_control_flow() {
// 	lx_code := '
// def complex_flow(input) do
//   step1 = case input do
//     x when x > 0 -> x * 2
//     x when x < 0 -> x * -1
//     _ -> 0
//   end

//   step2 = for x <- [1, 2, 3] do
//     x + step1
//   end

//   step3 = receive do
//     {:ok, value} -> value
//     {:error, _} -> "error"
//   after
//     1000 -> "timeout"
//   end

//   {step1, step2, step3}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('complex_flow(Input)')
// 	assert result.code.contains('Step1 = case Input do')
// 	assert result.code.contains('Step2 = for X <- [1, 2, 3] do')
// 	assert result.code.contains('Step3 = receive do')
// 	assert result.code.contains('{ok, Value} -> Value')
// }

// // =============================================================================
// // ASSIGNMENT AS PATTERN MATCHING TESTS
// // =============================================================================

// fn test_assignment_as_pattern_matching() {
// 	lx_code := '
// def test_pattern_assignments() do
//   x = 42
//   {a, b} = {1, 2}
//   [head | tail] = [1, 2, 3]
//   %{key: value} = %{key: "test", other: "data"}
//   User{name: name, age: age} = User{name: "John", age: 30}

//   result = x + a + b + head + String.length(value) + age
//   result
// end

// record User {
//   name :: string,
//   age :: integer
// }'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('X = 42')
// 	assert result.code.contains('{A, B} = {1, 2}')
// 	assert result.code.contains('[Head | Tail] = [1, 2, 3]')
// 	assert result.code.contains('#{key := Value} = #{key => "test", other => "data"}')
// 	assert result.code.contains('#user{name = Name, age = Age} = #user{name = "John", age = 30}')
// }

// fn test_assignment_with_type_annotation() {
// 	lx_code := '
// def test_typed_assignment() do
//   x :: integer = 42
//   name :: string = "John"
//   user :: User = User{name: name, age: 30}

//   result = x + user.age
//   result
// end

// record User {
//   name :: string,
//   age :: integer
// }'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('X = 42')
// 	assert result.code.contains('Name = "John"')
// 	assert result.code.contains('User = #user{name = Name, age = 30}')
// }

// // =============================================================================
// // COMPREHENSIVE INTEGRATION TESTS
// // =============================================================================

// fn test_comprehensive_module_structure() {
// 	lx_code := '
// type UserId :: integer
// type UserName :: string

// record User {
//   id :: UserId,
//   name :: UserName,
//   active :: boolean
// }

// spec create_user(UserId, UserName) :: User
// spec validate_user(User) :: boolean

// def create_user(id, name) do
//   User{id: id, name: name, active: true}
// end

// defp validate_user(user) do
//   user.id > 0 and String.length(user.name) > 0
// end

// worker UserWorker do
//   def init() do
//     %{users: []}
//   end

//   def handle_call({:create, id, name}, _from, state) do
//     user = create_user(id, name)
//     new_state = %{state | users: [user | state.users]}
//     {:reply, {:ok, user}, new_state}
//   end

//   defp validate_request(request) do
//     request != nil
//   end
// end

// supervisor UserSupervisor do
//   def init() do
//     children = [
//       %{id: :user_worker, start: {UserWorker, :start_link, []}}
//     ]
//     {:ok, children}
//   end

//   def start_link() do
//     Supervisor.start_link(__MODULE__, [], name: __MODULE__)
//   end
// end

// describe "User system tests" do
//   def setup() do
//     %{test_user: User{id: 1, name: "Test User", active: true}}
//   end

//   test "user creation" do
//     user = create_user(1, "John")
//     assert user.id == 1
//     assert user.name == "John"
//     assert user.active == true
//   end

//   test "user validation" do
//     valid_user = User{id: 1, name: "Valid", active: true}
//     invalid_user = User{id: 0, name: "", active: false}

//     assert validate_user(valid_user)
//     assert !validate_user(invalid_user)
//   end

//   describe "Worker tests" do
//     test "worker initialization" do
//       state = UserWorker.init()
//       assert state.users == []
//     end
//   end
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-type user_id() :: integer()')
// 	assert result.code.contains('-record(user, {id, name, active})')
// 	assert result.code.contains('-spec create_user(user_id(), user_name()) -> user()')
// 	assert result.code.contains('create_user(Id, Name)')
// 	assert result.code.contains('validate_user(User)')
// 	assert result.code.contains('init() ->')
// 	assert result.code.contains('handle_call({create, Id, Name}, _from, State)')
// 	assert result.code.contains('start_link() ->')
// }

// // =============================================================================
// // BASIC CONTEXT STRUCTURE TESTS
// // =============================================================================
// // These tests verify the basic structure of parser contexts using only
// // simple functionality that is currently working

// fn test_basic_function_definition() {
// 	// Test that basic function definitions work
// 	lx_code := '
// def hello() do
//   "world"
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('hello() ->')
// 	assert result.code.contains('"world"')
// }

// fn test_basic_record_definition() {
// 	// Test that basic record definitions work
// 	lx_code := '
// record Person {
//   name :: string,
//   age :: integer
// }

// def create_person() do
//   Person{name: "John", age: 30}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-record(person, {name, age})')
// 	assert result.code.contains('create_person() ->')
// 	assert result.code.contains('#person{name = "John", age = 30}')
// }

// fn test_basic_type_definition() {
// 	// Test that basic type definitions work
// 	lx_code := '
// type UserId :: integer

// def get_id() do
//   42
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-type user_id() :: integer()')
// 	assert result.code.contains('get_id() ->')
// 	assert result.code.contains('42')
// }

// fn test_basic_spec_definition() {
// 	// Test that basic spec definitions work
// 	lx_code := '
// spec hello() :: string

// def hello() do
//   "world"
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-spec hello() -> string()')
// 	assert result.code.contains('hello() ->')
// 	assert result.code.contains('"world"')
// }

// fn test_basic_private_function() {
// 	// Test that basic private functions work
// 	lx_code := '
// def public_func() do
//   private_func()
// end

// defp private_func() do
//   "private"
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('public_func() ->')
// 	assert result.code.contains('private_func() ->')
// 	assert result.code.contains('"private"')
// }

// fn test_basic_assignment_in_function() {
// 	// Test that basic assignments work in functions
// 	lx_code := '
// def test_assignment() do
//   x = 42
//   y = "hello"
//   {x, y}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_assignment() ->')
// 	assert result.code.contains('X = 42')
// 	assert result.code.contains('Y = "hello"')
// 	assert result.code.contains('{X, Y}')
// }

// fn test_basic_literal_expressions() {
// 	// Test that basic literal expressions work
// 	lx_code := '
// def test_literals() do
//   num = 42
//   str = "hello"
//   atom = :test
//   bool = true
//   {num, str, atom, bool}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_literals() ->')
// 	assert result.code.contains('Num = 42')
// 	assert result.code.contains('Str = "hello"')
// 	assert result.code.contains('Atom = test')
// 	assert result.code.contains('Bool = true')
// }

// fn test_basic_tuple_literal() {
// 	// Test that basic tuple literals work
// 	lx_code := '
// def test_tuple() do
//   tuple = {1, 2, 3}
//   tuple
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_tuple() ->')
// 	assert result.code.contains('Tuple = {1, 2, 3}')
// }

// fn test_basic_list_literal() {
// 	// Test that basic list literals work
// 	lx_code := '
// def test_list() do
//   list = [1, 2, 3]
//   list
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_list() ->')
// 	assert result.code.contains('List = [1, 2, 3]')
// }

// fn test_basic_arithmetic_operations() {
// 	// Test that basic arithmetic operations work
// 	lx_code := '
// def test_arithmetic() do
//   a = 10
//   b = 5
//   sum = a + b
//   diff = a - b
//   prod = a * b
//   quot = a / b
//   {sum, diff, prod, quot}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_arithmetic() ->')
// 	assert result.code.contains('A = 10')
// 	assert result.code.contains('B = 5')
// 	assert result.code.contains('Sum = A + B')
// 	assert result.code.contains('Diff = A - B')
// 	assert result.code.contains('Prod = A * B')
// 	assert result.code.contains('Quot = A / B')
// }

// fn test_basic_function_calls() {
// 	// Test that basic function calls work
// 	lx_code := '
// def helper() do
//   "helper result"
// end

// def main() do
//   result = helper()
//   result
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('helper() ->')
// 	assert result.code.contains('main() ->')
// 	assert result.code.contains('Result = helper()')
// }

// fn test_basic_function_with_parameters() {
// 	// Test that basic functions with parameters work
// 	lx_code := '
// def add(x, y) do
//   x + y
// end

// def test() do
//   result = add(10, 20)
//   result
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('add(X, Y) ->')
// 	assert result.code.contains('X + Y')
// 	assert result.code.contains('test() ->')
// 	assert result.code.contains('Result = add(10, 20)')
// }

// fn test_basic_record_access() {
// 	// Test that basic record access works
// 	lx_code := '
// record User {
//   name :: string,
//   age :: integer
// }

// def get_name(user) do
//   user.name
// end

// def test() do
//   user = User{name: "John", age: 30}
//   name = get_name(user)
//   name
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-record(user, {name, age})')
// 	assert result.code.contains('get_name(User) ->')
// 	assert result.code.contains('User#user.name')
// 	assert result.code.contains('test() ->')
// 	assert result.code.contains('User = #user{name = "John", age = 30}')
// }

// fn test_basic_multiple_statements() {
// 	// Test that multiple statements work in functions
// 	lx_code := '
// def multi_statement() do
//   x = 10
//   y = 20
//   z = x + y
//   result = z * 2
//   result
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('multi_statement() ->')
// 	assert result.code.contains('X = 10')
// 	assert result.code.contains('Y = 20')
// 	assert result.code.contains('Z = X + Y')
// 	assert result.code.contains('Result = Z * 2')
// }

// fn test_basic_string_operations() {
// 	// Test that basic string operations work
// 	lx_code := '
// def test_strings() do
//   str1 = "hello"
//   str2 = "world"
//   combined = str1 <> " " <> str2
//   combined
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_strings() ->')
// 	assert result.code.contains('Str1 = "hello"')
// 	assert result.code.contains('Str2 = "world"')
// 	// String concatenation might be handled differently
// }

// fn test_basic_boolean_operations() {
// 	// Test that basic boolean operations work
// 	lx_code := '
// def test_booleans() do
//   a = true
//   b = false
//   and_result = a and b
//   or_result = a or b
//   not_result = not a
//   {and_result, or_result, not_result}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_booleans() ->')
// 	assert result.code.contains('A = true')
// 	assert result.code.contains('B = false')
// 	// Boolean operations might be handled differently
// }

// fn test_basic_comparison_operations() {
// 	// Test that basic comparison operations work
// 	lx_code := '
// def test_comparisons() do
//   a = 10
//   b = 20
//   eq = a == b
//   neq = a != b
//   lt = a < b
//   gt = a > b
//   {eq, neq, lt, gt}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('test_comparisons() ->')
// 	assert result.code.contains('A = 10')
// 	assert result.code.contains('B = 20')
// 	assert result.code.contains('Eq = A == B')
// 	assert result.code.contains('Neq = A /= B')
// 	assert result.code.contains('Lt = A < B')
// 	assert result.code.contains('Gt = A > B')
// }

// fn test_basic_comprehensive_example() {
// 	// Test a comprehensive example with multiple features
// 	lx_code := '
// type UserId :: integer

// record User {
//   id :: UserId,
//   name :: string,
//   active :: boolean
// }

// spec create_user(UserId, string) :: User

// def create_user(id, name) do
//   User{id: id, name: name, active: true}
// end

// def get_user_name(user) do
//   user.name
// end

// def test_user_system() do
//   user = create_user(1, "John")
//   name = get_user_name(user)
//   is_active = user.active
//   {name, is_active}
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-type user_id() :: integer()')
// 	assert result.code.contains('-record(user, {id, name, active})')
// 	assert result.code.contains('-spec create_user(user_id(), string()) -> user()')
// 	assert result.code.contains('create_user(Id, Name) ->')
// 	assert result.code.contains('get_user_name(User) ->')
// 	assert result.code.contains('test_user_system() ->')
// }
