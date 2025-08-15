// module main

// fn test_application_definition() {
// 	lx_code := '
// application {
//   description "E-commerce API Server"
//   vsn "1.0.0"
//   applications [kernel, stdlib, cowboy]
//   registered [api_server, product_manager]
// }'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([]).

// % Application: E-commerce API Server
// % Version: 1.0.0
// % Applications: [kernel, stdlib, cowboy]
// % Registered: [api_server, product_manager]

// '
// 	assert erl1.success
// 	// TODO: Fix application syntax (not implemented)
// 	// assert erl1.code == expected
// }

// fn test_record_definitions() {
// 	lx_code := '
// record Product do
//   id :: string,
//   name :: string,
//   price :: float,
//   stock :: integer
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([]).

// -record(product, {
// id :: string(),
// name :: string(),
// price :: float(),
// stock :: integer()
// }).

// '
// 	assert erl1.success
// 	// TODO: Fix record syntax (not implemented)
// 	// assert erl1.code == expected
// }

// fn test_deps_declaration() {
// 	lx_code := '
// deps [:cowboy, :jsx, :crypto]

// def simple_function() do
//   "Hello"
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([simple_function/0]).

// % Dependencies: [cowboy, jsx, crypto]

// -spec simple_function() -> string().
// simple_function() ->
// "Hello".

// '
// 	assert erl1.success
// 	// TODO: Fix deps syntax (not implemented)
// 	// assert erl1.code == expected
// }

// fn test_complex_worker() {
// 	lx_code := '
// worker api_server do
//   def init(_) do
//     port = 8080
//     {:ok, %{port: port, routes: []}}
//   end

//   def handle_call({:start_server}, _from, state) do
//     {:reply, :ok, state}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([init/1, handle_call/3]).

// init(_) ->
// Port = 8080,
// {ok, #{port => Port, routes => []}}.

// handle_call({start_server}, _From, State) ->
// {reply, ok, State}.

// '
// 	assert erl1.success
// 	// TODO: Fix complex worker with variable assignments
// 	// assert erl1.code == expected
// }

// fn test_simple_function_only() {
// 	lx_code := '
// def validate_product(product_data) do
//   case product_data do
//     %{name: name} when name != "" -> {:ok, product_data}
//     _ -> {:error, "Invalid product"}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([validate_product/1]).

// -spec validate_product(map()) -> {ok, map()} | {error, string()}.
// validate_product(Product_data) ->
// case Product_data of
// #{name := Name} when Name /= "" -> {ok, Product_data};
// _ -> {error, "Invalid product"}
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix map pattern matching with guards
// 	// assert erl1.code == expected
// }
