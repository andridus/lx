module main

// ============ Task 11: Control Flow Tests ============

fn test_if_expressions() {
	lx_code := 'def test_if(x) do
    if x > 0 do
        "positive"
    else
        "negative"
    end
end'
	expected := '-module(test).
-export([test_if/1]).

-spec test_if(any()) -> binary().
test_if(X_1) ->
    case X_1 > 0 of
        true -> <<"positive"/utf8>>;
        false -> <<"negative"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_case_expressions() {
	lx_code := 'def test_case(status) do
    case status do
        :ok -> "success"
        :error -> "failure"
        _ -> "unknown"
    end
end'
	expected := '-module(test).
-export([test_case/1]).

-spec test_case(any()) -> binary().
test_case(STATUS_1) ->
    case STATUS_1 of
        ok ->
            <<"success"/utf8>>;
        error ->
            <<"failure"/utf8>>;
        _ ->
            <<"unknown"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_with_expressions() {
	lx_code := '
    def some_operation() do
        {:ok, "success"}
    end

    def test_with() do
    with {:ok, value} <- some_operation() do
        "success"
    else
        {:error, reason} -> "error"
    end
end'
	expected := '-module(test).
-export([some_operation/0, test_with/0]).

-spec some_operation() -> {atom(), binary()}.
some_operation() ->
    {ok, <<"success"/utf8>>}.
-spec test_with() -> binary().
test_with() ->
    case some_operation() of
        {ok, VALUE_1} ->
            <<"success"/utf8>>;
        {error, REASON_2} ->
            <<"error"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Concurrency Tests ============

fn test_spawn_expressions() {
	lx_code := '
    def server_loop() do
    :ok
end

def test_spawn() do
spawn(fn() -> server_loop())
end'
	expected := '-module(test).
-export([server_loop/0, test_spawn/0]).

-spec server_loop() -> atom().
server_loop() ->
    ok.
-spec test_spawn() -> pid().
test_spawn() ->
    spawn(fun() ->
        server_loop()
    end).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_send_expressions() {
	lx_code := 'def test_send(pid, msg) do
    pid ! msg
end'
	expected := '-module(test).
-export([test_send/2]).

-spec test_send(any(), any()) -> any().
test_send(PID_1, MSG_2) ->
    PID_1 ! MSG_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_receive_expressions() {
	lx_code := 'def test_receive() do
    receive do
        {:message, data} -> data
        :stop -> :ok
    end
end'
	expected := '-module(test).
-export([test_receive/0]).

-spec test_receive() -> any().
test_receive() ->
    receive
        {message, DATA_1} ->
            DATA_1;;
        stop ->
            ok;
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Binaries Tests ============

fn test_binary_literals() {
	lx_code := 'def test_binary() do
    <<1, 2, 3>>
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    <<1, 2, 3>>.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_binary_with_size() {
	lx_code := 'def test_binary_size() do
    <<42:8>>
end'
	expected := '-module(test).
-export([test_binary_size/0]).

-spec test_binary_size() -> binary().
test_binary_size() ->
    <<42:8>>.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_binary_string() {
	lx_code := 'def test_binary_string() do
    <<"hello">>
end'
	expected := '-module(test).
-export([test_binary_string/0]).

-spec test_binary_string() -> binary().
test_binary_string() ->
    <<<<"hello"/utf8>>>>.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Module System Tests ============

fn test_deps_declaration() {
	lx_code := 'application {\n  deps: [:cowboy, :jsx]\n}\n\n
def test_deps() do
    :ok
end'
	expected := '-module(test).\n-export([test_deps/0]).\n\n%% Application config:\n%%  deps: [cowboy, jsx]\n\n-spec test_deps() -> atom().
test_deps() ->
    ok.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_import_statement() {
	lx_code := 'import :cowboy

def test_import() do
    :ok
end'
	expected := '-module(test).
-export([test_import/0]).

%% Import: cowboy

-spec test_import() -> atom().
test_import() ->
    ok.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Advanced Features Tests ============

fn test_anonymous_functions() {
	lx_code := 'def test_anon() do
    fn(x) -> x * 2
end'
	expected := '-module(test).
-export([test_anon/0]).

-spec test_anon() -> function().
test_anon() ->
    fun(X_1) ->
        X_1 * 2
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_directive_annotations() {
	lx_code := '@doc "Test function"
def test_directive() do
    :ok
end'
	expected := '-module(test).
-export([test_directive/0]).

-doc "Test function".
-spec test_directive() -> atom().
test_directive() ->
    ok.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Integration Tests ============

fn test_complex_integration() {
	lx_code := 'def complex_test() do
    pid = spawn(fn() -> :ok)
    if pid != nil do
        pid ! "hello"
        :sent
    else
        :error
    end
end'
	expected := '-module(test).
-export([complex_test/0]).

-spec complex_test() -> atom().
complex_test() ->
    PID_1 = spawn(fun() ->
        ok
    end),
    case PID_1 /= nil of
        true -> PID_1 ! <<"hello"/utf8>>,
    sent;
        false -> error
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Task 11: Error Tests ============

fn test_syntax_errors() {
	lx_code := 'def invalid_syntax() do
    if x > 0
        "positive"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do" after if condition')
}

fn test_invalid_binary_syntax() {
	lx_code := 'def invalid_binary() do
    <<1, 2, 3
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected ">>"')
}

fn test_invalid_receive_syntax() {
	lx_code := 'def invalid_receive() do
    receive
        msg -> msg
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do"')
}
