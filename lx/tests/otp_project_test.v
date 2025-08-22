module main

fn test_application_block_basic_parsing() {
	lx_code := 'application {
  description: "Test App",
  vsn: "1.0.0"
}

def main() do
  :ok
end'

	expected := '-module(test).
-export([main/0]).

%% Application config:
%%  description: <<"Test App"/utf8>>
%%  vsn: <<"1.0.0"/utf8>>

-spec main() -> atom().
main() ->
    ok.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_anonymous_function_dot_call() {
	lx_code := 'def test_lambda_calls() do
  add = fn(x, y) do x + y end
  result1 = add.(5, 3)

  multiply = fn(a, b) do a * b end
  result2 = multiply.(4, 6)

  {result1, result2}
end'

	expected := '-module(test).
-export([test_lambda_calls/0]).

-spec test_lambda_calls() -> {integer(), integer()}.
test_lambda_calls() ->
    ADD_1 = fun(X_2Y_3) -> X_2 + Y_3 end,
    RESULT1_4 = ADD_1(5, 3),
    MULTIPLY_5 = fun(A_6B_7) -> A_6 * B_7 end,
    RESULT2_8 = MULTIPLY_5(4, 6),
    {RESULT1_4, RESULT2_8}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multi_clause_anonymous_function_dot_call() {
	lx_code := 'def test_multi_clause_lambda() do
  handler = fn do
    (:ok) -> "success"
    (:error) -> "failure"
    (_) -> "unknown"
  end

  result = handler.(:ok)
  result
end'

	expected := '-module(test).
-export([test_multi_clause_lambda/0]).

-spec test_multi_clause_lambda() -> binary().
test_multi_clause_lambda() ->
    HANDLER_1 = fun(Arg) -> case Arg of
    ok ->
        <<"success"/utf8>>;
    error ->
        <<"failure"/utf8>>;
    _ ->
        <<"unknown"/utf8>>
end end,
    RESULT_2 = HANDLER_1(ok),
    RESULT_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}
