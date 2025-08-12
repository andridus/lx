module main

import compile

// Use external compile_lx function from utils.v

fn test_application_block_basic_parsing() {
	lx_code := 'application {
  description: "Test App",
  vsn: "1.0.0"
}

def main() do
  :ok
end'

	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('%% Application config:')
	assert result.contains('%%  description: <<"Test App"/utf8>>')
	assert result.contains('%%  vsn: <<"1.0.0"/utf8>>')
}

fn test_anonymous_function_dot_call() {
	lx_code := 'def test_lambda_calls() do
  add = fn(x, y) do x + y end
  result1 = add.(5, 3)

  multiply = fn(a, b) do a * b end
  result2 = multiply.(4, 6)

  {result1, result2}
end'

	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('ADD_1 = fun(X_2, Y_3) ->')
	assert result.contains('X_2 + Y_3')
	assert result.contains('RESULT1_4 = ADD_1(5, 3)')
	assert result.contains('MULTIPLY_5 = fun(A_6, B_7) ->')
	assert result.contains('A_6 * B_7')
	assert result.contains('RESULT2_8 = MULTIPLY_5(4, 6)')
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

	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('HANDLER_1 = fun')
	assert result.contains('(ok) ->')
	assert result.contains('<<"success"/utf8>>')
	assert result.contains('(error) ->')
	assert result.contains('<<"failure"/utf8>>')
	assert result.contains('RESULT_2 = HANDLER_1(ok)')
}