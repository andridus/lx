import analysis.linter
import frontend.parser
import frontend.lexer
import ast
import errors

fn test_multi_clause_function_parsing() {
	// Test parsing of multi-clause function with correct arity order
	tokens := [
		// Function definition
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('f')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or {
		panic('Failed to parse multi-clause function')
	}

	// Find the function statement
	mut func_stmt := ast.FunctionStmt{}
	mut found := false
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt = stmt as ast.FunctionStmt
				found = true
				break
			}
			else {
				// Skip non-function statements
			}
		}
	}

	assert found == true
	assert func_stmt.name == 'f'
	assert func_stmt.clauses.len == 2

	// Check first clause (arity 1)
	clause1 := func_stmt.clauses[0]
	assert clause1.parameters.len == 1

	// Check second clause (arity 1 with guard)
	clause2 := func_stmt.clauses[1]
	assert clause2.parameters.len == 1
}

// fn test_multi_clause_function_with_different_arities() {
// 	// Test function with clauses of different arities in correct order
// 	tokens := [
// 		// Function definition
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
// 		lexer.Token(lexer.new_ident_token('process')),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
// 		// First clause: process() -> {:ok, nil}
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
// 		lexer.Token(lexer.new_atom_token('ok')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.nil_)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
// 		// Second clause: process(x) -> {:ok, x}
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
// 		lexer.Token(lexer.new_atom_token('ok')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
// 		// Third clause: process(x, y) -> {:ok, x, y}
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_ident_token('y')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
// 		lexer.Token(lexer.new_atom_token('ok')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_ident_token('y')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_))
// 	]

// 	mut parser_instance := parser.new_main_parser(tokens)
// 	module_stmt := parser_instance.parse_module() or {
// 		panic('Failed to parse multi-clause function with different arities')
// 	}

// 	// Find the function statement
// 	mut func_stmt := ast.FunctionStmt{}
// 	mut found := false
// 	for stmt in module_stmt.statements {
// 		match stmt {
// 			ast.FunctionStmt {
// 				func_stmt = stmt as ast.FunctionStmt
// 				found = true
// 				break
// 			}
// 			else {
// 				// Skip non-function statements
// 			}
// 		}
// 	}

// 	assert found == true
// 	assert func_stmt.name == 'process'
// 	assert func_stmt.clauses.len == 3

// 	// Check arities are in ascending order
// 	assert func_stmt.clauses[0].parameters.len == 0 // arity 0
// 	assert func_stmt.clauses[1].parameters.len == 1 // arity 1
// 	assert func_stmt.clauses[2].parameters.len == 2 // arity 2
// }

// fn test_multi_clause_function_with_blocks() {
// 	// Test function with clauses containing blocks
// 	tokens := [
// 		// Function definition
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
// 		lexer.Token(lexer.new_ident_token('calculate')),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
// 		// First clause: calculate(x) do x * 2 end
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.mult)),
// 		lexer.Token(lexer.new_int_token(2)),
// 		// Second clause: calculate(x, y) do result = x + y; result * 2 end
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_ident_token('y')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_ident_token('result')),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
// 		lexer.Token(lexer.new_ident_token('x')),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.plus)),
// 		lexer.Token(lexer.new_ident_token('y')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.semicolon)),
// 		lexer.Token(lexer.new_ident_token('result')),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.mult)),
// 		lexer.Token(lexer.new_int_token(2)),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_))
// 	]

// 	mut parser_instance := parser.new_main_parser(tokens)
// 	module_stmt := parser_instance.parse_module() or {
// 		panic('Failed to parse multi-clause function with blocks')
// 	}

// 	// Find the function statement
// 	mut func_stmt := ast.FunctionStmt{}
// 	mut found := false
// 	for stmt in module_stmt.statements {
// 		match stmt {
// 			ast.FunctionStmt {
// 				func_stmt = stmt as ast.FunctionStmt
// 				found = true
// 				break
// 			}
// 			else {
// 				// Skip non-function statements
// 			}
// 		}
// 	}

// 	assert found == true
// 	assert func_stmt.name == 'calculate'
// 	assert func_stmt.clauses.len == 2

// 	// Check first clause has simple expression
// 	clause1 := func_stmt.clauses[0]
// 	assert clause1.parameters.len == 1
// 	assert clause1.body.len == 1

// 	// Check second clause has block with multiple statements
// 	clause2 := func_stmt.clauses[1]
// 	assert clause2.parameters.len == 2
// }

// fn test_multi_clause_function_with_tuples() {
// 	// Test function with clauses containing tuples
// 	tokens := [
// 		// Function definition
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
// 		lexer.Token(lexer.new_ident_token('f')),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
// 		// First clause: f() -> {:ok, nil}
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
// 		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
// 		lexer.Token(lexer.new_atom_token('ok')),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.nil_)),
// 		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
// 		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_))
// 	]

// 	mut parser_instance := parser.new_main_parser(tokens)
// 	module_stmt := parser_instance.parse_module() or {
// 		panic('Failed to parse multi-clause function with tuples')
// 	}

// 	// Find the function statement
// 	mut func_stmt := ast.FunctionStmt{}
// 	mut found := false
// 	for stmt in module_stmt.statements {
// 		match stmt {
// 			ast.FunctionStmt {
// 				func_stmt = stmt as ast.FunctionStmt
// 				found = true
// 				break
// 			}
// 			else {
// 				// Skip non-function statements
// 			}
// 		}
// 	}

// 	assert found == true
// 	assert func_stmt.name == 'f'
// 	assert func_stmt.clauses.len == 1

// 	// Check first clause has tuple
// 	clause1 := func_stmt.clauses[0]
// 	assert clause1.parameters.len == 0
// 	assert clause1.body.len == 1
// }

// fn test_linter_arity_order_validation_correct() {
// 	// Test that linter correctly validates arity order
// 	code := 'def f do
//   (x) -> 1
//   (y) -> 2
// end'

// 	errors := linter.lint_code(code)
// 	assert errors.len == 0
// }

// fn test_linter_arity_order_validation_incorrect() {
// 	// Test that linter correctly detects incorrect arity order
// 	code := 'def f do
//   (x, y) -> 1
//   (x) -> 2
// end'

// 	errors := linter.lint_code(code)
// 	assert errors.len > 0
// }

fn main() {
	test_multi_clause_function_parsing()
	// test_multi_clause_function_with_tuples()
	// test_linter_arity_order_validation_correct()
	// test_linter_arity_order_validation_incorrect()
}
