module internal

import ast
import lexer
import errors

// ExpressionParser handles parsing of all expression types in LX
@[heap]
pub struct ExpressionParser {
	Parser
}

pub fn new_expression_parser(tokens []lexer.Token) &ExpressionParser {
	return &ExpressionParser{
		Parser: new_parser(tokens)
	}
}

// parse_expression parses the top-level expression
pub fn (mut ep ExpressionParser) parse_expression() ?ast.Expr {
	return ep.parse_assignment_expression()
}

// parse_assignment_expression parses assignment expressions (lowest precedence)
pub fn (mut ep ExpressionParser) parse_assignment_expression() ?ast.Expr {
	// Check if we have an identifier followed by assignment operator or type annotation
	if ep.current is lexer.IdentToken {
		ident := ep.current as lexer.IdentToken

		// Look ahead to next token
		next_token := ep.peek()

		if next_token is lexer.OperatorToken {
			op_token := next_token as lexer.OperatorToken
			if op_token.value == .assign {
				// Save the position BEFORE consuming the identifier
				ident_position := ast.new_position(ident.position.line, ident.position.column,
					ident.position.filename)

				// Consume the identifier
				ep.advance()
				// Consume the assignment operator
				ep.advance()

				// Parse the right-hand side expression
				value := ep.parse_assignment_expression()?

				return ast.AssignExpr{
					name:            ident.value
					value:           value
					type_annotation: none
					position:        ident_position
				}
			} else if op_token.value == .type_cons {
				// Type annotation: var :: Type = value
				ident_position := ast.new_position(ident.position.line, ident.position.column,
					ident.position.filename)

				// Consume the identifier
				ep.advance()
				// Consume the :: operator
				ep.advance()

				// Parse the type annotation
				type_annotation := ep.parse_type_expression()?

				// Expect assignment operator
				if !ep.check(lexer.operator(.assign)) {
					ep.add_error('Expected = after type annotation', 'Got ${ep.current.str()}')
					return none
				}
				ep.advance() // consume '='

				// Parse the right-hand side expression
				value := ep.parse_assignment_expression()?

				return ast.AssignExpr{
					name:            ident.value
					value:           value
					type_annotation: type_annotation
					position:        ident_position
				}
			}
		}
	}

	// If not an assignment, fall back to logical OR
	return ep.parse_or_expression()
}

// parse_or_expression parses expressions with 'or' precedence
fn (mut ep ExpressionParser) parse_or_expression() ?ast.Expr {
	mut left := ep.parse_and_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value != .or_ {
			break
		}
		ep.advance()
		right := ep.parse_and_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .or
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_and_expression parses expressions with 'and' precedence
fn (mut ep ExpressionParser) parse_and_expression() ?ast.Expr {
	mut left := ep.parse_comparison_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value != .and_ {
			break
		}
		ep.advance()
		right := ep.parse_comparison_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .and
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_comparison_expression parses comparison expressions
fn (mut ep ExpressionParser) parse_comparison_expression() ?ast.Expr {
	mut left := ep.parse_list_cons_expression()?

	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		op := match op_token.value {
			.eq { ast.BinaryOp.equal }
			.neq { ast.BinaryOp.not_equal }
			.lt { ast.BinaryOp.less_than }
			.gt { ast.BinaryOp.greater_than }
			.leq { ast.BinaryOp.less_equal }
			.geq { ast.BinaryOp.greater_equal }
			else { break }
		}

		ep.advance()
		right := ep.parse_list_cons_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

fn (mut ep ExpressionParser) parse_list_cons_expression() ?ast.Expr {
	mut left := ep.parse_additive_expression()?

	if ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value == .pipe {
			ep.advance()
			right := ep.parse_additive_expression()?

			return ast.ListConsExpr{
				head:     left
				tail:     right
				position: ep.get_current_position()
			}
		}
	}

	return left
}

fn (mut ep ExpressionParser) parse_additive_expression() ?ast.Expr {
	mut left := ep.parse_multiplicative_expression()?

	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		op := match op_token.value {
			.plus { ast.BinaryOp.add }
			.minus { ast.BinaryOp.subtract }
			else { break }
		}

		ep.advance()
		right := ep.parse_multiplicative_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_multiplicative_expression parses multiplication and division expressions
fn (mut ep ExpressionParser) parse_multiplicative_expression() ?ast.Expr {
	mut left := ep.parse_send_expression()?

	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		op := match op_token.value {
			.mult { ast.BinaryOp.multiply }
			.div { ast.BinaryOp.divide }
			else { break }
		}

		ep.advance()
		right := ep.parse_send_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_send_expression parses send expressions
fn (mut ep ExpressionParser) parse_send_expression() ?ast.Expr {
	mut left := ep.parse_postfix_expression()?

	// Check for send operator
	if ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value == .send {
			ep.advance() // consume '!'
			right := ep.parse_postfix_expression()?

			return ast.SendExpr{
				pid:      left
				message:  right
				position: ep.get_current_position()
			}
		}
	}

	return left
}

// parse_postfix_expression parses postfix expressions (function calls, record access, etc.)
fn (mut ep ExpressionParser) parse_postfix_expression() ?ast.Expr {
	mut expr := ep.parse_atom_expression()?

	// Handle postfix operations
	for {
		// Check for function call
		if ep.check(lexer.punctuation(.lparen)) {
			ep.advance() // consume '('
			mut arguments := []ast.Expr{}

			if !ep.check(lexer.punctuation(.rparen)) {
				for {
					arguments << ep.parse_expression()?
					if !ep.match(lexer.punctuation(.comma)) {
						break
					}
				}
			}

			ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

			// Check if this is an external function call (atom.function pattern)
			if expr is ast.RecordAccessExpr {
				record_access := expr as ast.RecordAccessExpr
				if record_access.record is ast.LiteralExpr {
					lit_expr := record_access.record as ast.LiteralExpr
					if lit_expr.value is ast.AtomLiteral {
						atom_lit := lit_expr.value as ast.AtomLiteral
						// This is an external call: :module.function()
						expr = ast.CallExpr{
							function:      ast.LiteralExpr{
								value: ast.NilLiteral{}
							} // placeholder
							external:      true
							module:        atom_lit.value
							function_name: record_access.field
							arguments:     arguments
							position:      ep.get_current_position()
						}
					} else {
						// Regular function call
						expr = ast.CallExpr{
							function:  expr
							arguments: arguments
							position:  ep.get_current_position()
						}
					}
				} else {
					// Regular function call
					expr = ast.CallExpr{
						function:  expr
						arguments: arguments
						position:  ep.get_current_position()
					}
				}
			} else {
				// Regular function call
				expr = ast.CallExpr{
					function:  expr
					arguments: arguments
					position:  ep.get_current_position()
				}
			}
		} else if ep.check(lexer.operator(.dot)) {
			// Record access
			ep.advance() // consume '.'

			// Expect an identifier after the dot
			if !ep.current.is_identifier() {
				ep.add_error('Expected field name after .', 'Got ${ep.current.str()}')
				return none
			}

			field_name := ep.current.get_value()
			ep.advance()

			expr = ast.RecordAccessExpr{
				record:   expr
				field:    field_name
				position: ep.get_current_position()
			}
		} else {
			break
		}
	}

	return expr
}

// parse_primary_expression parses primary expressions
fn (mut ep ExpressionParser) parse_primary_expression() ?ast.Expr {
	// This is a simplified version - will be expanded later
	return match ep.current {
		lexer.IdentToken {
			ident_token := ep.current as lexer.IdentToken
			name := ident_token.value
			position := ep.get_current_position()
			ep.advance()
			ast.VariableExpr{
				name:     name
				position: position
			}
		}
		lexer.IntToken {
			value := ep.current as lexer.IntToken
			ep.advance()
			ast.LiteralExpr{
				value: ast.IntegerLiteral{
					value: value.value
				}
			}
		}
		lexer.StringToken {
			value := ep.current as lexer.StringToken
			ep.advance()
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: value.value
				}
			}
		}
		lexer.BoolToken {
			value := ep.current as lexer.BoolToken
			ep.advance()
			ast.LiteralExpr{
				value: ast.BooleanLiteral{
					value: value.value
				}
			}
		}
		lexer.AtomToken {
			value := ep.current as lexer.AtomToken
			ep.advance()
			ast.LiteralExpr{
				value: ast.AtomLiteral{
					value: value.value
				}
			}
		}
		lexer.NilToken {
			ep.advance()
			ast.LiteralExpr{
				value: ast.NilLiteral{}
			}
		}
		lexer.KeywordToken {
			keyword_token := ep.current as lexer.KeywordToken
			match keyword_token.value {
				.nil_ {
					ep.advance()
					ast.LiteralExpr{
						value: ast.NilLiteral{}
					}
				}
				else {
					ep.add_error('Unexpected keyword in expression', 'Got ${ep.current.str()}')
					none
				}
			}
		}
		else {
			ep.add_error('Unexpected token in expression', 'Got ${ep.current.str()}')
			none
		}
	}
}

// parse_type_expression parses comprehensive type expressions for type annotations
fn (mut ep ExpressionParser) parse_type_expression() ?ast.TypeExpression {
	return ep.parse_union_type()
}

// parse_union_type parses union types (type1 | type2 | ...)
fn (mut ep ExpressionParser) parse_union_type() ?ast.TypeExpression {
	mut types := []ast.TypeExpression{}
	types << ep.parse_primary_type()?

	for ep.check_operator_value(.pipe) {
		ep.advance() // consume '|'
		types << ep.parse_primary_type()?
	}

	if types.len == 1 {
		return types[0]
	}

	return ast.UnionTypeExpr{
		types:    types
		position: ep.get_current_position()
	}
}

// parse_primary_type parses primary type expressions
fn (mut ep ExpressionParser) parse_primary_type() ?ast.TypeExpression {
	return match ep.current {
		lexer.IdentToken {
			ep.parse_named_type()
		}
		lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token.value {
				.lparen {
					ep.parse_function_or_tuple_type()
				}
				.lbrace {
					ep.parse_tuple_type()
				}
				else {
					ep.add_error('Unexpected token in type expression', 'Got ${ep.current.str()}')
					none
				}
			}
		}
		else {
			ep.add_error('Expected type expression', 'Got ${ep.current.str()}')
			none
		}
	}
}

// parse_named_type parses named types (simple, list, map, etc.)
fn (mut ep ExpressionParser) parse_named_type() ?ast.TypeExpression {
	ident_token := ep.current as lexer.IdentToken
	name := ident_token.value
	position := ep.get_current_position()
	ep.advance()

	// Check for parameterized types
	if ep.check(lexer.punctuation(.lparen)) {
		ep.advance() // consume '('

		match name {
			'list' {
				element_type := ep.parse_type_expression()?
				ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
				return ast.ListTypeExpr{
					element_type: element_type
					position:     position
				}
			}
			'map' {
				key_type := ep.parse_type_expression()?
				ep.consume(lexer.punctuation(.comma), 'Expected comma in map type')?
				value_type := ep.parse_type_expression()?
				ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
				return ast.MapTypeExpr{
					key_type:   key_type
					value_type: value_type
					position:   position
				}
			}
			else {
				ep.add_error('Unknown parameterized type: ${name}', 'Expected list or map')
				return none
			}
		}
	}

	// Check if it's a type variable (single lowercase letter)
	if name.len == 1 && name[0] >= `a` && name[0] <= `z` {
		return ast.VariableTypeExpr{
			name:     name
			position: position
		}
	}

	// Simple type
	return ast.SimpleTypeExpr{
		name:     name
		position: position
	}
}

// parse_function_or_tuple_type parses function types or parenthesized types
fn (mut ep ExpressionParser) parse_function_or_tuple_type() ?ast.TypeExpression {
	ep.advance() // consume '('

	if ep.check(lexer.punctuation(.rparen)) {
		// Empty parentheses - could be () -> ReturnType
		ep.advance() // consume ')'
		if ep.check(lexer.operator(.arrow)) {
			ep.advance() // consume '->'
			return_type := ep.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: []
				return_type: return_type
				position:    ep.get_current_position()
			}
		} else {
			ep.add_error('Expected -> after ()', 'Got ${ep.current.str()}')
			return none
		}
	}

	// Parse first type
	first_type := ep.parse_type_expression()?

	if ep.check(lexer.punctuation(.comma)) {
		// Multiple types - could be tuple or function parameters
		mut types := [first_type]

		for ep.match(lexer.punctuation(.comma)) {
			types << ep.parse_type_expression()?
		}

		ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

		// Check if it's a function type
		if ep.check(lexer.operator(.arrow)) {
			ep.advance() // consume '->'
			return_type := ep.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: types
				return_type: return_type
				position:    ep.get_current_position()
			}
		} else {
			// It's a tuple type
			return ast.TupleTypeExpr{
				element_types: types
				position:      ep.get_current_position()
			}
		}
	} else {
		ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

		// Check if it's a function type
		if ep.check(lexer.operator(.arrow)) {
			ep.advance() // consume '->'
			return_type := ep.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: [first_type]
				return_type: return_type
				position:    ep.get_current_position()
			}
		} else {
			// Just a parenthesized type
			return first_type
		}
	}
}

// parse_tuple_type parses tuple types {type1, type2, ...}
fn (mut ep ExpressionParser) parse_tuple_type() ?ast.TypeExpression {
	ep.advance() // consume '{'

	mut element_types := []ast.TypeExpression{}

	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			element_types << ep.parse_type_expression()?

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.TupleTypeExpr{
		element_types: element_types
		position:      ep.get_current_position()
	}
}

// check_operator_value checks if current token is a specific operator
fn (ep ExpressionParser) check_operator_value(op lexer.OperatorValue) bool {
	if ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		return op_token.value == op
	}
	return false
}

// Helper methods for error handling and position tracking
fn (mut ep ExpressionParser) add_error(message string, context string) {
	pos := ep.get_current_position()
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: context
		found:    ep.current.str()
	}), pos, '${message}: ${context}')
	ep.errors << comp_error
}

fn (ep ExpressionParser) get_current_position() ast.Position {
	// Use the current token's position if available
	pos := ep.current.get_position()
	return ast.new_position(pos.line, pos.column, pos.filename)
}
