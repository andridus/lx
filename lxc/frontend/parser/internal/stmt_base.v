module internal

import ast
import lexer
import errors

// StatementParser handles parsing of all statement types in LX
@[heap]
pub struct StatementParser {
	Parser
}

fn (sp StatementParser) current() lexer.Token {
	return sp.tokens[sp.position]
}

pub fn new_statement_parser(tokens []lexer.Token) &StatementParser {
	return &StatementParser{
		Parser: new_parser(tokens)
	}
}

// sync_current_token ensures the current token is always synchronized with the position
fn (mut sp StatementParser) sync_current_token() {
	if sp.position < sp.tokens.len {
		sp.current = sp.tokens[sp.position]
	} else {
		sp.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}
}

// safe_advance advances the parser and ensures current token is synchronized
fn (mut sp StatementParser) safe_advance() {
	sp.position++
	sp.sync_current_token()
}

// parse_statement parses a single statement
fn (mut sp StatementParser) parse_statement() ?ast.Stmt {
	// Skip directive tokens and go to the next token
	if sp.current is lexer.DirectiveToken {
		sp.advance()
		return sp.parse_statement()
	}

	return match sp.current {
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token.value {
				.def { sp.parse_function_statement() }
				.defp { sp.parse_private_function_statement() }
				.record { sp.parse_record_definition() }
				.type_ { sp.parse_type_alias_statement_with_modifier() }
				.worker { sp.parse_worker_statement() }
				.supervisor { sp.parse_supervisor_statement() }
				.spec { sp.parse_specification_statement() }
				.describe { sp.parse_test_describe_statement() }
				.test_ { sp.parse_test_statement() }
				else { sp.parse_expression_statement() }
			}
		}
		else {
			sp.parse_expression_statement()
		}
	}
}

// parse_expression_statement parses an expression as a statement
fn (mut sp StatementParser) parse_expression_statement() ?ast.Stmt {
	// Parse the expression using the current parser state
	expr := sp.parse_expression()?

	return ast.ExprStmt{
		expr: expr
	}
}

// parse_pattern parses patterns for pattern matching
fn (mut sp StatementParser) parse_pattern() ?ast.Pattern {
	mut expr_parser := new_expression_parser(sp.tokens)
	expr_parser.position = sp.position
	expr_parser.current = sp.current

	pattern := expr_parser.parse_pattern()?

	// Advance the main parser to the position where expression parser ended
	sp.position = expr_parser.position
	sp.sync_current_token()

	return pattern
}

// parse_expression parses expressions
fn (mut sp StatementParser) parse_expression() ?ast.Expr {
	mut expr_parser := new_expression_parser(sp.tokens)
	expr_parser.position = sp.position
	expr_parser.current = sp.current

	expr := expr_parser.parse_expression()?

	// Advance the main parser to the position where expression parser ended
	sp.position = expr_parser.position
	sp.sync_current_token()

	return expr
}

// parse_type parses type expressions
fn (mut sp StatementParser) parse_type() ?ast.LXType {
	return match sp.current {
		lexer.IdentToken {
			type_name := sp.current.value
			sp.advance()
			match type_name {
				'integer' { ast.LXType.integer }
				'float' { ast.LXType.float }
				'string' { ast.LXType.string }
				'boolean' { ast.LXType.boolean }
				'atom' { ast.LXType.atom }
				'nil' { ast.LXType.nil }
				'list' { ast.LXType.list }
				'tuple' { ast.LXType.tuple }
				'map' { ast.LXType.map }
				'record' { ast.LXType.record }
				'function' { ast.LXType.function }
				'pid' { ast.LXType.pid }
				'reference' { ast.LXType.reference }
				'port' { ast.LXType.port }
				'binary' { ast.LXType.binary }
				'bitstring' { ast.LXType.bitstring }
				'any' { ast.LXType.any }
				else { ast.LXType.unknown }
			}
		}
		else {
			sp.add_error('Expected type name', 'Got ${sp.current.str()}')
			ast.LXType.unknown
		}
	}
}

// parse_type_expression parses comprehensive type expressions for type annotations
fn (mut sp StatementParser) parse_type_expression() ?ast.TypeExpression {
	return sp.parse_union_type()
}

// parse_union_type parses union types (type1 | type2 | ...)
fn (mut sp StatementParser) parse_union_type() ?ast.TypeExpression {
	mut types := []ast.TypeExpression{}
	types << sp.parse_primary_type()?

	for sp.check_operator_value(.pipe) {
		sp.advance() // consume '|'
		types << sp.parse_primary_type()?
	}

	if types.len == 1 {
		return types[0]
	}

	return ast.UnionTypeExpr{
		types:    types
		position: sp.get_current_position()
	}
}

// parse_primary_type parses primary type expressions
fn (mut sp StatementParser) parse_primary_type() ?ast.TypeExpression {
	return match sp.current {
		lexer.IdentToken {
			sp.parse_named_type()
		}
		lexer.PunctuationToken {
			punc_token := sp.current as lexer.PunctuationToken
			match punc_token.value {
				.lparen {
					sp.parse_function_or_tuple_type()
				}
				.lbrace {
					sp.parse_tuple_type()
				}
				else {
					sp.add_error('Unexpected token in type expression', 'Got ${sp.current.str()}')
					none
				}
			}
		}
		else {
			sp.add_error('Expected type expression', 'Got ${sp.current.str()}')
			none
		}
	}
}

// parse_named_type parses named types (simple, list, map, etc.)
fn (mut sp StatementParser) parse_named_type() ?ast.TypeExpression {
	ident_token := sp.current as lexer.IdentToken
	name := ident_token.value
	position := sp.get_current_position()
	sp.advance()

	// Check for parameterized types
	if sp.check(lexer.punctuation(.lparen)) {
		sp.advance() // consume '('

		match name {
			'list' {
				element_type := sp.parse_type_expression()?
				sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
				return ast.ListTypeExpr{
					element_type: element_type
					position:     position
				}
			}
			'map' {
				key_type := sp.parse_type_expression()?
				sp.consume(lexer.punctuation(.comma), 'Expected comma in map type')?
				value_type := sp.parse_type_expression()?
				sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
				return ast.MapTypeExpr{
					key_type:   key_type
					value_type: value_type
					position:   position
				}
			}
			else {
				sp.add_error('Unknown parameterized type: ${name}', 'Expected list or map')
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
fn (mut sp StatementParser) parse_function_or_tuple_type() ?ast.TypeExpression {
	sp.advance() // consume '('

	if sp.check(lexer.punctuation(.rparen)) {
		// Empty parentheses - could be () -> ReturnType
		sp.advance() // consume ')'
		if sp.check(lexer.operator(.arrow)) {
			sp.advance() // consume '->'
			return_type := sp.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: []
				return_type: return_type
				position:    sp.get_current_position()
			}
		} else {
			sp.add_error('Expected -> after ()', 'Got ${sp.current.str()}')
			return none
		}
	}

	// Parse first type
	first_type := sp.parse_type_expression()?

	if sp.check(lexer.punctuation(.comma)) {
		// Multiple types - could be tuple or function parameters
		mut types := [first_type]

		for sp.match(lexer.punctuation(.comma)) {
			types << sp.parse_type_expression()?
		}

		sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

		// Check if it's a function type
		if sp.check(lexer.operator(.arrow)) {
			sp.advance() // consume '->'
			return_type := sp.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: types
				return_type: return_type
				position:    sp.get_current_position()
			}
		} else {
			// It's a tuple type
			return ast.TupleTypeExpr{
				element_types: types
				position:      sp.get_current_position()
			}
		}
	} else {
		sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

		// Check if it's a function type
		if sp.check(lexer.operator(.arrow)) {
			sp.advance() // consume '->'
			return_type := sp.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: [first_type]
				return_type: return_type
				position:    sp.get_current_position()
			}
		} else {
			// Just a parenthesized type
			return first_type
		}
	}
}

// parse_tuple_type parses tuple types {type1, type2, ...}
fn (mut sp StatementParser) parse_tuple_type() ?ast.TypeExpression {
	sp.advance() // consume '{'

	mut element_types := []ast.TypeExpression{}

	if !sp.check(lexer.punctuation(.rbrace)) {
		for {
			element_types << sp.parse_type_expression()?

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.TupleTypeExpr{
		element_types: element_types
		position:      sp.get_current_position()
	}
}

// check_operator_value checks if current token is a specific operator
fn (sp StatementParser) check_operator_value(op lexer.OperatorValue) bool {
	if sp.current is lexer.OperatorToken {
		op_token := sp.current as lexer.OperatorToken
		return op_token.value == op
	}
	return false
}

// Helper methods for error handling and position tracking
fn (mut sp StatementParser) add_error(message string, context string) {
	pos := sp.get_current_position()
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: context
		found:    sp.current.str()
	}), pos, '${message}: ${context}')
	sp.errors << comp_error
}

fn (sp StatementParser) get_current_position() ast.Position {
	// Use the current token's position if available
	pos := sp.current.get_position()
	return ast.new_position(pos.line, pos.column, pos.filename)
}
