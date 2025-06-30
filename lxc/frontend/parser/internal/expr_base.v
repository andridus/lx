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
	// Check if we have an identifier followed by assignment operator
	if ep.current is lexer.IdentToken {
		ident := ep.current as lexer.IdentToken

		// Look ahead to next token
		next_token := ep.peek()

		if next_token is lexer.OperatorToken {
			op_token := next_token as lexer.OperatorToken
			if op_token.value == .assign {
				// Consume the identifier
				ep.advance()
				// Consume the assignment operator
				ep.advance()

				// Parse the right-hand side expression
				value := ep.parse_assignment_expression()?

				return ast.AssignExpr{
					name:     ident.value
					value:    value
					position: ep.get_current_position()
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
	mut left := ep.parse_orelse_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value != .and_ {
			break
		}
		ep.advance()
		right := ep.parse_orelse_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .and
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_orelse_expression parses expressions with 'orelse' precedence
fn (mut ep ExpressionParser) parse_orelse_expression() ?ast.Expr {
	mut left := ep.parse_andalso_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value != .orelse {
			break
		}
		ep.advance()
		right := ep.parse_andalso_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .orelse
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_andalso_expression parses expressions with 'andalso' precedence
fn (mut ep ExpressionParser) parse_andalso_expression() ?ast.Expr {
	mut left := ep.parse_comparison_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value != .andalso {
			break
		}
		ep.advance()
		right := ep.parse_comparison_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .andalso
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
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
