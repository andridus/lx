module internal

import ast
import lexer
import errors

@[heap]
pub struct MainParser {
	Parser
}

pub fn new_main_parser(tokens []lexer.Token) &MainParser {
	return &MainParser{
		Parser: new_parser(tokens)
	}
}

pub fn get_errors(mp MainParser) []errors.CompilationError {
	return mp.errors
}

// has_errors checks if the parser has any errors
pub fn has_errors(mp MainParser) bool {
	return mp.errors.len > 0
}

// parse_module_internal parses an entire LX module
pub fn (mut mp MainParser) parse_module_internal() ?ast.ModuleStmt {
	mut statements := []ast.Stmt{}
	mut imports := []ast.Import{}
	mut exports := []string{}

	// Parse module header if present
	if mp.check(lexer.KeywordToken.module) {
		mp.advance() // consume 'module'

		if !mp.current.is_identifier() {
			mp.add_error('Expected module name', 'Got ${mp.current.str()}')
			return none
		}
		mp.advance()

		// Parse exports if present
		if mp.match(lexer.PunctuationToken.lbracket) {
			for !mp.check(lexer.PunctuationToken.rbracket) {
				export := mp.current.get_value()
				if !mp.current.is_identifier() {
					mp.add_error('Expected export name', 'Got ${mp.current.str()}')
					return none
				}
				mp.advance()
				exports << export

				if !mp.match(lexer.PunctuationToken.comma) {
					break
				}
			}
			mp.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?
		}

		// Parse imports if present
		if mp.match(lexer.KeywordToken.import) {
			for {
				import_stmt := mp.parse_import_statement()?
				imports << import_stmt

				if !mp.match(lexer.PunctuationToken.comma) {
					break
				}
			}
		}

		mp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?
	}

	// Parse all statements in the module
	for !mp.is_at_end() {
		if mp.check(lexer.PunctuationToken.rbrace) {
			break
		}

		stmt := mp.parse_statement()?
		statements << stmt
	}

	// Consume closing brace if present
	if mp.check(lexer.PunctuationToken.rbrace) {
		mp.advance()
	}

	return ast.ModuleStmt{
		name:       if exports.len > 0 { exports[0] } else { 'main' }
		exports:    exports
		imports:    imports
		statements: statements
		position:   mp.get_current_position()
	}
}

// parse_statement parses a single statement using the statement parser
fn (mut mp MainParser) parse_statement() ?ast.Stmt {
	mut stmt_parser := new_statement_parser(mp.tokens)
	stmt_parser.position = mp.position
	stmt_parser.current = mp.current

	stmt := stmt_parser.parse_statement() or {
		mp.errors << stmt_parser.errors
		return none
	}

	// Advance the main parser to the position where statement parser ended
	mp.position = stmt_parser.position
	if mp.position < mp.tokens.len {
		mp.current = mp.tokens[mp.position]
	} else {
		mp.current = lexer.EOFToken{}
	}

	// Copy any errors from the statement parser


	return stmt
}

// parse_import_statement parses import statements
fn (mut mp MainParser) parse_import_statement() ?ast.Import {
	module_name := mp.current.get_value()
	if !mp.current.is_identifier() {
		mp.add_error('Expected module name in import', 'Got ${mp.current.str()}')
		return none
	}
	mp.advance()

	mut aliases := []string{}

	// Parse aliases if present
	if mp.match(lexer.PunctuationToken.lbracket) {
		for !mp.check(lexer.PunctuationToken.rbracket) {
			alias := mp.current.get_value()
			if !mp.current.is_identifier() {
				mp.add_error('Expected alias name', 'Got ${mp.current.str()}')
				return none
			}
			mp.advance()
			aliases << alias

			if !mp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
		mp.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?
	}

	return ast.Import{
		module:   module_name
		aliases:  aliases
		position: mp.get_current_position()
	}
}

// parse_expression_internal parses expressions using the expression parser
pub fn (mut mp MainParser) parse_expression_internal() ?ast.Expr {
	mut expr_parser := new_expression_parser(mp.tokens)
	expr_parser.position = mp.position
	expr_parser.current = mp.current

	expr := expr_parser.parse_expression() or {
		mp.errors << expr_parser.errors
		return none
	}

	// Advance the main parser to the position where expression parser ended
	mp.position = expr_parser.position
	if mp.position < mp.tokens.len {
		mp.current = mp.tokens[mp.position]
	} else {
		mp.current = lexer.EOFToken{}
	}

	// Copy any errors from the expression parser


	return expr
}

// parse_pattern_internal parses patterns using the expression parser
pub fn (mut mp MainParser) parse_pattern_internal() ?ast.Pattern {
	mut expr_parser := new_expression_parser(mp.tokens)
	expr_parser.position = mp.position
	expr_parser.current = mp.current

	pattern := expr_parser.parse_pattern() or {
		mp.errors << expr_parser.errors
		return none
	}

	// Advance the main parser to the position where expression parser ended
	mp.position = expr_parser.position
	if mp.position < mp.tokens.len {
		mp.current = mp.tokens[mp.position]
	} else {
		mp.current = lexer.EOFToken{}
	}

	// Copy any errors from the expression parser


	return pattern
}

// Helper methods for error handling and position tracking
fn (mut mp MainParser) add_error(message string, context string) {
	pos := mp.get_current_position()
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: context
		found:    mp.current.str()
	}), pos, '${message}: ${context}')
	mp.errors << comp_error
}

fn (mp MainParser) get_current_position() ast.Position {
	return ast.Position{
		line:   1
		column: mp.position + 1
	}
}
