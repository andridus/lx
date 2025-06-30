module parser

import ast
import lexer
import errors
import internal

// PUBLIC ENTRYPOINTS - These functions are called from outside the parser module
pub type MainParser = internal.MainParser
pub type ExpressionParser = internal.ExpressionParser
pub type PrecedenceTable = internal.PrecedenceTable

// new_main_parser creates a new main parser instance
pub fn new_main_parser(tokens []lexer.Token) &MainParser {
	return internal.new_main_parser(tokens)
}

// new_expression_parser creates a new expression parser instance
pub fn new_expression_parser(tokens []lexer.Token) &ExpressionParser {
	return internal.new_expression_parser(tokens)
}

// new_precedence_table creates a new precedence table
pub fn new_precedence_table() PrecedenceTable {
	return internal.new_precedence_table()
}

// MainParser is the main parser that orchestrates the entire parsing process

// parse_module parses an entire LX module
pub fn (mut mp MainParser) parse_module() ?ast.ModuleStmt {
	// Implementation delegated to internal parser logic
	return mp.parse_module_internal()
}

// parse_expression parses expressions using the expression parser
pub fn (mut mp MainParser) parse_expression() ?ast.Expr {
	// Implementation delegated to expression parser
	return mp.parse_expression_internal()
}

// parse_pattern parses patterns using the expression parser
pub fn (mut mp MainParser) parse_pattern() ?ast.Pattern {
	// Implementation delegated to expression parser
	return mp.parse_pattern_internal()
}

// get_errors returns compilation errors from the parser
pub fn (mp MainParser) get_errors() []errors.CompilationError {
	return internal.get_errors(mp)
}

// has_errors checks if the parser has any errors
pub fn (mp MainParser) has_errors() bool {
	return internal.has_errors(mp)
}
