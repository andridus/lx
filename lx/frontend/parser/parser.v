module parser

import ast
import lexer
import errors
import internal

// ========================================
// PUBLIC API - SINGLE ENTRY POINT
// ========================================

// Parser is the main entry point for parsing LX code
// It follows the grammar specification strictly with clear context separation
pub struct Parser {
mut:
	lexer_tokens    []lexer.Token
	current_pos     int
	errors          []errors.CompilationError
	module_name     string
	global_registry &internal.GlobalRegistry
}

// new_parser creates a new parser instance with the given tokens
pub fn new_parser(tokens []lexer.Token, module_name string, global_registry &internal.GlobalRegistry) Parser {
	return Parser{
		lexer_tokens:    tokens
		current_pos:     0
		errors:          []
		module_name:     module_name
		global_registry: global_registry
	}
}

// parse_program is the SINGLE ENTRY POINT for parsing
// It parses the entire program according to the grammar:
// program ::= { module_statement } | application_definition
pub fn (mut p Parser) parse_program() ?ast.Stmt {
	// Create internal parser with proper context
	mut internal_parser := internal.new_lx_parser(p.lexer_tokens, p.module_name, p.global_registry)
	// Parse the complete program
	result := internal_parser.parse_program_statements() or {
		p.errors << internal_parser.get_errors()
		return none
	}
	// Copy any errors from internal parser
	p.errors << internal_parser.get_errors()

	return result
}

// get_errors returns all parsing errors
pub fn (p Parser) get_errors() []errors.CompilationError {
	return p.errors
}

// has_errors checks if there are any parsing errors
pub fn (p Parser) has_errors() bool {
	return p.errors.len > 0
}
