module parser

import ast
import lexer

// ModuleParser handles parsing of complete LX modules
@[heap]
pub struct ModuleParser {
	Parser
mut:
	statement_parser StatementParser
pub:
	module_name string
}

pub fn new_module_parser(tokens []lexer.Token, module_name string) &ModuleParser {
	return &ModuleParser{
		Parser:           new_parser(tokens)
		module_name:      module_name
		statement_parser: new_statement_parser(tokens)
	}
}

// parse_module parses a complete LX module
pub fn (mut mp ModuleParser) parse_module() ?ast.ModuleStmt {
	mut statements := []ast.Stmt{}

	for !mp.is_at_end() {
		stmt := mp.parse_statement()?
		statements << stmt
	}

	return ast.ModuleStmt{
		name:       mp.module_name
		statements: statements
	}
}

// parse_statement parses a single statement at module level
fn (mut mp ModuleParser) parse_statement() ?ast.Stmt {
	return mp.statement_parser.parse_statement()
}
