module internal

import ast
import lexer

// ========================================
// MODULE STATEMENT PARSING
// Grammar: program ::= { module_statement }
// ========================================

// parse_program_statements parses the entire program as a sequence of module statements
pub fn (mut p LXParser) parse_program_statements() ?ast.ModuleStmt {
	mut statements := []ast.Stmt{}
	mut imports := []ast.Import{}
	mut exports := []string{}

	// Ensure we're in mod context
	if p.context != .mod {
		p.add_error('Program must be parsed in mod context', 'Invalid context')
		return none
	}

	p.skip_newlines()

	// Parse module header if present
	if p.check(keyword_token(.module)) {
		module_info := p.parse_module_header()?
		exports = module_info.exports.clone()
		imports = module_info.imports.clone()
	}

	// Parse all module statements
	for !p.is_at_end() {
		p.skip_newlines()

		if p.is_at_end() {
			break
		}

		stmt := p.parse_module_statement()?
		statements << stmt
	}

	return ast.ModuleStmt{
		name:       if exports.len > 0 { exports[0] } else { 'main' }
		exports:    exports
		imports:    imports
		statements: statements
		position:   p.get_current_position()
	}
}

// ========================================
// MODULE STATEMENT DISPATCHER
// Grammar: module_statement ::= function_definition | record_definition | type_definition | ...
// ========================================

// parse_module_statement parses a single module statement
// Only structural declarations are allowed at module level
fn (mut p LXParser) parse_module_statement() ?ast.Stmt {
	// Skip directive tokens
	if p.current is lexer.DirectiveToken {
		p.advance()
		return p.parse_module_statement()
	}

	return match p.current {
		lexer.KeywordToken {
			keyword := p.current as lexer.KeywordToken
			match keyword.value {
				.def {
					p.parse_function_definition()
				}
				.defp {
					p.parse_private_function_definition()
				}
				.record {
					p.parse_record_definition()
				}
				.type_ {
					p.parse_type_definition()
				}
				.spec {
					p.parse_spec_definition()
				}
				.test_ {
					p.parse_test_definition()
				}
				.worker {
					p.parse_worker_definition()
				}
				.supervisor {
					p.parse_supervisor_definition()
				}
				.describe {
					p.parse_describe_block()
				}
				else {
					p.add_error('Invalid module statement', 'Expected def, record, type, spec, test, worker, supervisor, or describe')
					none
				}
			}
		}
		else {
			p.add_error('Expressions not allowed at module level', 'Only structural declarations are permitted')
			none
		}
	}
}

// ========================================
// FUNCTION DEFINITIONS
// Grammar: function_definition ::= 'def' identifier '(' param_list ')' block_expression
// ========================================

// parse_function_definition parses public function definitions
fn (mut p LXParser) parse_function_definition() ?ast.Stmt {
	p.advance() // consume 'def'

	// Parse function name
	if !p.current.is_identifier() {
		p.add_error('Expected function name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse function clauses
	clauses := p.parse_function_clauses()?

	return ast.FunctionStmt{
		id:         '' // Will be filled by semantic analysis
		name:       name
		clauses:    clauses
		is_private: false
		directives: []
		position:   p.get_current_position()
	}
}

// parse_private_function_definition parses private function definitions
fn (mut p LXParser) parse_private_function_definition() ?ast.Stmt {
	p.advance() // consume 'defp'

	// Parse function name
	if !p.current.is_identifier() {
		p.add_error('Expected function name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse function clauses
	clauses := p.parse_function_clauses()?

	return ast.FunctionStmt{
		id:         '' // Will be filled by semantic analysis
		name:       name
		clauses:    clauses
		is_private: true
		directives: []
		position:   p.get_current_position()
	}
}

// ========================================
// RECORD DEFINITIONS
// Grammar: record_definition ::= 'record' identifier '{' field_list '}'
// ========================================

// parse_record_definition parses record definitions
fn (mut p LXParser) parse_record_definition() ?ast.Stmt {
	p.advance() // consume 'record'

	// Parse record name
	if !p.current.is_identifier() {
		p.add_error('Expected record name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse record fields
	p.consume(punctuation_token(.lbrace), 'Expected { after record name')?

	mut fields := []ast.RecordFieldDef{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			field := p.parse_record_field()?
			fields << field

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after record fields')?

	return ast.RecordDefStmt{
		name:     name
		fields:   fields
		position: p.get_current_position()
	}
}

// ========================================
// TYPE DEFINITIONS
// Grammar: type_definition ::= 'type' identifier '::' type_expression
// ========================================

// parse_type_definition parses type alias definitions
fn (mut p LXParser) parse_type_definition() ?ast.Stmt {
	p.advance() // consume 'type'

	// Parse type name
	if !p.current.is_identifier() {
		p.add_error('Expected type name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse type annotation
	p.consume(operator_token(.type_cons), 'Expected :: after type name')?

	type_expr := p.parse_type_expression()?

	return ast.TypeAliasStmt{
		name:       name
		type_expr:  type_expr
		alias_type: .regular
		position:   p.get_current_position()
	}
}

// ========================================
// SPEC DEFINITIONS
// Grammar: spec_definition ::= 'spec' identifier '::' type_expression
// ========================================

// parse_spec_definition parses function specifications
fn (mut p LXParser) parse_spec_definition() ?ast.Stmt {
	p.advance() // consume 'spec'

	// Parse function name
	if !p.current.is_identifier() {
		p.add_error('Expected function name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse spec annotation
	p.consume(operator_token(.type_cons), 'Expected :: after function name')?

	_ := p.parse_type_expression()? // Parse but don't use for now

	// For now, return as ModuleStmt since SpecStmt doesn't exist yet
	return ast.ModuleStmt{
		name:       'spec_' + name
		exports:    []
		imports:    []
		statements: []
		position:   p.get_current_position()
	}
}

// ========================================
// TEST DEFINITIONS
// Grammar: test_definition ::= 'test' string_literal block_expression
// ========================================

// parse_test_definition parses test definitions
fn (mut p LXParser) parse_test_definition() ?ast.Stmt {
	p.advance() // consume 'test'

	// Parse test name
	if p.current !is lexer.StringToken {
		p.add_error('Expected test name string', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse test body as block expression
	_ := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_block_expression()
	})? // Parse but don't use for now

	// For now, return as ModuleStmt since TestStmt doesn't exist yet
	return ast.ModuleStmt{
		name:       'test_' + name
		exports:    []
		imports:    []
		statements: []
		position:   p.get_current_position()
	}
}

// ========================================
// WORKER/SUPERVISOR DEFINITIONS
// Grammar: worker_definition ::= 'worker' identifier block_top_level
// Grammar: supervisor_definition ::= 'supervisor' identifier block_top_level
// ========================================

// parse_worker_definition parses worker definitions
fn (mut p LXParser) parse_worker_definition() ?ast.Stmt {
	p.advance() // consume 'worker'

	// Parse worker name
	if !p.current.is_identifier() {
		p.add_error('Expected worker name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse worker body as block_top_level (mod context)
	body := p.parse_block_top_level()?

	// For now, return as ModuleStmt since WorkerStmt doesn't exist yet
	return ast.ModuleStmt{
		name:       'worker_' + name
		exports:    []
		imports:    []
		statements: body
		position:   p.get_current_position()
	}
}

// parse_supervisor_definition parses supervisor definitions
fn (mut p LXParser) parse_supervisor_definition() ?ast.Stmt {
	p.advance() // consume 'supervisor'

	// Parse supervisor name
	if !p.current.is_identifier() {
		p.add_error('Expected supervisor name', 'Got ${p.current.str()}')
		return none
	}

	name := p.current.get_value()
	p.advance()

	// Parse supervisor body as block_top_level (mod context)
	body := p.parse_block_top_level()?

	// For now, return as ModuleStmt since SupervisorStmt doesn't exist yet
	return ast.ModuleStmt{
		name:       'supervisor_' + name
		exports:    []
		imports:    []
		statements: body
		position:   p.get_current_position()
	}
}

// ========================================
// DESCRIBE BLOCKS
// Grammar: describe_block ::= 'describe' string_literal block_top_level
// ========================================

// parse_describe_block parses describe blocks for testing
fn (mut p LXParser) parse_describe_block() ?ast.Stmt {
	p.advance() // consume 'describe'

	// Parse description string
	if p.current !is lexer.StringToken {
		p.add_error('Expected description string', 'Got ${p.current.str()}')
		return none
	}

	description := p.current.get_value()
	p.advance()

	// Parse describe body as block_top_level (mod context)
	body := p.parse_block_top_level()?

	// For now, return as ModuleStmt since DescribeStmt doesn't exist yet
	return ast.ModuleStmt{
		name:       'describe_' + description
		exports:    []
		imports:    []
		statements: body
		position:   p.get_current_position()
	}
}

// ========================================
// HELPER FUNCTIONS
// ========================================

// ModuleHeaderInfo represents module header information
struct ModuleHeaderInfo {
	exports []string
	imports []ast.Import
}

// parse_module_header parses module header with imports and exports
fn (mut p LXParser) parse_module_header() ?ModuleHeaderInfo {
	mut exports := []string{}
	mut imports := []ast.Import{}

	p.advance() // consume 'module'

	// Parse module name (currently not used, but consume it)
	if !p.current.is_identifier() {
		p.add_error('Expected module name', 'Got ${p.current.str()}')
		return ModuleHeaderInfo{exports: exports, imports: imports}
	}
	p.advance()

	// Parse exports if present
	if p.match(punctuation_token(.lbracket)) {
		for !p.check(punctuation_token(.rbracket)) {
			export := p.current.get_value()
			if !p.current.is_identifier() {
				p.add_error('Expected export name', 'Got ${p.current.str()}')
				break
			}
			p.advance()
			exports << export

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
		p.consume(punctuation_token(.rbracket), 'Expected closing bracket')?
	}

	// Parse imports if present
	if p.match(keyword_token(.import)) {
		for {
			import_stmt := p.parse_import_statement()?
			imports << import_stmt

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.lbrace), 'Expected opening brace')?

	return ModuleHeaderInfo{exports: exports, imports: imports}
}

// parse_import_statement parses import statements
fn (mut p LXParser) parse_import_statement() ?ast.Import {
	module_name := p.current.get_value()
	if !p.current.is_identifier() {
		p.add_error('Expected module name in import', 'Got ${p.current.str()}')
		return ast.Import{}
	}
	p.advance()

	mut aliases := []string{}

	// Parse aliases if present
	if p.match(punctuation_token(.lbracket)) {
		for !p.check(punctuation_token(.rbracket)) {
			alias := p.current.get_value()
			if !p.current.is_identifier() {
				p.add_error('Expected alias name', 'Got ${p.current.str()}')
				break
			}
			p.advance()
			aliases << alias

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
		p.consume(punctuation_token(.rbracket), 'Expected closing bracket')?
	}

	return ast.Import{
		module:   module_name
		aliases:  aliases
		position: p.get_current_position()
	}
}