module internal

import ast
import lexer

// ========================================
// MODULE STATEMENT PARSING
// Grammar: program ::= { module_statement }
// ========================================

// parse_program_statements parses the entire program as a sequence of module statements
// Returns either a ModuleStmt or ApplicationStmt depending on content
pub fn (mut p LXParser) parse_program_statements() ?ast.Stmt {
	mut statements := []ast.Stmt{}
	mut imports := []ast.Import{}
	mut exports := []string{}

	// Ensure we're in mod context
	if p.context != .mod {
		p.add_error('Program must be parsed in mod context', 'Invalid context')
		return none
	}

	p.skip_newlines()

	// Check if this is an application definition
	if p.check(keyword_token(.application)) {
		// Parse application definition
		return p.parse_application_definition()
	}

	// Parse module header if present
	mut module_name := 'main'
	if p.check(keyword_token(.module)) {
		module_info := p.parse_module_header()?
		exports = module_info.exports.clone()
		imports = module_info.imports.clone()
		if exports.len > 0 {
			module_name = exports[0]
		}
	}

	// Parse all module statements
	for !p.is_at_end() {
		p.skip_newlines()

		if p.is_at_end() {
			break
		}

		stmt := p.parse_module_statement() or { return none }

		statements << stmt
	}

	return ast.ModuleStmt{
		name:       module_name
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
				.application {
					p.parse_application_definition()
				}
				else {
					p.add_error('Invalid module statement', 'Expected def, record, type, spec, test, worker, supervisor, describe, or application')
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
	clauses := p.parse_function_clauses() or { return none }

	return ast.FunctionStmt{
		id:         '' // Will be filled by semantic analysis
		name:       name
		clauses:    clauses
		is_private: false
		directives: []
		position:   p.get_current_position()
		ast_id:     p.generate_ast_id()
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
		ast_id:     p.generate_ast_id()
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

	record_stmt := ast.RecordDefStmt{
		name:     name
		fields:   fields
		position: p.get_current_position()
	}

	// Register record in global registry with key 'module.Record'
	p.global_registry.records['${p.module_name}.${name}'] = record_stmt

	return record_stmt
}

// ========================================
// TYPE DEFINITIONS
// Grammar: type_definition ::= 'type' identifier '::' type_expression
// ========================================

// parse_type_definition parses type alias definitions with optional modifiers
fn (mut p LXParser) parse_type_definition() ?ast.Stmt {
	p.advance() // consume 'type'

	// Check for optional modifier
	mut alias_type := ast.TypeAliasType.regular
	if p.check(keyword_token(.opaque)) {
		alias_type = ast.TypeAliasType.opaque
		p.advance()
	} else if p.check(keyword_token(.nominal)) {
		alias_type = ast.TypeAliasType.nominal
		p.advance()
	}

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

	type_alias_stmt := ast.TypeAliasStmt{
		name:       name
		type_expr:  type_expr
		alias_type: alias_type
		position:   p.get_current_position()
	}

	// Register type alias in global registry with key 'module.type'
	p.global_registry.types['${p.module_name}.${name}'] = type_alias_stmt

	return type_alias_stmt
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

	// Consume 'do'
	p.consume(keyword_token(.do_), 'Expected "do" after worker name')?

	// Parse worker body (statements)
	mut statements := []ast.Stmt{}

	p.skip_newlines()

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		p.skip_newlines()

		if p.check(keyword_token(.end_)) {
			break
		}

		stmt := p.parse_module_statement()?
		statements << stmt

		p.skip_newlines()
	}

	// Consume 'end'
	p.consume(keyword_token(.end_), 'Expected "end" after worker body')?

	return ast.WorkerStmt{
		name:       name
		statements: statements
		position:   p.get_current_position()
		ast_id:     p.generate_ast_id()
	}
}

// parse_supervisor_definition parses supervisor definitions
fn (mut p LXParser) parse_supervisor_definition() ?ast.Stmt {
	p.advance() // consume 'supervisor'

	// Parse optional supervisor name
	mut name := ''
	if p.current.is_identifier() {
		name = p.current.get_value()
		p.advance()
	}

	// Consume 'do'
	p.consume(keyword_token(.do_), 'Expected "do" after supervisor declaration')?

	// Parse supervisor body
	mut children := ast.ChildrenSpec(ast.ListChildren{
		children: []
		position: p.get_current_position()
	})
	mut strategy := ast.SupervisorStrategy.one_for_one
	mut statements := []ast.Stmt{}

	p.skip_newlines()

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		p.skip_newlines()

		if p.check(keyword_token(.end_)) {
			break
		}

		if p.check(keyword_token(.children)) {
			children = p.parse_children_spec()?
		} else if p.check(keyword_token(.strategy)) {
			strategy = p.parse_strategy_spec()?
		} else {
			// Regular module statement
			stmt := p.parse_module_statement()?
			statements << stmt
		}

		p.skip_newlines()
	}

	// Consume 'end'
	p.consume(keyword_token(.end_), 'Expected "end" after supervisor body')?

	return ast.SupervisorStmt{
		name:       name
		children:   children
		strategy:   strategy
		statements: statements
		position:   p.get_current_position()
		ast_id:     p.generate_ast_id()
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
		return ModuleHeaderInfo{
			exports: exports
			imports: imports
		}
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

	return ModuleHeaderInfo{
		exports: exports
		imports: imports
	}
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

// ========================================
// APPLICATION DEFINITION
// Grammar: application_definition ::= 'application' '{' application_fields '}'
// ========================================

// parse_application_definition parses application definitions
fn (mut p LXParser) parse_application_definition() ?ast.Stmt {
	p.advance() // consume 'application'

	p.consume(punctuation_token(.lbrace), 'Expected opening brace after application')?

	mut fields := map[string]ast.Expr{}

	// Parse application fields using record-like syntax
	if !p.check(punctuation_token(.rbrace)) {
		for {
			p.skip_newlines()

			if p.check(punctuation_token(.rbrace)) {
				break
			}

			// Parse field name (same as record field parsing)
			field_name := if p.current.is_key() {
				// Handle key tokens like description:
				key_value := p.current.get_key_value()
				p.advance()
				key_value
			} else {
				// Handle identifier followed by colon
				name := p.current.get_value()
				if !p.current.is_identifier() {
					p.add_error('Expected field name', 'Got ${p.current.str()}')
					return none
				}
				p.advance()
				p.consume(punctuation_token(.colon), 'Expected colon after field name')?
				name
			}

			// Parse field value as expression
			old_context := p.context
			p.context = .expression
			field_value := p.parse_expression()?
			p.context = old_context
			fields[field_name] = field_value

			// Optional comma
			if !p.match(punctuation_token(.comma)) {
				break
			}

			p.skip_newlines()
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected closing brace after application fields')?

	return ast.ApplicationStmt{
		fields:   fields
		position: p.get_current_position()
	}
}

// parse_children_spec parses children specification
fn (mut p LXParser) parse_children_spec() ?ast.ChildrenSpec {
	p.advance() // consume 'children'

	old_context := p.context
	p.context = .expression

	// Parse children expression
	expr := p.parse_expression()?

	p.context = old_context

	// Convert expression to ChildrenSpec
	return match expr {
		ast.ListLiteralExpr {
			p.parse_list_children_spec(expr)
		}
		ast.MapLiteralExpr {
			mut workers := []string{}
			mut supervisors := []string{}

			for entry in expr.entries {
				if entry.key is ast.LiteralExpr {
					literal_expr := entry.key as ast.LiteralExpr
					if literal_expr.value is ast.AtomLiteral {
						atom_literal := literal_expr.value as ast.AtomLiteral
						if atom_literal.value == 'worker' {
							if entry.value is ast.ListLiteralExpr {
								list_expr := entry.value as ast.ListLiteralExpr
								for element in list_expr.elements {
									if element is ast.VariableExpr {
										workers << element.name
									}
								}
							}
						} else if atom_literal.value == 'supervisor' {
							if entry.value is ast.ListLiteralExpr {
								list_expr := entry.value as ast.ListLiteralExpr
								for element in list_expr.elements {
									if element is ast.VariableExpr {
										supervisors << element.name
									}
								}
							}
						}
					}
				}
			}

			ast.MapChildren{
				workers:     workers
				supervisors: supervisors
				position:    expr.position
			}
		}
		else {
			p.add_error('Children must be a list or map', 'Got ${expr.str()}')
			return none
		}
	}
}

// parse_strategy_spec parses supervision strategy
fn (mut p LXParser) parse_strategy_spec() ?ast.SupervisorStrategy {
	p.advance() // consume 'strategy'

	// Check if current token is an atom
	if p.current !is lexer.AtomToken {
		p.add_error('Expected strategy atom', 'Got ${p.current.str()}')
		return ast.SupervisorStrategy.one_for_one
	}

	strategy_name := p.current.get_value()
	p.advance()

	return match strategy_name {
		'one_for_one' {
			ast.SupervisorStrategy.one_for_one
		}
		'one_for_all' {
			ast.SupervisorStrategy.one_for_all
		}
		'rest_for_one' {
			ast.SupervisorStrategy.rest_for_one
		}
		else {
			p.add_error('Invalid supervision strategy', 'Expected :one_for_one, :one_for_all, or :rest_for_one')
			ast.SupervisorStrategy.one_for_one
		}
	}
}

// parse_list_children_spec parses list-based children specification
fn (mut p LXParser) parse_list_children_spec(expr ast.ListLiteralExpr) ast.ChildrenSpec {
	// Check if it's a list of tuples or simple identifiers
	if expr.elements.len > 0 {
		first_element := expr.elements[0]
		if first_element is ast.TupleExpr {
			// List of tuples - detailed specification
			mut children := []ast.ChildTuple{}
			for element in expr.elements {
				if element is ast.TupleExpr {
					tuple_expr := element as ast.TupleExpr
					if tuple_expr.elements.len >= 4 {
						// Extract tuple elements: {name, restart, shutdown, type}
						name := p.extract_atom_from_expr(tuple_expr.elements[0]) or {
							p.add_error('Failed to extract name from tuple', 'Got ${tuple_expr.elements[0].str()}')
							continue
						}
						restart := p.extract_atom_from_expr(tuple_expr.elements[1]) or {
							p.add_error('Failed to extract restart from tuple', 'Got ${tuple_expr.elements[1].str()}')
							continue
						}
						shutdown := p.extract_shutdown_from_expr(tuple_expr.elements[2]) or {
							p.add_error('Failed to extract shutdown from tuple', 'Got ${tuple_expr.elements[2].str()}')
							continue
						}
						type_ := p.extract_atom_from_expr(tuple_expr.elements[3]) or {
							p.add_error('Failed to extract type from tuple', 'Got ${tuple_expr.elements[3].str()}')
							continue
						}

						children << ast.ChildTuple{
							name:     name
							restart:  restart
							shutdown: shutdown
							type_:    type_
							position: tuple_expr.position
						}
					} else {
						p.add_error('Child tuple must have at least 4 elements: {name, restart, shutdown, type}',
							'Got ${tuple_expr.elements.len} elements')
					}
				} else {
					p.add_error('Mixed tuple and non-tuple elements in children list',
						'Got ${element.str()}')
				}
			}
			return ast.TupleChildren{
				children: children
				position: expr.position
			}
		} else {
			// Simple list of identifiers
			mut children := []string{}
			for element in expr.elements {
				if element is ast.VariableExpr {
					children << element.name
				} else {
					p.add_error('Children list must contain identifiers', 'Got ${element.str()}')
				}
			}
			return ast.ListChildren{
				children: children
				position: expr.position
			}
		}
	} else {
		// Empty list
		return ast.ListChildren{
			children: []
			position: expr.position
		}
	}
}

// extract_atom_from_expr extracts atom value from expression
fn (mut p LXParser) extract_atom_from_expr(expr ast.Expr) ?string {
	if expr is ast.LiteralExpr {
		literal_expr := expr as ast.LiteralExpr
		if literal_expr.value is ast.AtomLiteral {
			atom_literal := literal_expr.value as ast.AtomLiteral
			return atom_literal.value
		}
	}
	p.add_error('Expected atom', 'Got ${expr.str()}')
	return none
}

// extract_shutdown_from_expr extracts shutdown value from expression (integer or infinity)
fn (mut p LXParser) extract_shutdown_from_expr(expr ast.Expr) ?string {
	if expr is ast.LiteralExpr {
		literal_expr := expr as ast.LiteralExpr
		match literal_expr.value {
			ast.IntegerLiteral {
				return literal_expr.value.value.str()
			}
			ast.AtomLiteral {
				if literal_expr.value.value == 'infinity' {
					return 'infinity'
				}
			}
			else {}
		}
	}
	p.add_error('Expected integer or :infinity for shutdown', 'Got ${expr.str()}')
	return none
}

// parse_string_field parses a string literal for application fields
