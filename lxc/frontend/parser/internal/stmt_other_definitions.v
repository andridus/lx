module internal

import ast
import lexer

// parse_record_definition parses record definitions
fn (mut sp StatementParser) parse_record_definition() ?ast.Stmt {
	sp.advance() // consume 'record'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected record name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	mut fields := []ast.RecordFieldDef{}
	if !sp.check(lexer.punctuation(.rbrace)) {
		for {
			field_name := sp.current.get_value()
			if !sp.current.is_identifier() {
				sp.add_error('Expected field name', 'Got ${sp.current.str()}')
				return none
			}
			sp.advance()

			sp.consume(lexer.operator(.type_cons), 'Expected :: after field name')?

			field_type := sp.parse_type()?

			fields << ast.RecordFieldDef{
				name:       field_name
				field_type: field_type
				position:   sp.get_current_position()
			}

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.RecordDefStmt{
		name:     name
		fields:   fields
		position: sp.get_current_position()
	}
}

// parse_type_alias_statement_with_modifier parses type alias definitions with optional modifiers
fn (mut sp StatementParser) parse_type_alias_statement_with_modifier() ?ast.Stmt {
	sp.advance() // consume 'type'

	// Check for optional modifier
	mut alias_type := ast.TypeAliasType.regular
	if sp.check(lexer.keyword(.opaque)) {
		alias_type = ast.TypeAliasType.opaque
		sp.advance()
	} else if sp.check(lexer.keyword(.nominal)) {
		alias_type = ast.TypeAliasType.nominal
		sp.advance()
	}

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected type alias name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.operator(.type_cons), 'Expected :: after type alias name')?

	type_expr := sp.parse_type_expression()?

	return ast.TypeAliasStmt{
		name:       name
		type_expr:  type_expr
		alias_type: alias_type
		position:   sp.get_current_position()
	}
}

// parse_type_alias_statement parses type alias definitions (type name :: type_expression)
fn (mut sp StatementParser) parse_type_alias_statement() ?ast.Stmt {
	sp.advance() // consume 'type'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected type alias name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.operator(.type_cons), 'Expected :: after type alias name')?

	type_expr := sp.parse_type_expression()?

	return ast.TypeAliasStmt{
		name:       name
		type_expr:  type_expr
		alias_type: ast.TypeAliasType.regular
		position:   sp.get_current_position()
	}
}

// parse_worker_statement parses worker definitions
fn (mut sp StatementParser) parse_worker_statement() ?ast.Stmt {
	sp.advance() // consume 'worker'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected worker name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.keyword(.do_), 'Expected do after worker name')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.keyword(.end_)) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after worker body')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific WorkerStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_supervisor_statement parses supervisor definitions
fn (mut sp StatementParser) parse_supervisor_statement() ?ast.Stmt {
	sp.advance() // consume 'supervisor'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected supervisor name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	// Parse strategy
	sp.consume(lexer.keyword(.strategy), 'Expected strategy keyword')?
	// strategy := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected strategy name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	// Parse children
	sp.consume(lexer.keyword(.children), 'Expected children keyword')?
	sp.consume(lexer.punctuation(.lbracket), 'Expected opening bracket')?

	mut children := []string{}
	if !sp.check(lexer.punctuation(.rbracket)) {
		for {
			child := sp.current.get_value()
			if !sp.current.is_identifier() {
				sp.add_error('Expected child name', 'Got ${sp.current.str()}')
				return none
			}
			sp.advance()
			children << child

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	sp.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?
	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific SupervisorStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: []
		position:   sp.get_current_position()
	}
}

// parse_specification_statement parses specification statements
fn (mut sp StatementParser) parse_specification_statement() ?ast.Stmt {
	sp.advance() // consume 'spec'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected specification name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.punctuation(.rbrace)) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific SpecStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_test_describe_statement parses test describe blocks
fn (mut sp StatementParser) parse_test_describe_statement() ?ast.Stmt {
	sp.advance() // consume 'describe'

	description := sp.current.get_value()
	if sp.current is lexer.StringToken {
		sp.advance()
	} else {
		sp.add_error('Expected string description', 'Got ${sp.current.str()}')
		return none
	}

	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.punctuation(.rbrace)) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific TestDescribeStmt type
	return ast.ModuleStmt{
		name:       description
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_test_statement parses test statements
fn (mut sp StatementParser) parse_test_statement() ?ast.Stmt {
	sp.advance() // consume 'test'

	description := sp.current.get_value()
	if sp.current is lexer.StringToken {
		sp.advance()
	} else {
		sp.add_error('Expected string description', 'Got ${sp.current.str()}')
		return none
	}

	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.punctuation(.rbrace)) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific TestStmt type
	return ast.ModuleStmt{
		name:       description
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}
