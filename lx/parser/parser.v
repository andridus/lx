module parser

import lexer
import ast
import errors
import kernel

pub struct Parser {
	directives_table &DirectivesTable
mut:
	lexer          lexer.Lexer
	current        lexer.Token
	next           lexer.Token
	error_reporter errors.ErrorReporter
	next_ast_id    int  = 1
	at_line_start  bool = true // Track if we're at the start of a new line
	temp_doc_node  ?ast.Node
}

pub fn new_parser(code string, file_path string, directives_table &DirectivesTable) Parser {
	mut l := lexer.new_lexer(code, file_path)
	mut p := Parser{
		lexer:            l
		error_reporter:   errors.new_error_reporter()
		directives_table: directives_table
	}
	p.current = p.lexer.next_token()
	p.next = p.lexer.next_token()
	return p
}

pub fn (mut p Parser) parse() !ast.Node {
	return p.parse_module()
}

pub fn (p Parser) get_errors() []errors.Err {
	return p.error_reporter.all()
}

pub fn (mut p Parser) get_next_id() int {
	id := p.next_ast_id
	p.next_ast_id++
	return id
}

fn (mut p Parser) advance() {
	// Track if current token is newline to set line start flag
	if p.current.type_ == .newline {
		p.at_line_start = true
	} else if p.current.type_ != .newline {
		// Only reset at_line_start when we encounter a non-newline token
		p.at_line_start = false
	}

	p.current = p.next
	p.next = p.lexer.next_token()
}

fn (mut p Parser) error(msg string) {
	p.error_reporter.report(.parser, msg, p.current.position)
}

fn (mut p Parser) error_and_return(msg string) !ast.Node {
	p.error_reporter.report(.parser, msg, p.current.position)
	return error(msg)
}

fn (mut p Parser) error_and_return_with_suggestion(msg string, suggestion string) !ast.Node {
	p.error_reporter.report_with_suggestion(.parser, msg, p.current.position, suggestion)
	return error(msg)
}

fn (mut p Parser) parse_module() !ast.Node {
	mut functions := []ast.Node{}
	mut records := []ast.Node{}
	mut all_nodes := []ast.Node{}
	start_pos := p.current.position
	module_id := p.get_next_id()
	for p.current.type_ != .eof {
		if p.current.type_ == .newline {
			p.advance()
			continue
		}

		if p.current.type_ == .error {
			p.error('Lexical error: ${p.current.value}')
			return error('Lexical error')
		}

		if p.current.type_ == .record {
			record := p.parse_record_definition()!
			records << record
		} else if p.current.type_ == .def {
			func := p.parse_function()!
			functions << func
		} else if p.current.type_ == .type {
			type_def := p.parse_type_def()!
			all_nodes << type_def
		} else if p.current.type_ == .deps {
			deps := p.parse_deps_declaration()!
			all_nodes << deps
		} else if p.current.type_ == .application {
			app := p.parse_application_config()!
			all_nodes << app
		} else if p.current.type_ == .import {
			imp := p.parse_import_statement()!
			all_nodes << imp
		} else if p.current.type_ == .supervisor {
			sup := p.parse_supervisor_definition()!
			functions << sup
		} else if p.current.type_ == .worker {
			wrk := p.parse_worker_definition()!
			functions << wrk
		} else if p.current.type_ == .describe {
			describe_blocks := p.parse_describe_block()!
			for test_func in describe_blocks {
				functions << test_func
			}
		} else if p.current.type_ == .test {
			test_block := p.parse_test_block()!
			functions << test_block
		} else {
			p.error('Expected top-level construct, got "${p.current.value}"')
			return error('Expected top-level construct')
		}
	}

	// Combine records and functions

	all_nodes << records
	all_nodes << functions

	return ast.new_module(module_id, 'main', all_nodes, start_pos)
}

pub fn (mut p Parser) parse_with_modname(modname string) !ast.Node {
	return p.parse_module_with_name(modname)
}

fn (mut p Parser) parse_module_with_name(modname string) !ast.Node {
	mut functions := []ast.Node{}
	mut records := []ast.Node{}
	mut all_nodes := []ast.Node{}
	start_pos := p.current.position
	module_id := p.get_next_id()

	for p.current.type_ != .eof {
		if p.current.type_ == .newline {
			p.advance()
			continue
		}

		if p.current.type_ == .error {
			p.error('Lexical error: ${p.current.value}')
			return error('Lexical error')
		}

		match p.current.type_ {
			.at_sign {
				directive := p.parse_directive_new()!
				all_nodes << directive
			}
			.record {
				record := p.parse_record_definition()!
				records << record
			}
			.def {
				func := p.parse_function()!
				functions << func
			}
			.type {
				type_def := p.parse_type_def()!
				all_nodes << type_def
			}
			.deps {
				deps := p.parse_deps_declaration()!
				all_nodes << deps
			}
			.application {
				app := p.parse_application_config()!
				all_nodes << app
			}
			.import {
				imp := p.parse_import_statement()!
				all_nodes << imp
			}
			.supervisor {
				sup := p.parse_supervisor_definition()!
				functions << sup
			}
			.worker {
				wrk := p.parse_worker_definition()!
				functions << wrk
			}
			.describe {
				describe_blocks := p.parse_describe_block()!
				for test_func in describe_blocks {
					functions << test_func
				}
			}
			.test {
				test_block := p.parse_test_block()!
				functions << test_block
			}
			else {
				p.error('Expected top-level construct, got "${p.current.value}"')
				return error('Expected top-level construct')
			}
		}
	}

	// Combine records and functions
	all_nodes << records
	all_nodes << functions

	return ast.new_module(module_id, modname, all_nodes, start_pos)
}

fn (mut p Parser) parse_function() !ast.Node {
	if p.current.type_ != .def {
		p.error('Expected "def", got "${p.current.value}"')
		return error('Expected def')
	}
	start_pos := p.current.position
	func_id := p.get_next_id()
	p.advance()

	if p.current.type_ != .identifier {
		p.error('Expected function name, got "${p.current.value}"')
		return error('Expected function name')
	}
	func_name := p.current.value
	p.advance()

	mut has_parens := false
	mut args := []ast.Node{}
	if p.current.type_ == .lparen {
		has_parens = true
		p.advance() // Skip '('
		if p.current.type_ != .rparen {
			for {
				arg := p.parse_arg()!
				args << arg
				if p.current.type_ == .rparen {
					break
				}
				if p.current.type_ != .comma {
					return p.error_and_return('Expected comma or closing parenthesis')
				}
				p.advance() // Skip comma
			}
		}
		if p.current.type_ != .rparen {
			return p.error_and_return('Expected closing parenthesis')
		}
		p.advance() // Skip ')'
	}

	// Skip return type annotation for now
	if p.current.type_ == .double_colon {
		p.advance() // Skip ::
		p.parse_type_annotation()! // Parse but ignore for now
	}

	if p.current.type_ != .do {
		return p.error_and_return_with_suggestion('Function definition requires "do" keyword',
			'Add "do" after parentheses: ${func_name}() do')
	}
	p.advance()

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	mut body := ast.Node{}
	if has_parens {
		// Nunca é multi-head
		body = p.parse_block()!
	} else {
		// Sempre multi-head
		mut heads := []ast.Node{}
		for p.current.type_ != .end && p.current.type_ != .eof {
			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .lparen {
				head := p.parse_function_head()!
				heads << head
			} else {
				break
			}
		}
		if heads.len == 0 {
			return p.error_and_return('Expected at least one function head (pattern) in multi-head function')
		}
		body = ast.new_block(p.get_next_id(), heads, heads[0].position)
	}

	if p.current.type_ != .end {
		return p.error_and_return('Expected end keyword')
	}
	p.advance()

	if node := p.temp_doc_node {
		p.directives_table.add_doc('${func_name}/${args.len}', node)
		p.temp_doc_node = none // Clear after use to avoid applying to next function
	}

	return ast.new_function_with_params(func_id, func_name, args, body, start_pos)
}

fn (mut p Parser) parse_arg() !ast.Node {
	pos := p.current.position

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected argument name')
	}
	arg_name := p.current.value
	p.advance()

	// Parse type annotation
	if p.current.type_ == .double_colon {
		p.advance() // Skip ::
		type_annotation := p.parse_type_annotation()!

		// Create parameter with type annotation
		return ast.Node{
			id:       p.get_next_id()
			kind:     .function_parameter
			value:    arg_name
			children: [type_annotation]
			position: pos
		}
	}

	return ast.new_function_parameter(p.get_next_id(), arg_name, pos)
}

fn (mut p Parser) parse_type_annotation() !ast.Node {
	pos := p.current.position

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected type name')
	}
	type_name := p.current.value
	p.advance()

	return ast.Node{
		id:       p.get_next_id()
		kind:     .identifier
		value:    type_name
		children: []
		position: pos
	}
}

fn (mut p Parser) parse_function_body() !ast.Node {
	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	// Check if this is a block expression with braces
	if p.current.type_ == .lbrace {
		return p.parse_block_expression()!
	}

	// Check if this is a function head pattern: (pattern) -> body
	if p.current.type_ == .lparen {
		return p.parse_function_head()!
	}

	mut expressions := []ast.Node{}

	// Consome múltiplas expressões até encontrar 'end' ou 'eof'
	for p.current.type_ != .end && p.current.type_ != .eof {
		// Skip newlines entre expressões
		for p.current.type_ == .newline {
			p.advance()
		}
		if p.current.type_ == .end || p.current.type_ == .eof {
			break
		}
		expr := p.parse_expression()!
		expressions << expr
		// Skip newlines após expressão
		for p.current.type_ == .newline {
			p.advance()
		}
		// Skip semicolon if present
		if p.current.type_ == .semicolon {
			p.advance() // Skip ';'
		}
	}

	// Se há apenas uma expressão, retorna ela diretamente
	if expressions.len == 1 {
		return expressions[0]
	}

	// Se há múltiplas expressões, cria um block
	return ast.new_block(p.get_next_id(), expressions, if expressions.len > 0 {
		expressions[0].position
	} else {
		p.current.position
	})
}

fn (mut p Parser) parse_block_expression() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'

	mut expressions := []ast.Node{}

	for {
		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		if p.current.type_ == .rbrace {
			break
		}

		expression := p.parse_expression()!
		expressions << expression

		// Skip newlines after expression
		for p.current.type_ == .newline {
			p.advance()
		}

		if p.current.type_ == .semicolon {
			p.advance() // Skip ';'
		} else if p.current.type_ != .rbrace {
			return p.error_and_return('Expected semicolon or closing brace')
		}
	}

	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing brace')
	}
	p.advance() // Skip '}'

	return ast.new_block(p.get_next_id(), expressions, pos)
}

fn (mut p Parser) parse_function_head() !ast.Node {
	pos := p.current.position

	if p.current.type_ != .lparen {
		return p.error_and_return('Expected opening parenthesis for function head')
	}
	p.advance() // Skip '('

	// Parse args
	args := p.parse_args()!

	if p.current.type_ != .rparen {
		return p.error_and_return('Expected closing parenthesis')
	}
	p.advance() // Skip ')'

	// Parse return type annotation (not used in simplified version)
	if p.current.type_ == .double_colon {
		p.advance() // Skip ::
		p.parse_type_annotation()! // Parse but don't store
	}

	if p.current.type_ != .arrow {
		return p.error_and_return('Expected arrow (->)')
	}
	p.advance() // Skip '->'

	// Parse body - check if there's a newline for multi-expression
	mut body := ast.Node{}
	if p.current.type_ == .newline {
		// Multi-expression body
		for p.current.type_ == .newline {
			p.advance()
		}

		// Parse as block until we find next pattern or end
		mut expressions := []ast.Node{}
		for p.current.type_ != .lparen && p.current.type_ != .end && p.current.type_ != .eof {
			// Skip newlines between expressions
			for p.current.type_ == .newline {
				p.advance()
			}

			if p.current.type_ == .lparen || p.current.type_ == .end || p.current.type_ == .eof {
				break
			}

			expr := p.parse_expression()!
			expressions << expr
		}

		if expressions.len == 0 {
			return p.error_and_return('Expected at least one expression in function body')
		}

		// Create block node
		body = ast.new_block(p.get_next_id(), expressions, pos)
	} else {
		// Single expression body
		body = p.parse_expression()!
	}

	// Create function with args as children[0] and body as children[1]
	mut children := []ast.Node{}
	children << args // args block
	children << body // function body

	return ast.Node{
		id:       p.get_next_id()
		kind:     .function
		value:    '' // anonymous function
		children: children
		position: pos
	}
}

fn (mut p Parser) parse_args() !ast.Node {
	// Handle empty args ()
	if p.current.type_ == .rparen {
		return ast.Node{
			id:       p.get_next_id()
			kind:     .block
			value:    ''
			children: []
			position: p.current.position
		}
	}

	// Handle mixed args (literals, identifiers with type annotations, etc.)
	mut args := []ast.Node{}
	mut first_pos := p.current.position

	for {
		// Parse current argument
		mut arg := ast.Node{}

		if p.current.type_ == .identifier {
			// Identifier with optional type annotation
			arg_name := p.current.value
			arg_pos := p.current.position
			p.advance()

			// Parse type annotation if present
			mut type_annotation := ast.Node{}
			if p.current.type_ == .double_colon {
				p.advance() // Skip ::
				type_annotation = p.parse_type_annotation()!
			}

			arg = ast.Node{
				id:       p.get_next_id()
				kind:     .identifier
				value:    arg_name
				children: if type_annotation.value != '' { [type_annotation] } else { [] }
				position: arg_pos
			}
		} else if p.current.type_ == .integer || p.current.type_ == .float
			|| p.current.type_ == .string || p.current.type_ == .atom || p.current.type_ == .true_
			|| p.current.type_ == .false_ || p.current.type_ == .nil_ {
			// Literal
			arg = p.parse_literal()!
		} else if p.current.type_ == .lbracket {
			// List literal or list cons
			arg = p.parse_list_expression()!
		} else if p.current.type_ == .lbrace {
			// Tuple literal
			arg = p.parse_tuple_expression()!
		} else if p.current.type_ == .percent {
			// Map literal
			arg = p.parse_map_literal()!
		} else {
			// Try to parse as any other expression
			arg = p.parse_expression()!
		}

		args << arg

		// Check if there are more arguments
		if p.current.type_ == .comma {
			p.advance() // Skip comma
		} else {
			break
		}
	}

	// Return args block
	return ast.Node{
		id:       p.get_next_id()
		kind:     .block
		value:    ''
		children: args
		position: first_pos
	}
}

fn (mut p Parser) parse_literal() !ast.Node {
	pos := p.current.position
	lit_id := p.get_next_id()

	return match p.current.type_ {
		.integer {
			value := p.current.value.int()
			p.advance()
			ast.new_integer(lit_id, value, pos)
		}
		.float {
			value := p.current.value.f64()
			p.advance()
			ast.new_float(lit_id, value, pos)
		}
		.string {
			value := p.current.value
			p.advance()
			ast.new_string(lit_id, value, pos)
		}
		.true_ {
			p.advance()
			ast.new_boolean(lit_id, true, pos)
		}
		.false_ {
			p.advance()
			ast.new_boolean(lit_id, false, pos)
		}
		.atom {
			value := p.current.value
			p.advance()
			ast.new_atom(lit_id, value, pos)
		}
		.nil_ {
			p.advance()
			ast.new_nil(lit_id, pos)
		}
		else {
			p.error('Expected literal, got "${p.current.value}"')
			return error('Expected literal')
		}
	}
}

fn (mut p Parser) parse_binding() !ast.Node {
	if p.current.type_ != .identifier {
		p.error('Expected variable name')
		return error('Expected variable name')
	}

	var_name := p.current.value
	pos := p.current.position
	p.advance()

	if p.current.type_ != .bind {
		p.error('Expected =')
		return error('Expected =')
	}
	p.advance()

	value := p.parse_expression()!

	return ast.new_variable_binding(p.get_next_id(), var_name, value, pos)
}

fn (mut p Parser) parse_variable_ref() !ast.Node {
	if p.current.type_ != .identifier {
		p.error('Expected variable name')
		return error('Expected variable name')
	}

	var_name := p.current.value
	pos := p.current.position

	p.advance()

	return ast.new_variable_ref(p.get_next_id(), var_name, pos)
}

fn (mut p Parser) parse_block() !ast.Node {
	mut expressions := []ast.Node{}
	start_pos := p.current.position

	// Skip initial newlines after 'do' or '->'
	for p.current.type_ == .newline {
		p.advance()
	}

	for {
		// Stop if we encounter 'end', 'else' or other non-expression tokens
		if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
			break
		}

		// Skip newlines before expression
		for p.current.type_ == .newline {
			p.advance()
		}

		// Stop if we encounter 'end', 'else' after skipping newlines
		if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
			break
		}

		// Special handling for match expressions - they consume all remaining expressions
		if p.current.type_ == .match {
			match_expr := p.parse_match_with_continuation()!
			expressions << match_expr
			break // Match consumes all remaining expressions
		}

		expr := p.parse_expression()!
		expressions << expr

		// Check for semicolon or newline separator
		if p.current.type_ == .semicolon {
			p.advance()
			// Skip newlines after semicolon
			for p.current.type_ == .newline {
				p.advance()
			}
		} else if p.current.type_ == .newline {
			p.advance()
			// Skip multiple newlines
			for p.current.type_ == .newline {
				p.advance()
			}
		} else {
			// If we reach here, we have a complete expression
			// Continue to next expression if there are more tokens
			if p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
				continue
			}
			break
		}
	}
	return ast.new_block(p.get_next_id(), expressions, start_pos)
}

fn (mut p Parser) parse_expression() !ast.Node {
	return p.parse_expression_with_precedence(0)
}

fn (mut p Parser) parse_expression_with_precedence(precedence int) !ast.Node {
	mut left := p.parse_prefix_expression()!

	for {
		// Check for infix operators
		if (p.current.type_ == .identifier && p.is_infix_function(p.current.value))
			|| p.current.type_ == .exclamation || p.current.type_ == .slash
			|| p.current.type_ == .in {
			// Normalize operator token to a name understood by kernel
			op_name := if p.current.type_ == .exclamation {
				'!'
			} else if p.current.type_ == .slash {
				'/'
			} else if p.current.type_ == .in {
				'in'
			} else {
				p.current.value
			}
			function_info := kernel.get_function_info(op_name) or { break }
			if function_info.precedence < precedence {
				break
			}
			left = p.parse_infix_expression_with_name(left, op_name)!
			continue
		}

		// Map access: expr[key]
		if p.current.type_ == .lbracket {
			left = p.parse_map_access(left)!
			continue
		}

		// Record access: expr.field
		if p.current.type_ == .dot {
			left = p.parse_record_access(left)!
			continue
		}

		break
	}

	return left
}

fn (mut p Parser) parse_infix_expression_with_name(left ast.Node, name string) !ast.Node {
	pos := p.current.position
	// advance current token (already captured name)
	p.advance()

	function_info := kernel.get_function_info(name) or { return error('Unknown function: ${name}') }

	right := p.parse_expression_with_precedence(function_info.precedence)!
	return ast.new_function_caller(p.get_next_id(), name, [left, right], pos)
}

fn (mut p Parser) parse_prefix_expression() !ast.Node {
	return match p.current.type_ {
		.integer, .float, .string, .true_, .false_, .atom, .nil_ { p.parse_literal() }
		.identifier { p.parse_identifier_expression() }
		.lparen { p.parse_parentheses() }
		.lbracket { p.parse_list_expression() }
		.lbrace { p.parse_tuple_expression() }
		.percent { p.parse_map_literal() }
		.case { p.parse_case_expression() }
		.fn { p.parse_lambda_expression() }
		// Task 11: Control Flow
		.if_ { p.parse_if_expression() }
		.with { p.parse_with_expression() }
		.match { p.parse_match_expression() }
		// Task 11: Concurrency
		.spawn { p.parse_spawn_expression() }
		.receive { p.parse_receive_expression() }
		// Task 11: Binaries
		.double_lt { p.parse_binary_literal() }
		// Task 11: Advanced Features
		.at_sign { p.parse_directive_new() }
		.for_ { p.parse_list_comprehension() }
		.assert { p.parse_assert_expression() }
		else { error('Unexpected token: ${p.current.type_}') }
	}
}

fn (mut p Parser) parse_identifier_expression() !ast.Node {
	identifier := p.current.value
	pos := p.current.position

	p.advance()
	if identifier.starts_with('$') {
		return p.parse_directive_call(identifier, pos)
	}

	// Verifica se é uma chamada de função (com parênteses)
	if p.current.type_ == .lparen {
		return p.parse_function_call(identifier, pos)
	}

	// Check if this is a record literal
	if p.current.type_ == .lbrace {
		// This is a record literal, not a variable reference
		record_node := ast.Node{
			id:       p.get_next_id()
			kind:     .identifier
			value:    identifier
			children: []
			position: pos
		}
		return p.parse_record_literal_with_name(record_node)
	}

	// Check for record access
	if p.current.type_ == .dot {
		record_node := ast.Node{
			id:       p.get_next_id()
			kind:     .identifier
			value:    identifier
			children: []
			position: pos
		}
		return p.parse_record_access(record_node)
	}

	// used to not required parameters
	if p.is_single_arg_prefix_function(identifier) {
		arg := p.parse_expression()!
		return ast.new_function_caller(p.get_next_id(), identifier, [arg], pos)
	}

	// Verifica se é um binding (identificador seguido de =)
	if p.current.type_ == .bind {
		p.advance() // Skip '='
		value := p.parse_expression()!
		return ast.new_variable_binding(p.get_next_id(), identifier, value, pos)
	}

	// Check for type annotation
	if p.current.type_ == .double_colon {
		p.advance() // Skip ::
		type_annotation := p.parse_type_annotation()!

		// Create identifier with type annotation
		return ast.Node{
			id:       p.get_next_id()
			kind:     .identifier
			value:    identifier
			children: [type_annotation]
			position: pos
		}
	}

	// Apenas referência de variável
	return ast.new_variable_ref(p.get_next_id(), identifier, pos)
}

fn (mut p Parser) parse_function_call(function_name string, pos ast.Position) !ast.Node {
	p.advance() // Skip '('

	mut arguments := []ast.Node{}

	if p.current.type_ != .rparen {
		for {
			arg := p.parse_expression()!
			arguments << arg

			if p.current.type_ == .rparen {
				break
			}

			if p.current.type_ != .comma {
				return error('Expected comma or closing parenthesis')
			}

			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rparen {
		return error('Expected closing parenthesis')
	}

	p.advance() // Skip ')'

	return ast.new_function_caller(p.get_next_id(), function_name, arguments, pos)
}

fn (mut p Parser) parse_directive_call(directive_name string, pos ast.Position) !ast.Node {
	actual_name := directive_name[1..]

	if !p.is_valid_directive(actual_name) {
		p.error('Unknown directive: ${directive_name}')
		return error('Unknown directive: ${directive_name}')
	}

	if p.current.type_ != .lparen {
		p.error('Directive ${directive_name} requires parentheses')
		return error('Directive ${directive_name} requires parentheses')
	}

	p.advance() // Skip '('

	mut arguments := []ast.Node{}

	if p.current.type_ != .rparen {
		for {
			arg := p.parse_expression()!
			arguments << arg

			if p.current.type_ == .rparen {
				break
			}

			if p.current.type_ != .comma {
				return error('Expected comma or closing parenthesis')
			}

			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rparen {
		return error('Expected closing parenthesis')
	}

	p.advance() // Skip ')'

	return ast.new_directive_call(p.get_next_id(), actual_name, arguments, pos)
}

fn (p Parser) is_valid_directive(name string) bool {
	return name in ['print', 'type']
}

fn (mut p Parser) parse_function_call_no_parens(function_name string, pos ast.Position) !ast.Node {
	mut arguments := []ast.Node{}

	// Parse argumentos até encontrar um token que não seja argumento
	for {
		arg := p.parse_expression()!
		arguments << arg

		// Para se encontrar um token que não seja parte de uma expressão
		if p.current.type_ == .eof || p.current.type_ == .semicolon || p.current.type_ == .comma
			|| p.current.type_ == .rparen || p.current.type_ == .newline {
			break
		}
	}

	return ast.new_function_caller(p.get_next_id(), function_name, arguments, pos)
}

fn (mut p Parser) parse_infix_expression(left ast.Node) !ast.Node {
	function_name := p.current.value // Nome da função (ex: "+", "*", ">")
	pos := p.current.position

	// Obtém informações da função nativa
	function_info := kernel.get_function_info(function_name) or {
		return error('Unknown function: ${function_name}')
	}

	p.advance()

	right := p.parse_expression_with_precedence(function_info.precedence)!

	// Cria um nó de chamada de função com os dois argumentos
	return ast.new_function_caller(p.get_next_id(), function_name, [left, right], pos)
}

fn (mut p Parser) parse_parentheses() !ast.Node {
	p.advance() // Skip '('

	expr := p.parse_expression()! // 0 = precedência mais baixa

	if p.current.type_ != .rparen {
		return error('Expected closing parenthesis')
	}
	p.advance() // Skip ')'

	return ast.new_parentheses(p.get_next_id(), expr, p.current.position)
}

fn (p Parser) is_operator(identifier string) bool {
	return identifier in ['+', '-', '*', '/', '==', '!=', '<', '<=', '>', '>=', '&&&', '|||', '^^^',
		'<<<', '>>>', 'and', 'or']
}

fn (p Parser) is_infix_function(name string) bool {
	function_info := kernel.get_function_info(name) or { return false }
	return function_info.fixity == .infix
}

fn (p Parser) is_single_arg_prefix_function(identifier string) bool {
	single_arg_prefix_functions := ['not']
	return identifier in single_arg_prefix_functions
}

fn (mut p Parser) parse_list_expression() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['

	// Skip newlines after opening bracket
	for p.current.type_ == .newline {
		p.advance()
	}

	// Check for empty list
	if p.current.type_ == .rbracket {
		p.advance() // Skip ']'
		return ast.new_list_literal(p.get_next_id(), [], pos)
	}

	// Parse first element
	first_element := p.parse_expression()!

	// Check if it's a cons operation
	if p.current.type_ == .pipe {
		p.advance() // Skip '|'
		tail := p.parse_expression()!

		if p.current.type_ != .rbracket {
			return error('Expected closing bracket')
		}
		p.advance() // Skip ']'

		// Check if this is a pattern binding (cons pattern followed by =)
		if p.current.type_ == .bind {
			p.advance() // Skip '='
			expr := p.parse_expression()!

			// Create the pattern node from the cons
			pattern := ast.new_list_cons(p.get_next_id(), first_element, tail, pos)
			return ast.new_pattern_binding(p.get_next_id(), pattern, expr, pos)
		}

		return ast.new_list_cons(p.get_next_id(), first_element, tail, pos)
	}

	// It's a regular list literal
	mut elements := [first_element]

	// Parse remaining elements
	for p.current.type_ == .comma {
		p.advance() // Skip comma

		// Skip newlines after comma
		for p.current.type_ == .newline {
			p.advance()
		}

		element := p.parse_expression()!
		elements << element
	}

	// Skip newlines before closing bracket
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbracket {
		return error('Expected closing bracket')
	}
	p.advance() // Skip ']'

	// Check if this is a pattern binding (list pattern followed by =)
	if p.current.type_ == .bind {
		p.advance() // Skip '='
		expr := p.parse_expression()!

		// Create the pattern node from the list
		pattern := ast.new_list_literal(p.get_next_id(), elements, pos)
		return ast.new_pattern_binding(p.get_next_id(), pattern, expr, pos)
	}

	return ast.new_list_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_tuple_expression() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'

	// Skip newlines after opening brace
	for p.current.type_ == .newline {
		p.advance()
	}

	// Check for empty tuple
	if p.current.type_ == .rbrace {
		p.advance() // Skip '}'
		return ast.new_tuple_literal(p.get_next_id(), [], pos)
	}

	// Parse first element
	first_element := p.parse_expression()!

	// It's a regular tuple literal
	mut elements := [first_element]

	// Parse remaining elements
	for p.current.type_ == .comma {
		p.advance() // Skip comma

		// Skip newlines after comma
		for p.current.type_ == .newline {
			p.advance()
		}

		element := p.parse_expression()!
		elements << element
	}

	// Skip newlines before closing brace
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}
	p.advance() // Skip '}'

	// Check if this is a pattern binding (tuple pattern followed by =)
	if p.current.type_ == .bind {
		p.advance() // Skip '='
		expr := p.parse_expression()!

		// Create the pattern node from the tuple
		pattern := ast.new_tuple_literal(p.get_next_id(), elements, pos)
		return ast.new_pattern_binding(p.get_next_id(), pattern, expr, pos)
	}

	return ast.new_tuple_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_map_literal() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '%'

	// Check if this is a record update (has record name followed by {)
	if p.current.type_ == .identifier {
		// Look ahead to see if next token is {
		if p.next.type_ == .lbrace {
			return p.parse_record_update_with_name()
		}
	}

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after %')
	}
	p.advance() // Skip '{'

	// Check if this is a record update (has |)
	if p.current.type_ == .pipe {
		return p.parse_record_update()
	}

	// Skip newlines after opening brace
	for p.current.type_ == .newline {
		p.advance()
	}

	// Check for empty map
	if p.current.type_ == .rbrace {
		p.advance() // Skip '}'
		return ast.new_map_literal(p.get_next_id(), [], pos)
	}

	mut entries := []ast.Node{}

	// Parse key-value pairs
	for {
		// Parse key (can be any term LX)
		key := p.parse_map_key()!

		if p.current.type_ != .colon {
			return error('Expected colon after map key')
		}
		p.advance() // Skip ':'

		// Parse value
		value := p.parse_expression()!

		entries << key
		entries << value

		if p.current.type_ == .rbrace {
			break
		}

		if p.current.type_ != .comma {
			return error('Expected comma or closing brace')
		}

		p.advance() // Skip comma

		// Skip newlines after comma
		for p.current.type_ == .newline {
			p.advance()
		}
	}

	// Skip newlines before closing brace
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}

	p.advance() // Skip '}'

	return ast.new_map_literal(p.get_next_id(), entries, pos)
}

fn (mut p Parser) parse_map_key() !ast.Node {
	// In map context, identifiers without : are treated as atoms
	if p.current.type_ == .identifier {
		// Check if next token is : (meaning this is an atom)
		if p.next.type_ == .colon {
			atom_name := p.current.value
			pos := p.current.position
			p.advance() // Skip identifier
			return ast.new_atom(p.get_next_id(), atom_name, pos)
		}
	}

	// Otherwise, parse as normal expression
	return p.parse_expression()
}

fn (mut p Parser) parse_map_access(map_expr ast.Node) !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['

	key_expr := p.parse_expression()!

	if p.current.type_ != .rbracket {
		return error('Expected closing bracket for map access')
	}
	p.advance() // Skip ']'

	return ast.new_map_access(p.get_next_id(), map_expr, key_expr, pos)
}

// Record parsing functions
fn (mut p Parser) parse_record_definition() !ast.Node {
	pos := p.current.position
	p.advance() // Skip 'record'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected record name after record keyword')
	}
	record_name := p.current.value
	p.advance() // Skip record name

	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected opening brace after record name')
	}
	p.advance() // Skip '{'

	// Skip whitespace and newlines after opening brace
	for p.current.type_ == .newline {
		p.advance()
	}

	mut fields := []ast.Node{}

	if p.current.type_ != .rbrace {
		for {
			if p.current.type_ != .identifier {
				return p.error_and_return('Expected field name')
			}
			field_name := p.current.value
			p.advance() // Skip field name

			// Check if field has default value first
			mut default_value := ast.Node{}
			mut has_default := false
			mut field_type := ''

			if p.current.type_ == .bind {
				p.advance() // Skip '='
				default_value = p.parse_expression()!
				has_default = true

				// Type is optional when there's a default value
				if p.current.type_ == .double_colon {
					p.advance() // Skip ::

					if p.current.type_ != .identifier {
						return p.error_and_return('Expected field type after ::')
					}
					field_type = p.current.value
					p.advance() // Skip field type
				} else {
					// No explicit type, will be inferred from default value
					field_type = ''
				}
			} else {
				// No default value, expect :: and type
				if p.current.type_ != .double_colon {
					return p.error_and_return_with_suggestion('Expected :: after field name',
						'Add type annotation: ${field_name} :: integer or ${field_name} :: string')
				}
				p.advance() // Skip ::

				if p.current.type_ != .identifier {
					return p.error_and_return('Expected field type')
				}
				field_type = p.current.value
				p.advance() // Skip field type
			}

			// Create field type node
			field_type_node := ast.Node{
				id:       p.get_next_id()
				kind:     .identifier
				value:    field_type
				children: []
				position: pos
			}

			// Create field node with proper AST structure
			mut field_node := ast.Node{}
			if has_default {
				field_node = ast.new_record_field(p.get_next_id(), field_name, field_type_node,
					default_value, pos)
			} else {
				field_node = ast.new_record_field_without_default(p.get_next_id(), field_name,
					field_type_node, pos)
			}
			fields << field_node

			// Allow newline-separated or comma-separated fields
			for p.current.type_ == .newline {
				p.advance()
			}

			if p.current.type_ == .rbrace {
				break
			}

			if p.current.type_ == .comma {
				p.advance() // Skip comma and continue
				continue
			}

			// If not comma or closing brace, expect next identifier (next field) or end
			if p.current.type_ != .identifier {
				return p.error_and_return('Expected comma or closing brace')
			}
		}
	}

	// Skip newlines before closing brace
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing brace')
	}
	p.advance() // Skip '}'

	return ast.new_record_definition(p.get_next_id(), record_name, fields, pos)
}

fn (mut p Parser) parse_record_literal() !ast.Node {
	pos := p.current.position

	if p.current.type_ != .identifier {
		return error('Expected record name')
	}
	record_name := p.current.value
	p.advance() // Skip record name

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after record name')
	}
	p.advance() // Skip '{'

	mut field_values := []ast.Node{}

	if p.current.type_ != .rbrace {
		for {
			if p.current.type_ != .identifier {
				return error('Expected field name')
			}
			field_name := p.current.value
			p.advance() // Skip field name

			if p.current.type_ != .colon {
				return error('Expected colon after field name')
			}
			p.advance() // Skip ':'

			// Parse field value
			field_value := p.parse_expression()!

			// Create field value node
			field_node := ast.Node{
				id:       p.get_next_id()
				kind:     .identifier
				value:    field_name
				children: [field_value]
				position: pos
			}
			field_values << field_node

			if p.current.type_ == .rbrace {
				break
			}

			if p.current.type_ != .comma {
				return error('Expected comma or closing brace')
			}

			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}

	p.advance() // Skip '}'

	return ast.new_record_literal(p.get_next_id(), record_name, field_values, pos)
}

fn (mut p Parser) parse_record_literal_with_name(record_node ast.Node) !ast.Node {
	record_name := record_node.value
	pos := record_node.position

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after record name')
	}
	p.advance() // Skip '{'

	mut field_values := []ast.Node{}

	if p.current.type_ != .rbrace {
		for {
			if p.current.type_ != .identifier {
				return error('Expected field name')
			}
			field_name := p.current.value
			p.advance() // Skip field name

			if p.current.type_ != .colon {
				return error('Expected colon after field name')
			}
			p.advance() // Skip ':'

			// Parse field value
			field_value := p.parse_expression()!

			// Create field value node
			field_node := ast.Node{
				id:       p.get_next_id()
				kind:     .identifier
				value:    field_name
				children: [field_value]
				position: pos
			}
			field_values << field_node

			if p.current.type_ == .rbrace {
				break
			}

			if p.current.type_ != .comma {
				return error('Expected comma or closing brace')
			}

			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}

	p.advance() // Skip '}'

	return ast.new_record_literal(p.get_next_id(), record_name, field_values, pos)
}

fn (mut p Parser) parse_record_access(node ast.Node) !ast.Node {
	if p.current.type_ != .dot {
		return error('Expected dot for record access')
	}
	p.advance() // Skip '.'

	// Check if this is a lambda call: fun.(args)
	if p.current.type_ == .lparen {
		return p.parse_lambda_call(node)!
	}

	if p.current.type_ != .identifier {
		return error('Expected field name after dot')
	}
	field_name := p.current.value
	p.advance() // Skip field name

	return ast.new_record_access(p.get_next_id(), node, field_name, node.position)
}

fn (mut p Parser) parse_record_update() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '%'

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after %')
	}
	p.advance() // Skip '{'

	// Parse record expression
	record_expr := p.parse_expression()!

	if p.current.type_ != .pipe {
		return error('Expected | after record expression')
	}
	p.advance() // Skip '|'

	// Parse field name
	if p.current.type_ != .identifier {
		return error('Expected field name after |')
	}
	field_name := p.current.value
	p.advance() // Skip field name

	if p.current.type_ != .colon {
		return error('Expected colon after field name')
	}
	p.advance() // Skip ':'

	// Parse field value
	field_value := p.parse_expression()!

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}
	p.advance() // Skip '}'

	// For the old syntax, we don't have a record name, so we'll use a placeholder
	return ast.new_record_update(p.get_next_id(), 'unknown', record_expr, field_name,
		field_value, pos)
}

fn (mut p Parser) parse_record_update_with_name() !ast.Node {
	pos := p.current.position
	// Don't advance here since we already advanced past % in parse_map_literal

	if p.current.type_ != .identifier {
		return error('Expected record name after %')
	}
	record_name := p.current.value
	p.advance() // Skip record name

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after record name')
	}
	p.advance() // Skip '{'

	// Parse record expression
	record_expr := p.parse_expression()!

	if p.current.type_ != .pipe {
		return error('Expected | after record expression')
	}
	p.advance() // Skip '|'

	// Parse field updates (can be multiple)
	mut field_updates := []ast.Node{}

	for {
		// Parse field name
		if p.current.type_ != .identifier {
			return error('Expected field name after |')
		}
		field_name := p.current.value
		p.advance() // Skip field name

		if p.current.type_ != .colon {
			return error('Expected colon after field name')
		}
		p.advance() // Skip ':'

		// Parse field value
		field_value := p.parse_expression()!

		// Create field update node
		field_node := ast.Node{
			id:       p.get_next_id()
			kind:     .identifier
			value:    field_name
			children: [field_value]
			position: pos
		}
		field_updates << field_node

		if p.current.type_ == .rbrace {
			break
		}

		if p.current.type_ != .comma {
			return error('Expected comma or closing brace')
		}

		p.advance() // Skip comma
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}
	p.advance() // Skip '}'

	// For now, we'll use the first field update
	// TODO: Support multiple field updates
	if field_updates.len == 0 {
		return error('Expected at least one field update')
	}

	// If there's only one field update, use it
	if field_updates.len == 1 {
		first_field := field_updates[0]
		return ast.new_record_update(p.get_next_id(), record_name, record_expr, first_field.value,
			first_field.children[0], pos)
	}

	// For multiple field updates, we need to chain them
	// For now, just use the first one and ignore the rest
	// TODO: Implement proper chaining of multiple field updates
	first_field := field_updates[0]
	return ast.new_record_update(p.get_next_id(), record_name, record_expr, first_field.value,
		first_field.children[0], pos)
}

// New parsing functions for additional functionality

fn (mut p Parser) parse_type_alias() !ast.Node {
	if p.current.type_ != .type {
		return p.error_and_return('Expected "type" keyword')
	}
	start_pos := p.current.position
	p.advance() // Skip 'type'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected type alias name')
	}
	type_name := p.current.value
	p.advance()

	if p.current.type_ != .bind {
		return p.error_and_return('Expected "=" after type alias name')
	}
	p.advance() // Skip '='

	type_def := p.parse_type_expression()!

	return ast.new_type_alias(p.get_next_id(), type_name, type_def, start_pos)
}

fn (mut p Parser) parse_type_expression() !ast.Node {
	pos := p.current.position

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected type name')
	}

	type_name := p.current.value
	p.advance()

	// Handle parameterized types like list(integer)
	if p.current.type_ == .lparen {
		p.advance() // Skip '('
		mut params := []ast.Node{}

		if p.current.type_ != .rparen {
			for {
				param := p.parse_type_expression()!
				params << param

				if p.current.type_ == .rparen {
					break
				}
				if p.current.type_ != .comma {
					return p.error_and_return('Expected comma or closing parenthesis in type parameters')
				}
				p.advance() // Skip comma
			}
		}

		if p.current.type_ != .rparen {
			return p.error_and_return('Expected closing parenthesis in type expression')
		}
		p.advance() // Skip ')'

		return ast.Node{
			id:       p.get_next_id()
			kind:     .identifier
			value:    type_name
			children: params
			position: pos
		}
	}

	return ast.new_identifier(p.get_next_id(), type_name, pos)
}

fn (mut p Parser) parse_case_expression() !ast.Node {
	if p.current.type_ != .case {
		return p.error_and_return('Expected "case" keyword')
	}
	start_pos := p.current.position
	p.advance() // Skip 'case'

	expr := p.parse_expression()!

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after case expression')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	mut clauses := []ast.Node{}
	for p.current.type_ != .end && p.current.type_ != .eof {
		if p.current.type_ == .newline {
			p.advance()
			continue
		}

		clause := p.parse_case_clause()!
		clauses << clause
	}

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close case expression')
	}
	p.advance() // Skip 'end'

	return ast.new_case_expression(p.get_next_id(), expr, clauses, start_pos)
}

fn (mut p Parser) parse_case_clause() !ast.Node {
	start_pos := p.current.position

	// Parse pattern
	pattern := p.parse_pattern()!

	// Parse optional guard
	mut guard := ast.Node{}
	if p.current.type_ == .when {
		p.advance() // Skip 'when'
		guard = p.parse_expression()!
	}

	if p.current.type_ != .arrow {
		return p.error_and_return('Expected "->" after case pattern')
	}
	p.advance() // Skip '->'

	// Skip newlines after '->'
	for p.current.type_ == .newline {
		p.advance()
	}

	// Parse the body as a block instead of a single expression
	// This allows complex expressions like 'with', 'if', etc. inside case clauses
	body := p.parse_case_clause_body()!

	// Se há guard, inclui como terceiro child, senão usa apenas pattern e body
	if guard.id != 0 {
		return ast.new_case_clause_with_guard(p.get_next_id(), pattern, guard, body, start_pos)
	} else {
		return ast.new_case_clause(p.get_next_id(), pattern, body, start_pos)
	}
}

// Parse the body of a case clause, which can contain multiple expressions
// similar to how LX5 handles it
fn (mut p Parser) parse_case_clause_body() !ast.Node {
	mut expressions := []ast.Node{}
	start_pos := p.current.position

	for {
		// Stop if we encounter patterns that indicate next case clause or end
		if p.current.type_ == .end || p.current.type_ == .eof {
			break
		}

		// Check if this looks like a new case pattern
		if p.looks_like_case_pattern() {
			break
		}

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		// Stop again after skipping newlines
		if p.current.type_ == .end || p.current.type_ == .eof {
			break
		}

		if p.looks_like_case_pattern() {
			break
		}

		expr := p.parse_expression()!
		expressions << expr

		// Handle separators
		if p.current.type_ == .semicolon {
			p.advance()
		} else if p.current.type_ == .newline {
			p.advance()
		} else {
			// If no separator and not at end, continue
			if p.current.type_ != .end && p.current.type_ != .eof && !p.looks_like_case_pattern() {
				continue
			}
			break
		}
	}

	// If we only have one expression, return it directly
	if expressions.len == 1 {
		return expressions[0]
	}

	// If we have multiple expressions, create a block
	return ast.new_block(p.get_next_id(), expressions, start_pos)
}

// Check if current position looks like a case pattern
fn (mut p Parser) looks_like_case_pattern() bool {
	// A case pattern can ONLY start at the beginning of a new line
	// This is the key rule mentioned by the user
	if !p.at_line_start {
		return false
	}

	// First, check for obvious non-patterns
	if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_
		|| p.current.type_ == .if_ || p.current.type_ == .with || p.current.type_ == .case
		|| p.current.type_ == .fn || p.current.type_ == .spawn || p.current.type_ == .receive
		|| p.current.type_ == .do || p.current.type_ == .rescue {
		return false
	}

	// For patterns that are followed immediately by -> or when, we're confident
	if p.next.type_ == .arrow || p.next.type_ == .when {
		return true
	}

	// For tokens that could reasonably start a pattern at line beginning,
	// assume they are patterns (since we're at line start in case context)
	if p.current.type_ == .lbrace || p.current.type_ == .lbracket || p.current.type_ == .identifier
		|| p.current.type_ == .integer || p.current.type_ == .string || p.current.type_ == .atom
		|| p.current.type_ == .true_ || p.current.type_ == .false_ || p.current.type_ == .nil_ {
		return true
	}

	// For everything else, be conservative
	return false
}

fn (mut p Parser) parse_pattern() !ast.Node {
	match p.current.type_ {
		.lbracket {
			// List pattern: [] or [head | tail]
			return p.parse_list_pattern()
		}
		.lbrace {
			// Tuple pattern: {a, b, c}
			return p.parse_tuple_pattern()
		}
		.double_lt {
			// Binary pattern: <<version:8, size:16, rest/binary>>
			return p.parse_binary_pattern()
		}
		.identifier {
			// Variable pattern, atom, or record pattern
			name := p.current.value
			identifier_pos := p.current.position
			p.advance()

			// Check if this is a record pattern
			if p.current.type_ == .lbrace {
				// This is a record pattern, not a variable
				record_node := ast.Node{
					id:       p.get_next_id()
					kind:     .identifier
					value:    name
					children: []
					position: identifier_pos
				}
				return p.parse_record_literal_with_name(record_node)
			}

			// Check for type annotation
			if p.current.type_ == .double_colon {
				p.advance() // Skip ::
				type_annotation := p.parse_type_expression()!

				// Create identifier with type annotation
				return ast.Node{
					id:       p.get_next_id()
					kind:     .identifier
					value:    name
					children: [type_annotation]
					position: identifier_pos
				}
			}

			return ast.new_identifier(p.get_next_id(), name, identifier_pos)
		}
		.integer, .float, .string, .true_, .false_, .nil_, .atom {
			// Literal patterns
			return p.parse_literal()
		}
		else {
			return p.error_and_return('Invalid pattern')
		}
	}
}

// Parse binary pattern: <<version:8, size:16, rest/binary>>
fn (mut p Parser) parse_binary_pattern() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip '<<'

	// Skip newlines after opening <<
	for p.current.type_ == .newline {
		p.advance()
	}

	mut segments := []ast.Node{}

	// Parse segments
	if p.current.type_ != .double_gt {
		for {
			// Skip newlines before segment
			for p.current.type_ == .newline {
				p.advance()
			}

			segment := p.parse_binary_segment_pattern()!
			segments << segment

			// Skip newlines after segment
			for p.current.type_ == .newline {
				p.advance()
			}

			if p.current.type_ == .comma {
				p.advance() // Skip ','
				// Skip newlines after comma
				for p.current.type_ == .newline {
					p.advance()
				}
			} else {
				break
			}
		}
	}

	// Skip newlines before closing >>
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .double_gt {
		return p.error_and_return('Expected ">>" to close binary pattern')
	}
	p.advance() // Skip '>>'

	return ast.new_binary_pattern(p.get_next_id(), segments, start_pos)
}

// Parse binary segment pattern: version:8 or rest/binary
fn (mut p Parser) parse_binary_segment_pattern() !ast.Node {
	start_pos := p.current.position

	// Parse variable name
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected variable name in binary pattern')
	}

	variable := p.parse_binary_expression()!

	mut size := ?ast.Node(none)
	mut options := []string{}

	// Parse size: variable:size
	if p.current.type_ == .colon {
		p.advance() // Skip ':'
		size = p.parse_binary_expression()!
	}

	// Parse options: /binary, /integer, etc.
	if p.current.type_ == .slash {
		p.advance() // Skip '/'

		// Parse option string
		if p.current.type_ != .identifier {
			return p.error_and_return('Invalid binary pattern option: expected identifier after /')
		}

		mut option_str := p.current.value
		p.advance()

		// Check for unit:N format
		if p.current.type_ == .colon && option_str == 'unit' {
			p.advance() // Skip ':'
			if p.current.type_ == .integer {
				option_str += ':' + p.current.value
				p.advance()
			} else {
				return p.error_and_return('Expected integer after unit:')
			}
		}

		options << option_str

		// Parse additional options with dashes
		mut dash := true
		for dash {
			if p.current.type_ == .identifier && p.current.value == '-' {
				p.advance() // Skip '-'
				if p.current.type_ == .identifier {
					mut next_option := p.current.value
					p.advance()

					// Check for unit:N format in chained options
					if p.current.type_ == .colon && next_option == 'unit' {
						p.advance() // Skip ':'
						if p.current.type_ == .integer {
							next_option += ':' + p.current.value
							p.advance()
						} else {
							return p.error_and_return('Expected integer after unit:')
						}
					}

					options << next_option
				} else {
					return p.error_and_return('Invalid binary pattern option after -')
				}
			} else {
				dash = false
			}
		}
	}

	return ast.new_binary_segment(p.get_next_id(), variable, size, options, start_pos)
}

// Parse binary or pattern - decides if it's <<...>> = expr or just <<...>>
fn (mut p Parser) parse_binary_or_pattern() !ast.Node {
	// Parse as binary pattern first
	binary_pattern := p.parse_binary_pattern()!

	// Check if there's a = after the pattern (pattern matching)
	if p.current.type_ == .bind {
		p.advance() // Skip '='
		expr := p.parse_expression()!
		return ast.new_pattern_binding(p.get_next_id(), binary_pattern, expr, binary_pattern.position)
	} else {
		// No =, so this is a binary literal (but we parsed it as pattern)
		// We need to convert the pattern back to a literal
		return p.convert_pattern_to_literal(binary_pattern)
	}
}

// Convert a binary pattern to a binary literal
fn (mut p Parser) convert_pattern_to_literal(pattern ast.Node) !ast.Node {
	// Convert binary_pattern to binary_literal
	return ast.Node{
		id:       pattern.id
		kind:     .binary_literal
		value:    pattern.value
		children: pattern.children
		position: pattern.position
	}
}

fn (mut p Parser) parse_list_pattern() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['

	if p.current.type_ == .rbracket {
		p.advance() // Skip ']'
		return ast.new_list_literal(p.get_next_id(), [], pos)
	}

	first_element := p.parse_pattern()!

	if p.current.type_ == .pipe {
		// List cons pattern: [head | tail]
		p.advance() // Skip '|'
		tail := p.parse_pattern()!

		if p.current.type_ != .rbracket {
			return p.error_and_return('Expected "]" after list cons pattern')
		}
		p.advance() // Skip ']'

		return ast.new_list_cons(p.get_next_id(), first_element, tail, pos)
	}

	// Regular list pattern: [elem1, elem2, ...]
	mut elements := [first_element]

	for p.current.type_ == .comma {
		p.advance() // Skip ','
		element := p.parse_pattern()!
		elements << element
	}

	if p.current.type_ != .rbracket {
		return p.error_and_return('Expected "]" to close list pattern')
	}
	p.advance() // Skip ']'

	return ast.new_list_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_tuple_pattern() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'

	// Skip newlines after opening brace
	for p.current.type_ == .newline {
		p.advance()
	}

	// Check for empty tuple
	if p.current.type_ == .rbrace {
		p.advance() // Skip '}'
		return ast.new_tuple_literal(p.get_next_id(), [], pos)
	}

	// Parse first pattern
	first_pattern := p.parse_pattern()!
	mut patterns := [first_pattern]

	// Parse remaining patterns
	for p.current.type_ == .comma {
		p.advance() // Skip comma

		// Skip newlines after comma
		for p.current.type_ == .newline {
			p.advance()
		}

		pattern := p.parse_pattern()!
		patterns << pattern
	}

	// Skip newlines before closing brace
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace in tuple pattern')
	}
	p.advance() // Skip '}'

	return ast.new_tuple_literal(p.get_next_id(), patterns, pos)
}

fn (mut p Parser) parse_lambda_expression() !ast.Node {
	if p.current.type_ != .fn {
		return p.error_and_return('Expected "fn" keyword')
	}
	start_pos := p.current.position
	p.advance() // Skip 'fn'

	// Check if it's a multi-head lambda: fn do ... end
	if p.current.type_ == .do {
		p.advance() // Skip 'do'

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		// Parse multiple heads
		mut heads := []ast.Node{}
		for p.current.type_ != .end && p.current.type_ != .eof {
			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .lparen {
				head := p.parse_function_head()!
				heads << head
			} else {
				break
			}
		}

		if heads.len == 0 {
			return p.error_and_return('Expected at least one function head (pattern) in multi-head lambda')
		}

		if p.current.type_ != .end {
			return p.error_and_return('Expected "end" to close multi-head lambda')
		}
		p.advance() // Skip 'end'

		body := ast.new_block(p.get_next_id(), heads, heads[0].position)
		return ast.new_lambda_expression(p.get_next_id(), [], body, start_pos)
	}

	// Regular lambda with parameters: fn(params) -> expr or fn(params) do ... end
	if p.current.type_ != .lparen {
		return p.error_and_return('Expected "(" after fn or "do" for multi-head lambda')
	}
	p.advance() // Skip '('

	mut params := []ast.Node{}
	if p.current.type_ != .rparen {
		for {
			if p.current.type_ != .identifier {
				return p.error_and_return('Expected parameter name in lambda')
			}
			param_name := p.current.value
			param_pos := p.current.position
			p.advance()

			// Parse type annotation if present
			mut type_annotation := ast.Node{}
			if p.current.type_ == .double_colon {
				p.advance() // Skip ::
				type_annotation = p.parse_type_annotation()!
			}

			// Create parameter with optional type annotation
			param := ast.Node{
				id:       p.get_next_id()
				kind:     .function_parameter
				value:    param_name
				children: if type_annotation.value != '' { [type_annotation] } else { [] }
				position: param_pos
			}
			params << param

			if p.current.type_ == .rparen {
				break
			}
			if p.current.type_ != .comma {
				return p.error_and_return('Expected comma or closing parenthesis in lambda parameters')
			}
			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rparen {
		return p.error_and_return('Expected closing parenthesis in lambda parameters')
	}
	p.advance() // Skip ')'

	if p.current.type_ == .arrow {
		p.advance() // Skip '->'

		// Check if there's a newline after arrow - if so, require 'end'
		if p.current.type_ == .newline {
			// Skip newlines
			for p.current.type_ == .newline {
				p.advance()
			}

			// Parse body as block and expect 'end'
			body := p.parse_block()!

			if p.current.type_ != .end {
				return p.error_and_return('Expected "end" to close lambda body after line break')
			}
			p.advance() // Skip 'end'

			return ast.new_lambda_expression(p.get_next_id(), params, body, start_pos)
		} else {
			// Single line after arrow - no 'end' needed
			body := p.parse_expression()!
			return ast.new_lambda_expression(p.get_next_id(), params, body, start_pos)
		}
	} else if p.current.type_ == .do {
		p.advance() // Skip 'do'

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		body := p.parse_block()!

		if p.current.type_ != .end {
			return p.error_and_return('Expected "end" to close lambda body')
		}
		p.advance() // Skip 'end'

		return ast.new_lambda_expression(p.get_next_id(), params, body, start_pos)
	}

	// This should never be reached, but V requires explicit return
	return p.error_and_return('Expected "->" or "do" after lambda parameters')
}

// ============ Task 11: Control Flow Parsing ============

// Parse if expressions: if condition do ... else ... end
fn (mut p Parser) parse_if_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'if'

	condition := p.parse_expression()!

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after if condition')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	then_expr := p.parse_block()!

	mut else_expr := ?ast.Node(none)
	if p.current.type_ == .else_ {
		p.advance() // Skip 'else'

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		else_expr = p.parse_block()!
	}

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close if expression')
	}
	p.advance() // Skip 'end'

	return ast.new_if_expr(p.get_next_id(), condition, then_expr, else_expr, start_pos)
}

// Parse with expressions: with pattern <- expr [, pattern <- expr ...] do ... else ... end
fn (mut p Parser) parse_with_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'with'

	// Parse multiple clauses
	mut clauses := []ast.Node{}

	for {
		pattern := p.parse_expression()!

		if p.current.type_ != .left_arrow {
			return p.error_and_return('Expected "<-" after with pattern')
		}
		p.advance() // Skip '<-'

		expr := p.parse_expression()!

		// Create clause node (pattern and expression)
		clause := ast.Node{
			id:       p.get_next_id()
			kind:     .pattern_match
			children: [pattern, expr]
			position: start_pos
		}
		clauses << clause

		// Check if there are more clauses
		if p.current.type_ == .comma {
			p.advance() // Skip ','
			// Skip newlines after comma
			for p.current.type_ == .newline {
				p.advance()
			}
			continue
		} else {
			break
		}
	}

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after with expression')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_block()!

	mut else_body := ?ast.Node(none)
	if p.current.type_ == .else_ {
		p.advance() // Skip 'else'

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		// Check if this is case-style else (pattern -> expr) or block-style else
		if p.looks_like_case_clause() {
			else_body = p.parse_case_clauses_as_block()!
		} else {
			else_body = p.parse_block()!
		}
	}

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close with expression')
	}
	p.advance() // Skip 'end'

	// Support multiple clauses
	return ast.new_with_expr_multi(p.get_next_id(), clauses, body, else_body, start_pos)
}

// Check if current position looks like a case clause (pattern -> expr)
fn (mut p Parser) looks_like_case_clause() bool {
	// Simple heuristic: if current token could start a pattern and we're not at 'end'
	// then assume it's a case-style else
	return p.current.type_ != .end && (p.current.type_ == .identifier
		|| p.current.type_ == .lbrace || p.current.type_ == .lbracket
		|| p.current.type_ == .integer || p.current.type_ == .string
		|| p.current.type_ == .atom)
}

// Parse case-style clauses as a case expression block
fn (mut p Parser) parse_case_clauses_as_block() !ast.Node {
	start_pos := p.current.position
	mut clauses := []ast.Node{}

	for p.current.type_ != .end && p.current.type_ != .eof {
		if p.current.type_ == .newline {
			p.advance()
			continue
		}

		clause := p.parse_case_clause()!
		clauses << clause

		// Skip optional newlines after clause
		for p.current.type_ == .newline {
			p.advance()
		}
	}

	// Create a dummy case expression for the else clauses
	dummy_expr := ast.new_identifier(p.get_next_id(), 'Error', start_pos)
	return ast.new_case_expression(p.get_next_id(), dummy_expr, clauses, start_pos)
}

// Parse match expressions with continuation (collects all remaining expressions)
fn (mut p Parser) parse_match_with_continuation() !ast.Node {
	start_pos := p.current.position

	p.advance() // Skip 'match'

	pattern := p.parse_expression()!

	if p.current.type_ != .left_arrow {
		return p.error_and_return('Expected "<-" after match pattern')
	}
	p.advance() // Skip '<-'

	expr := p.parse_expression()!

	// Check for rescue clause
	mut rescue_body := ast.Node{}

	if p.current.type_ == .rescue {
		p.advance() // Skip 'rescue'

		error_pattern := p.parse_expression()! // error pattern - should be a variable

		// Store the error pattern for the analyzer to register
		// We'll add it to the rescue body as context

		if p.current.type_ != .do {
			return p.error_and_return('Expected "do" after rescue pattern')
		}
		p.advance() // Skip 'do'

		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		rescue_body_expr := p.parse_expression()!

		// Create a rescue block that includes both the error pattern and the body
		rescue_body = ast.Node{
			id:       p.get_next_id()
			kind:     .block
			children: [error_pattern, rescue_body_expr]
			position: start_pos
		}

		// Skip newlines after rescue body
		for p.current.type_ == .newline {
			p.advance()
		}

		if p.current.type_ != .end {
			return p.error_and_return('Expected "end" to close match rescue expression, got ${p.current.type_}')
		}
		p.advance() // Skip 'end'

		// After rescue, continue parsing for continuation expressions
		// Skip separators
		if p.current.type_ == .semicolon {
			p.advance()
		}
		for p.current.type_ == .newline {
			p.advance()
		}

		// Collect continuation expressions after rescue
		mut continuation_exprs := []ast.Node{}
		for {
			// Stop if we encounter 'end', 'else' or other non-expression tokens
			if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
				break
			}

			// Skip newlines before expression
			for p.current.type_ == .newline {
				p.advance()
			}

			// Stop if we encounter 'end', 'else' after skipping newlines
			if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
				break
			}

			expr_cont := p.parse_expression()!
			continuation_exprs << expr_cont

			// Check for separators
			if p.current.type_ == .semicolon {
				p.advance()
				for p.current.type_ == .newline {
					p.advance()
				}
			} else if p.current.type_ == .newline {
				p.advance()
				for p.current.type_ == .newline {
					p.advance()
				}
			} else {
				// Continue if there are more tokens
				if p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
					continue
				}
				break
			}
		}

		// If there are continuation expressions, wrap them in a block and create match with continuation
		if continuation_exprs.len > 0 {
			continuation_block := ast.new_block(p.get_next_id(), continuation_exprs, start_pos)
			// Create match expr manually with both rescue and continuation
			mut children := [pattern, expr, rescue_body, continuation_block]
			return ast.Node{
				id:       p.get_next_id()
				kind:     .match_expr
				children: children
				position: start_pos
			}
		} else {
			return ast.new_match_expr(p.get_next_id(), pattern, expr, rescue_body, start_pos)
		}
	}

	// Skip separators
	if p.current.type_ == .semicolon {
		p.advance()
	}
	for p.current.type_ == .newline {
		p.advance()
	}

	// Collect all remaining expressions as continuation
	mut continuation_exprs := []ast.Node{}
	for {
		// Stop if we encounter 'end', 'else' or other non-expression tokens
		if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
			break
		}

		// Skip newlines before expression
		for p.current.type_ == .newline {
			p.advance()
		}

		// Stop if we encounter 'end', 'else' after skipping newlines
		if p.current.type_ == .end || p.current.type_ == .eof || p.current.type_ == .else_ {
			break
		}

		// Handle nested matches recursively
		if p.current.type_ == .match {
			nested_match := p.parse_match_with_continuation()!
			continuation_exprs << nested_match
			break // Nested match consumes all remaining expressions
		}

		expr_cont := p.parse_expression()!
		continuation_exprs << expr_cont

		// Check for separators
		if p.current.type_ == .semicolon {
			p.advance()
			for p.current.type_ == .newline {
				p.advance()
			}
		} else if p.current.type_ == .newline {
			p.advance()
			for p.current.type_ == .newline {
				p.advance()
			}
		} else {
			// Continue if there are more tokens
			if p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
				continue
			}
			break
		}
	}

	// Create a match expression with continuation as a nested block
	if continuation_exprs.len > 0 {
		continuation_block := ast.new_block(p.get_next_id(), continuation_exprs, start_pos)
		// Simple match (no rescue) with continuation

		return ast.new_match_expr(p.get_next_id(), pattern, expr, continuation_block,
			start_pos)
	} else {
		// Simple match (no rescue) without continuation

		return ast.new_match_expr(p.get_next_id(), pattern, expr, none, start_pos)
	}
}

// Parse match expressions: match pattern <- expr rescue error do ... end
// or simple match: match pattern <- expr (fail fast)
fn (mut p Parser) parse_match_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'match'

	pattern := p.parse_expression()!

	if p.current.type_ != .left_arrow {
		return p.error_and_return('Expected "<-" after match pattern')
	}
	p.advance() // Skip '<-'

	expr := p.parse_expression()!

	// Check if this is a simple match (no rescue clause)
	if p.current.type_ != .rescue {
		// Simple match: match pattern <- expr (fail fast)
		return ast.new_match_expr(p.get_next_id(), pattern, expr, none, start_pos)
	}

	// Full match with rescue: match pattern <- expr rescue error do ... end
	p.advance() // Skip 'rescue'

	_ := p.parse_expression()! // error pattern (not used in AST for now)

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after rescue pattern')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	rescue_body := p.parse_block()!

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close match expression')
	}
	p.advance() // Skip 'end'

	return ast.new_match_expr(p.get_next_id(), pattern, expr, rescue_body, start_pos)
}

// Parse list comprehensions: for x in list when condition do expr end
fn (mut p Parser) parse_list_comprehension() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'for'

	// Parse variable
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected variable name after "for"')
	}
	var_name := p.current.value
	var_pos := p.current.position
	p.advance()

	// Parse 'in'
	if p.current.type_ != .in {
		return p.error_and_return('Expected "in" after variable name')
	}
	p.advance()

	// Parse list expression
	list_expr := p.parse_expression()!

	mut condition_expr := ast.Node{}
	mut has_condition := false

	// Parse optional 'when' condition
	if p.current.type_ == .when {
		p.advance() // Skip 'when'
		condition_expr = p.parse_expression()!
		has_condition = true
	}

	// Parse 'do'
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after list expression')
	}
	p.advance()

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	// Parse body expression
	body_expr := p.parse_expression()!

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	// Parse 'end'
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close list comprehension')
	}
	p.advance()

	// Create variable node
	var_node := ast.new_variable_ref(p.get_next_id(), var_name, var_pos)

	// Build children: [variable, list, body, condition?]
	mut children := [var_node, list_expr, body_expr]
	if has_condition {
		children << condition_expr
	}

	return ast.new_list_comprehension(p.get_next_id(), children, start_pos)
}

// ============ Task 11: Concurrency Parsing ============

// Parse spawn expressions: spawn(fn -> ... end)
fn (mut p Parser) parse_spawn_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'spawn'

	if p.current.type_ != .lparen {
		return p.error_and_return('Expected "(" after spawn')
	}
	p.advance() // Skip '('

	func_expr := p.parse_expression()!

	if p.current.type_ != .rparen {
		return p.error_and_return('Expected ")" after spawn expression')
	}
	p.advance() // Skip ')'

	return ast.new_spawn_expr(p.get_next_id(), func_expr, start_pos)
}

// Parse receive expressions: receive do pattern -> expr; pattern -> expr end
fn (mut p Parser) parse_receive_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'receive'

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after receive')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	mut clauses := []ast.Node{}

	for p.current.type_ != .end && p.current.type_ != .eof {
		// Skip newlines between clauses
		for p.current.type_ == .newline {
			p.advance()
		}

		if p.current.type_ == .end {
			break
		}

		pattern := p.parse_expression()!

		if p.current.type_ != .arrow {
			return p.error_and_return('Expected "->" after receive pattern')
		}
		p.advance() // Skip '->'

		expr := p.parse_expression()!

		clause := ast.new_case_clause(p.get_next_id(), pattern, expr, start_pos)
		clauses << clause

		// Skip optional semicolon
		if p.current.type_ == .semicolon {
			p.advance()
		}
	}

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close receive expression')
	}
	p.advance() // Skip 'end'

	return ast.new_receive_expr(p.get_next_id(), clauses, start_pos)
}

// ============ Task 11: Binaries Parsing ============

// Parse binary literals: <<1, 2, 3>>, <<"hello">>
fn (mut p Parser) parse_binary_literal() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip '<<'

	// Skip newlines after opening <<
	for p.current.type_ == .newline {
		p.advance()
	}

	mut segments := []ast.Node{}

	if p.current.type_ != .double_gt {
		for {
			// Skip newlines before segment
			for p.current.type_ == .newline {
				p.advance()
			}

			segment := p.parse_binary_segment()!
			segments << segment

			// Skip newlines after segment
			for p.current.type_ == .newline {
				p.advance()
			}

			if p.current.type_ == .comma {
				p.advance() // Skip ','
				// Skip newlines after comma
				for p.current.type_ == .newline {
					p.advance()
				}
			} else {
				break
			}
		}
	}

	// Skip newlines before closing >>
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .double_gt {
		return p.error_and_return('Expected ">>" to close binary literal')
	}
	p.advance() // Skip '>>'

	// Check if this is a pattern match (binary pattern followed by =)
	if p.current.type_ == .bind {
		// Convert to binary pattern and create pattern binding
		binary_pattern := ast.new_binary_pattern(p.get_next_id(), segments, start_pos)
		p.advance() // Skip '='
		expr := p.parse_expression()!
		return ast.new_pattern_binding(p.get_next_id(), binary_pattern, expr, start_pos)
	}

	return ast.new_binary_literal(p.get_next_id(), segments, start_pos)
}

// Parse binary segment: value:size/options
fn (mut p Parser) parse_binary_segment() !ast.Node {
	start_pos := p.current.position

	// Parse value within binary context (/ is not division here)
	value := p.parse_binary_expression()!

	mut size := ?ast.Node(none)
	mut options := []string{}

	// Parse size: value:size
	if p.current.type_ == .colon {
		p.advance() // Skip ':'
		size = p.parse_binary_expression()!
	} else if p.current.type_ == .atom {
		// Handle case where lexer tokenized :identifier as atom (consuming the colon)
		// In this case, the atom represents the size parameter
		size = p.parse_binary_expression()!
	}

	// Parse options: /integer-big or /unit:8
	if p.current.type_ == .slash {
		p.advance() // Skip '/'

		// Parse option string
		if p.current.type_ != .identifier {
			return p.error_and_return('Invalid binary segment option: expected identifier after /')
		}

		mut option_str := p.current.value
		p.advance()

		// Check for unit:N format
		if p.current.type_ == .colon && option_str == 'unit' {
			p.advance() // Skip ':'
			if p.current.type_ == .integer {
				option_str += ':' + p.current.value
				p.advance()
			} else {
				return p.error_and_return('Expected integer after unit:')
			}
		}

		options << option_str

		// Parse additional options with dashes
		mut dash := true
		for dash {
			if p.current.type_ == .identifier && p.current.value == '-' {
				p.advance() // Skip '-'
				if p.current.type_ == .identifier {
					mut next_option := p.current.value
					p.advance()

					// Check for unit:N format in chained options
					if p.current.type_ == .colon && next_option == 'unit' {
						p.advance() // Skip ':'
						if p.current.type_ == .integer {
							next_option += ':' + p.current.value
							p.advance()
						} else {
							return p.error_and_return('Expected integer after unit:')
						}
					}

					options << next_option
				} else {
					return p.error_and_return('Invalid binary pattern option after -')
				}
			} else {
				dash = false
			}
		}
	}

	return ast.new_binary_segment(p.get_next_id(), value, size, options, start_pos)
}

// Parse expression within binary context where / is not division
fn (mut p Parser) parse_binary_expression() !ast.Node {
	// In binary context, we only parse atoms, literals, identifiers, and function calls
	// We stop at :, /, ,, and >>
	return match p.current.type_ {
		.integer, .float, .string, .true_, .false_, .nil_ {
			p.parse_literal()
		}
		.atom {
			// In binary context, treat atoms as identifiers (variable references)
			// This happens because lexer tokenizes :identifier as atom instead of : + identifier
			atom_value := p.current.value
			pos := p.current.position
			p.advance()

			// Convert atom to identifier for use as variable reference in binary context
			ast.new_identifier(p.get_next_id(), atom_value, pos)
		}
		.identifier {
			// Simple identifier or function call in binary context
			identifier := p.current.value
			pos := p.current.position
			p.advance()

			// Check if this is a function call
			if p.current.type_ == .lparen {
				return p.parse_function_call(identifier, pos)
			}

			// Otherwise, it's just a simple identifier (variable reference)
			return ast.new_identifier(p.get_next_id(), identifier, pos)
		}
		.lparen {
			p.advance() // Skip '('
			expr := p.parse_expression()!
			if p.current.type_ != .rparen {
				return p.error_and_return('Expected ")" after expression')
			}
			p.advance() // Skip ')'
			expr
		}
		else {
			p.error_and_return('Invalid expression in binary segment')
		}
	}
}

// ============ Task 11: Module System Parsing ============

// Parse deps declaration: deps [:cowboy, :jsx]
fn (mut p Parser) parse_deps_declaration() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'deps'

	if p.current.type_ != .lbracket {
		return p.error_and_return('Expected "[" after deps')
	}

	deps_list := p.parse_list_expression()!

	return ast.new_deps_declaration(p.get_next_id(), deps_list.children, start_pos)
}

// Parse application config: application { ... }
fn (mut p Parser) parse_application_config() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'application'

	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected "{" after application')
	}

	config_map := p.parse_map_literal()!

	return ast.new_application_config(p.get_next_id(), config_map.children, start_pos)
}

// Parse import statement: import Module
fn (mut p Parser) parse_import_statement() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'import'

	if p.current.type_ != .identifier && p.current.type_ != .atom {
		return p.error_and_return('Expected module name after import')
	}

	module_name := p.current.value
	p.advance()

	return ast.new_import_statement(p.get_next_id(), module_name, start_pos)
}

// Parse supervisor definition: supervisor name do ... end
fn (mut p Parser) parse_supervisor_definition() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'supervisor'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected supervisor name')
	}

	name := p.current.value
	p.advance()

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after supervisor name')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_block()!

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close supervisor definition')
	}
	p.advance() // Skip 'end'

	return ast.new_supervisor_def(p.get_next_id(), name, body, start_pos)
}

// Parse worker definition: worker name do ... end
fn (mut p Parser) parse_worker_definition() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'worker'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected worker name')
	}

	name := p.current.value
	p.advance()

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after worker name')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_block()!

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close worker definition')
	}
	p.advance() // Skip 'end'

	return ast.new_worker_def(p.get_next_id(), name, body, start_pos)
}

// ============ Task 11: Advanced Features Parsing ============

// Parse @ directives: @doc, @spec, @moduledoc
fn (mut p Parser) parse_directive_new() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip '@'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected directive name after @')
	}

	name := p.current.value
	p.advance()

	mut args := []ast.Node{}

	// Check for parentheses format: @doc("text")
	if p.current.type_ == .lparen {
		p.advance() // Skip '('

		if p.current.type_ != .rparen {
			for {
				arg := p.parse_expression()!
				args << arg

				if p.current.type_ == .comma {
					p.advance() // Skip ','
				} else {
					break
				}
			}
		}

		if p.current.type_ != .rparen {
			return p.error_and_return('Expected ")" to close directive arguments')
		}
		p.advance() // Skip ')'
	} else if p.current.type_ == .string {
		// Direct string format: @doc "text"
		arg := p.parse_expression()!
		args << arg
	}
	match name {
		'moduledoc' {
			p.directives_table.update_moduledoc(args[0].value)
		}
		'doc' {
			p.temp_doc_node = args[0]
		}
		else {}
	}
	return ast.new_directive(p.get_next_id(), name, args, start_pos)
}

// Parse test blocks: describe "name" do ... end
fn (mut p Parser) parse_test_block() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'describe' or 'test'

	if p.current.type_ != .string {
		return p.error_and_return('Expected test name string')
	}

	name := p.current.value
	p.advance()

	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after test name')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_block()!

	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close test block')
	}
	p.advance() // Skip 'end'

	return ast.new_test_block(p.get_next_id(), name, body, start_pos)
}

fn (mut p Parser) parse_lambda_call(lambda ast.Node) !ast.Node {
	// lambda.(...args...)
	if p.current.type_ != .lparen {
		return error('Expected opening parenthesis for lambda call')
	}
	pos := p.current.position
	p.advance() // Skip '('

	mut args := []ast.Node{}
	if p.current.type_ != .rparen {
		for {
			arg := p.parse_expression()!
			args << arg

			if p.current.type_ == .rparen {
				break
			}
			if p.current.type_ != .comma {
				return error('Expected comma or closing parenthesis in lambda call arguments')
			}
			p.advance() // Skip comma
		}
	}

	if p.current.type_ != .rparen {
		return error('Expected closing parenthesis for lambda call')
	}
	p.advance() // Skip ')'

	return ast.new_lambda_call(p.get_next_id(), lambda, args, pos)
}

// Parse type definitions: type name :: definition
fn (mut p Parser) parse_type_def() !ast.Node {
	if p.current.type_ != .type {
		return p.error_and_return('Expected "type" keyword')
	}
	start_pos := p.current.position
	p.advance() // Skip 'type'

	// Check for opaque/nominal modifiers
	mut is_opaque := false
	mut is_nominal := false

	if p.current.type_ == .identifier && p.current.value == 'opaque' {
		is_opaque = true
		p.advance()
	} else if p.current.type_ == .identifier && p.current.value == 'nominal' {
		is_nominal = true
		p.advance()
	}

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected type name')
	}
	type_name := p.current.value
	p.advance()

	// Parse generic parameters if present: name(T)
	mut params := []string{}
	if p.current.type_ == .lparen {
		p.advance() // Skip '('

		if p.current.type_ != .rparen {
			for {
				if p.current.type_ != .identifier {
					return p.error_and_return('Expected type parameter name')
				}
				params << p.current.value
				p.advance()

				if p.current.type_ == .rparen {
					break
				}
				if p.current.type_ != .comma {
					return p.error_and_return('Expected comma or closing parenthesis')
				}
				p.advance() // Skip comma
			}
		}

		if p.current.type_ != .rparen {
			return p.error_and_return('Expected closing parenthesis')
		}
		p.advance() // Skip ')'
	}

	if p.current.type_ != .double_colon {
		return p.error_and_return('Expected "::" after type name')
	}
	p.advance() // Skip '::'

	type_def := p.parse_type_expression()!

	// Create appropriate type node based on modifiers
	if is_opaque {
		return ast.new_opaque_type(p.get_next_id(), type_name, type_def, start_pos)
	} else if is_nominal {
		return ast.new_nominal_type(p.get_next_id(), type_name, type_def, start_pos)
	} else {
		// For regular types, convert single type_def to variants array
		variants := [type_def]
		return ast.new_type_def(p.get_next_id(), type_name, variants, start_pos)
	}
}

// Parse assert expressions: assert condition
fn (mut p Parser) parse_assert_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'assert'

	condition := p.parse_expression()!

	return ast.new_function_caller(p.get_next_id(), 'assert', [condition], start_pos)
}

// Parse describe blocks: describe "name" do ... end
fn (mut p Parser) parse_describe_block() ![]ast.Node {
	p.advance() // Skip 'describe'

	if p.current.type_ != .string {
		return error('Expected test suite name string')
	}

	_ := p.current.value // describe name (not used for now)
	p.advance()

	if p.current.type_ != .do {
		return error('Expected "do" after describe name')
	}
	p.advance() // Skip 'do'

	// Skip newlines
	for p.current.type_ == .newline {
		p.advance()
	}

	mut test_functions := []ast.Node{}

	// Parse test blocks inside describe
	for p.current.type_ != .end && p.current.type_ != .eof {
		// Skip newlines
		for p.current.type_ == .newline {
			p.advance()
		}

		if p.current.type_ == .end {
			break
		}

		if p.current.type_ == .test {
			test_block := p.parse_test_block()!
			test_functions << test_block
		} else {
			return error('Only test blocks are allowed inside describe blocks')
		}

		// Skip newlines after test
		for p.current.type_ == .newline {
			p.advance()
		}
	}

	if p.current.type_ != .end {
		return error('Expected "end" to close describe block')
	}
	p.advance() // Skip 'end'

	return test_functions
}
