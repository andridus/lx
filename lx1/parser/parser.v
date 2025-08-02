module parser

import lexer
import ast
import errors
import kernel

pub struct Parser {
mut:
	lexer          lexer.Lexer
	current        lexer.Token
	next           lexer.Token
	error_reporter errors.ErrorReporter
	next_ast_id    int = 1
}

pub fn new_parser(code string, file_path string) Parser {
	mut l := lexer.new_lexer(code, file_path)
	mut p := Parser{
		lexer:          l
		error_reporter: errors.new_error_reporter()
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
		} else {
			p.error('Expected "def" or "record", got "${p.current.value}"')
			return error('Expected def or record')
		}
	}

	// Combine records and functions
	mut all_nodes := []ast.Node{}
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
		} else {
			p.error('Expected "def" or "record", got "${p.current.value}"')
			return error('Expected def or record')
		}
	}

	// Combine records and functions
	mut all_nodes := []ast.Node{}
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

	if p.current.type_ != .lparen {
		return p.error_and_return_with_suggestion('Function definition requires parentheses after name',
			'Add empty parentheses: ${func_name}()')
	}
	p.advance()

	if p.current.type_ != .rparen {
		return p.error_and_return_with_suggestion('Function parameters not supported in Task 1',
			'Use empty parentheses: ${func_name}()')
	}
	p.advance()

	if p.current.type_ != .do {
		return p.error_and_return_with_suggestion('Function definition requires "do" keyword',
			'Add "do" after parentheses: ${func_name}() do')
	}
	p.advance()

	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_block()!

	for p.current.type_ == .newline {
		p.advance()
	}
	p.advance()

	return ast.new_function(func_id, func_name, body, start_pos)
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
		// Stop if we encounter 'end' or other non-expression tokens
		if p.current.type_ == .end || p.current.type_ == .eof {
			break
		}

		// Skip newlines before expression
		for p.current.type_ == .newline {
			p.advance()
		}

		// Stop if we encounter 'end' after skipping newlines
		if p.current.type_ == .end || p.current.type_ == .eof {
			break
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
			if p.current.type_ != .end && p.current.type_ != .eof {
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
		if p.current.type_ == .identifier && p.is_infix_function(p.current.value) {
			function_info := kernel.get_function_info(p.current.value) or { break }
			if function_info.precedence < precedence {
				break
			}
			left = p.parse_infix_expression(left)!
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

fn (mut p Parser) parse_prefix_expression() !ast.Node {
	return match p.current.type_ {
		.integer, .float, .string, .true_, .false_, .atom, .nil_ { p.parse_literal() }
		.identifier { p.parse_identifier_expression() }
		.lparen { p.parse_parentheses() }
		.lbracket { p.parse_list_expression() }
		.lbrace { p.parse_tuple_expression() }
		.percent { p.parse_map_literal() }
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

			if p.current.type_ == .rbrace {
				break
			}

			if p.current.type_ != .comma {
				return p.error_and_return('Expected comma or closing brace')
			}

			p.advance() // Skip comma
		}
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
