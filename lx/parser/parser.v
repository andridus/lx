module parser

import lexer
import ast
import errors
import kernel
import os

// ===================================================================
// ================= Parser Structure and Core Functions ================
// ===================================================================

pub struct Parser {
	directives_table &DirectivesTable
mut:
	lexer                 lexer.Lexer
	current               lexer.Token
	next                  lexer.Token
	error_reporter        errors.ErrorReporter
	next_ast_id           int  = 1
	at_line_start         bool = true // Tracks if we are at the beginning of a new line
	temp_doc_node         ?ast.Node
	current_module_name   string
	in_supervisor_context bool // Tracks if we are inside a supervisor do...end block
}

// new_parser creates and initializes a new Parser.
pub fn new_parser(code string, file_path string, directives_table &DirectivesTable) Parser {
	mut l := lexer.new_lexer(code, file_path)
	// Derive the module name from the file path
	mut base := os.base(file_path)
	if idx := base.last_index('.') {
		base = base[..idx]
	}
	mut p := Parser{
		lexer:               l
		error_reporter:      errors.new_error_reporter()
		directives_table:    directives_table
		current_module_name: base
	}
	p.current = p.lexer.next_token()
	p.next = p.lexer.next_token()
	return p
}

// parse starts code analysis, assuming the module name 'main'.
pub fn (mut p Parser) parse() !ast.Node {
	return p.do_parse_module('main')
}

// parse_with_modname starts code analysis with a specified module name.
pub fn (mut p Parser) parse_with_modname(modname string) !ast.Node {
	return p.do_parse_module(modname)
}

// get_errors returns all errors accumulated during analysis.
pub fn (p Parser) get_errors() []errors.Err {
	return p.error_reporter.all()
}

// get_next_id returns a unique ID for the next AST node.
pub fn (mut p Parser) get_next_id() int {
	id := p.next_ast_id
	p.next_ast_id++
	return id
}

// advance consumes the current token and advances to the next.
fn (mut p Parser) advance() {
	if p.current.type_ == .newline {
		p.at_line_start = true
	} else if p.current.type_ != .newline {
		p.at_line_start = false
	}

	p.current = p.next
	p.next = p.lexer.next_token()
}

// ===================================================================
// ==================== Error Handling ==========================
// ===================================================================

fn (mut p Parser) error(msg string) {
	p.error_reporter.report(.parser, msg, p.current.position)
}

fn (mut p Parser) error_and_return(msg string) !ast.Node {
	p.error(msg)
	return error(msg)
}

fn (mut p Parser) error_and_return_with_suggestion(msg string, suggestion string) !ast.Node {
	p.error_reporter.report_with_suggestion(.parser, msg, p.current.position, suggestion)
	return error(msg)
}

// ===================================================================
// ==================== Top-Level Analysis (Module) ================
// ===================================================================

// do_parse_module is the unified function that analyzes the body of a module.
// REFACTORED: Unifies the logic of `parse_module` and `parse_module_with_name`.
fn (mut p Parser) do_parse_module(modname string) !ast.Node {
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
			return p.error_and_return('Lexical error: ${p.current.value}')
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
			.def, .defp {
				func := p.parse_function()!
				functions << func
			}
			.type {
				type_def := p.parse_type_def()!
				all_nodes << type_def
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
				functions << describe_blocks
			}
			.test {
				test_block := p.parse_test_block()!
				functions << test_block
			}
			else {
				return p.error_and_return('Expected a top-level construct, but found "${p.current.value}"')
			}
		}
	}

	all_nodes << records
	all_nodes << functions
	return ast.new_module(module_id, modname, all_nodes, start_pos)
}

// ===================================================================
// =============== Definition Analysis (Functions, etc) ================
// ===================================================================

fn (mut p Parser) parse_function() !ast.Node {
	is_private := p.current.type_ == .defp
	start_pos := p.current.position
	func_id := p.get_next_id()
	p.advance() // Skip 'def' or 'defp'

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected function name, but found "${p.current.value}"')
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
				arg := p.parse_expression()!
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

	mut return_type_annotation := ast.Node{}
	mut has_return_type := false
	if p.current.type_ == .double_colon {
		p.advance() // Skip ::
		return_type_annotation = p.parse_type_annotation()!
		has_return_type = true
	}

	if p.current.type_ != .do {
		return p.error_and_return_with_suggestion('Function definition requires the "do" keyword',
			'Add "do" after parentheses: ${func_name}() do')
	}
	p.advance()

	for p.current.type_ == .newline {
		p.advance()
	}

	mut body := ast.Node{}
	if has_parens {
		body = p.parse_block()!
	} else {
		// Multi-head function
		mut heads := []ast.Node{}
		for p.current.type_ != .end && p.current.type_ != .eof {
			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .lparen {
				heads << p.parse_function_head()!
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
		return p.error_and_return('Expected the "end" keyword')
	}
	p.advance()

	if node := p.temp_doc_node {
		p.directives_table.add_doc('${func_name}/${args.len}', node)
		p.temp_doc_node = none
	}

	mut fn_node := ast.new_function_with_params(func_id, func_name, args, body, start_pos)

	if has_return_type {
		fn_node.children << return_type_annotation
	}

	if is_private {
		fn_node.kind = .private_function
	}
	return fn_node
}

fn (mut p Parser) parse_function_head() !ast.Node {
	pos := p.current.position
	if p.current.type_ == .double_colon {
		p.advance()
		p.parse_type_annotation()!
	}
	config := ClauseConfig{
		require_parens:    true
		allow_guard:       true
		end_tokens:        [.end, .eof]
		pattern_end_token: .arrow
	}
	clause := p.parse_generic_clause(config)!
	args := clause.children[0]
	body := clause.children[1]
	mut children := [args, body]
	if clause.children.len > 2 {
		children << clause.children[2] // guard
	}
	return ast.Node{
		id:       p.get_next_id()
		kind:     .function
		value:    '' // anonymous function
		children: children
		position: pos
	}
}

// ===================================================================
// =================== Expression Analysis (Pratt Parser) ==========
// ===================================================================

fn (mut p Parser) parse_expression() !ast.Node {
	for p.current.type_ == .newline {
		p.advance()
	}
	return p.parse_expression_with_precedence(0)
}

fn (mut p Parser) parse_expression_with_precedence(precedence int) !ast.Node {
	mut left := p.parse_prefix_expression()!
	for {
		if p.current.type_ == .newline && p.next.type_ == .pipe_forward {
			p.advance()
			continue
		}
		if p.current.type_ == .eof {
			break
		}
		if p.current.type_ == .pipe_forward {
			left = p.parse_pipeline_expression(left)!
			continue
		}
		op_type := p.current.type_
		if (op_type == .identifier && p.is_infix_function(p.current.value))
			|| op_type == .exclamation || op_type == .slash || op_type == .in {
			op_name := match op_type {
				.exclamation { '!' }
				.slash { '/' }
				.in { 'in' }
				else { p.current.value }
			}
			function_info := kernel.get_function_info(op_name) or { break }
			if function_info.precedence < precedence {
				break
			}
			left = p.parse_infix_expression_with_name(left, op_name)!
			continue
		}

		if p.current.type_ == .lbracket {
			left = p.parse_map_access(left)!
			continue
		}
		if p.current.type_ == .dot {
			left = p.parse_record_access(left)!
			continue
		}
		if p.current.type_ == .bind {
			p.advance()
			right := p.parse_expression()!
			left = ast.new_pattern_binding(p.get_next_id(), left, right, left.position)
			continue
		}
		break
	}
	return left
}

fn (mut p Parser) parse_infix_expression_with_name(left ast.Node, name string) !ast.Node {
	pos := p.current.position
	p.advance()
	function_info := kernel.get_function_info(name) or { return error('Unknown function: ${name}') }
	right := p.parse_expression_with_precedence(function_info.precedence)!
	return ast.new_function_caller(p.get_next_id(), name, [left, right], pos)
}

fn (mut p Parser) parse_prefix_expression() !ast.Node {
	return match p.current.type_ {
		.integer, .float, .string, .charlist, .true_, .false_, .atom, .nil_ { p.parse_literal() }
		.identifier { p.parse_identifier_expression() }
		.module_token { p.parse_module_token() }
		.lparen { p.parse_parentheses() }
		.lbracket { p.parse_list_expression() }
		.lbrace { p.parse_tuple_expression() }
		.percent { p.parse_map_literal() }
		.case { p.parse_case_expression() }
		.fn { p.parse_lambda_expression() }
		.if_ { p.parse_if_expression() }
		.with { p.parse_with_expression() }
		.match { p.parse_match_expression() }
		.spawn { p.parse_spawn_expression() }
		.receive { p.parse_receive_expression() }
		.double_lt { p.parse_binary_literal() }
		.at_sign { p.parse_directive_new() }
		.for_ { p.parse_list_comprehension() }
		.assert { p.parse_assert_expression() }
		.def { p.parse_function() }
		else { error('Unexpected token: ${p.current.type_}') }
	}
}

// ===================================================================
// =========== Literal and Primary Expression Analysis ============
// ===================================================================

fn (mut p Parser) parse_literal() !ast.Node {
	pos := p.current.position
	lit_id := p.get_next_id()
	typ := p.current.type_
	val := p.current.value
	p.advance()

	return match typ {
		.integer {
			ast.new_integer(lit_id, val.int(), pos)
		}
		.float {
			ast.new_float(lit_id, val.f64(), pos)
		}
		.string {
			if val.contains('#{') {
				p.parse_string_interpolation(lit_id, val, pos)!
			} else {
				ast.new_string(lit_id, val, pos)
			}
		}
		.charlist {
			ast.new_charlist(lit_id, val, pos)
		}
		.true_ {
			ast.new_boolean(lit_id, true, pos)
		}
		.false_ {
			ast.new_boolean(lit_id, false, pos)
		}
		.atom {
			ast.new_atom(lit_id, val, pos)
		}
		.nil_ {
			ast.new_nil(lit_id, pos)
		}
		else {
			p.error_and_return('Expected a literal, but found "${val}"')!
		}
	}
}

fn (mut p Parser) parse_identifier_expression() !ast.Node {
	identifier := p.current.value
	pos := p.current.position
	p.advance()

	if identifier.starts_with('$') {
		return p.parse_directive_call(identifier, pos)
	}
	if identifier.starts_with('&') {
		return p.parse_function_capture(identifier, pos)
	}

	match p.current.type_ {
		.colon {
			return p.parse_external_function_call(identifier, pos)
		}
		.lparen {
			return p.parse_function_call(identifier, pos)
		}
		.lbrace {
			// It's a record literal: RecordName{...}
			return p.parse_record_literal(identifier, pos)
		}
		.dot {
			record_node := ast.new_identifier(p.get_next_id(), identifier, pos)
			return p.parse_record_access(record_node)
		}
		.bind {
			if p.in_supervisor_context && (identifier == 'strategy' || identifier == 'children') {
				return p.error_and_return('Supervisor directives should not use "=". Use "${identifier} :value"')
			}
			p.advance() // Skip '='
			value := p.parse_expression()!
			return ast.new_variable_binding(p.get_next_id(), identifier, value, pos)
		}
		.double_colon {
			p.advance() // Skip ::
			type_annotation := p.parse_type_annotation()!
			return ast.Node{
				id:       p.get_next_id()
				kind:     .type_annotation
				children: [
					ast.new_identifier(p.get_next_id(), identifier, pos),
					type_annotation,
				]
				position: pos
			}
		}
		else {
			// Prefix functions without parentheses (e.g., not, spawn)
			if p.is_single_arg_prefix_function(identifier) {
				arg := p.parse_expression()!
				return ast.new_function_caller(p.get_next_id(), identifier, [arg], pos)
			}
			// Special handling for supervisor directives without '='
			if p.in_supervisor_context && (identifier == 'strategy' || identifier == 'children') {
				value := p.parse_expression()!
				return ast.new_variable_binding(p.get_next_id(), identifier, value, pos)
			}
			// Just a variable reference
			return ast.new_variable_ref(p.get_next_id(), identifier, pos)
		}
	}
}

fn (mut p Parser) parse_parentheses() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '('
	expr := p.parse_expression()!
	if p.current.type_ != .rparen {
		return p.error_and_return('Expected closing parenthesis')
	}
	p.advance() // Skip ')'
	return ast.new_parentheses(p.get_next_id(), expr, pos)
}

fn (mut p Parser) parse_list_expression() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['
	if p.current.type_ == .rbracket {
		p.advance()
		return ast.new_list_literal(p.get_next_id(), [], pos)
	}
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ == .rbracket {
		p.advance()
		return ast.new_list_literal(p.get_next_id(), [], pos)
	}

	mut first_element := p.parse_expression()!
	if p.current.type_ == .pipe {
		p.advance() // Skip '|'
		tail := p.parse_expression()!
		if p.current.type_ != .rbracket {
			return p.error_and_return('Expected closing bracket')
		}
		p.advance()
		return ast.new_list_cons(p.get_next_id(), first_element, tail, pos)
	}

	mut elements := [first_element]
	for p.current.type_ == .comma {
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		elements << p.parse_expression()!
	}
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .rbracket {
		return p.error_and_return('Expected closing bracket')
	}
	p.advance()

	return ast.new_list_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_tuple_expression() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'
	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ == .rbrace {
		p.advance()
		return ast.new_tuple_literal(p.get_next_id(), [], pos)
	}

	mut elements := [p.parse_expression()!]
	for p.current.type_ == .comma {
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		elements << p.parse_expression()!
	}

	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing brace')
	}
	p.advance()

	return ast.new_tuple_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_map_literal() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '%'

	if p.current.type_ == .identifier {
		if p.next.type_ == .lbrace {
			return p.parse_record_update_with_name()
		}
	}

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after %')
	}
	p.advance() // Skip '{'

	if p.current.type_ == .pipe {
		return p.parse_record_update()
	}

	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ == .rbrace {
		p.advance() // Skip '}'
		return ast.new_map_literal(p.get_next_id(), [], pos)
	}

	mut entries := []ast.Node{}

	for {
		key := p.parse_map_key()!

		if p.current.type_ != .colon {
			return error('Expected colon after map key')
		}
		p.advance() // Skip ':'

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
		for p.current.type_ == .newline {
			p.advance()
		}
	}

	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}
	p.advance()

	return ast.new_map_literal(p.get_next_id(), entries, pos)
}

fn (mut p Parser) parse_map_key() !ast.Node {
	if p.current.type_ == .identifier && p.next.type_ == .colon {
		atom_name := p.current.value
		pos := p.current.position
		p.advance()
		return ast.new_atom(p.get_next_id(), atom_name, pos)
	}
	return p.parse_expression()
}

fn (mut p Parser) parse_map_access(map_expr ast.Node) !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['
	key_expr := p.parse_expression()!
	if p.current.type_ != .rbracket {
		return p.error_and_return('Expected closing bracket for map access')
	}
	p.advance() // Skip ']'
	return ast.new_map_access(p.get_next_id(), map_expr, key_expr, pos)
}

fn (mut p Parser) parse_module_token() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '__MODULE__'
	return ast.new_atom(p.get_next_id(), p.current_module_name, pos)
}

fn (mut p Parser) parse_string_interpolation(id int, value string, pos ast.Position) !ast.Node {
	mut segments := []ast.Node{}
	mut current_pos := 0

	for current_pos < value.len {
		interp_start_opt := value.index_after('#{', current_pos)
		if interp_start_opt == none {
			if current_pos < value.len {
				remaining := value[current_pos..]
				if remaining.len > 0 {
					segments << ast.new_string(p.get_next_id(), remaining, pos)
				}
			}
			break
		}
		interp_start := interp_start_opt or { break }
		if interp_start > current_pos {
			before := value[current_pos..interp_start]
			if before.len > 0 {
				segments << ast.new_string(p.get_next_id(), before, pos)
			}
		}
		interp_end_opt := value.index_after('}', interp_start + 2)
		if interp_end_opt == none {
			return p.error_and_return('Unclosed string interpolation')
		}
		interp_end := interp_end_opt or {
			return p.error_and_return('Unclosed string interpolation')
		}

		expr_str := value[interp_start + 2..interp_end]
		if expr_str.len > 0 {
			segments << ast.new_variable_ref(p.get_next_id(), expr_str, pos)
		}
		current_pos = interp_end + 1
	}

	return ast.new_string_interpolation(id, segments, pos)
}

// ===================================================================
// ==================== Block Analysis ============================
// ===================================================================

fn (mut p Parser) parse_block() !ast.Node {
	mut expressions := []ast.Node{}
	start_pos := p.current.position
	for p.current.type_ == .newline {
		p.advance()
	}

	for p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
		if p.current.type_ == .match {
			match_expr := p.parse_match_expression()!
			expressions << match_expr
			break
		}
		if p.current.type_ == .def {
			func := p.parse_function()!
			expressions << func
			continue
		}

		expr := p.parse_expression()!
		expressions << expr

		// Consome separadores
		if p.current.type_ == .semicolon {
			p.advance()
		} else if p.current.type_ == .newline {
			p.advance()
		} else if p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
			// If it's not a separator or a final token, it might be an error or an implicit end of the block.
			// We continue to the next iteration so that the loop condition can reevaluate.
			continue
		}
	}
	return ast.new_block(p.get_next_id(), expressions, start_pos)
}

// ===================================================================
// =========== Function and Operator Call Analysis ============
// ===================================================================

// do_parse_parenthesized_argument_list is a helper function to parse lists of arguments
// between parentheses, eliminating code duplication.
// REFACTORED: Centralizes the logic of parsing `(arg1, arg2, ...)`
fn (mut p Parser) do_parse_parenthesized_argument_list() ![]ast.Node {
	mut arguments := []ast.Node{}
	p.advance() // Skip '('

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

	return arguments
}

fn (mut p Parser) parse_function_call(function_name string, pos ast.Position) !ast.Node {
	arguments := p.do_parse_parenthesized_argument_list()!
	return ast.new_function_caller(p.get_next_id(), function_name, arguments, pos)
}

fn (mut p Parser) parse_directive_call(directive_name string, pos ast.Position) !ast.Node {
	actual_name := directive_name[1..]
	if !p.is_valid_directive(actual_name) {
		return p.error_and_return('Unknown directive: ${directive_name}')
	}
	if p.current.type_ != .lparen {
		return p.error_and_return('Directive ${directive_name} requires parentheses')
	}
	arguments := p.do_parse_parenthesized_argument_list()!
	return ast.new_directive_call(p.get_next_id(), actual_name, arguments, pos)
}

fn (mut p Parser) parse_external_function_call(module_name string, pos ast.Position) !ast.Node {
	p.advance() // Skip ':'

	if p.current.type_ !in [.identifier, .charlist] {
		return p.error_and_return('Expected function name after ":"')
	}

	mut function_name := p.current.value
	if p.current.type_ == .charlist {
		function_name = '\'${function_name}\''
	}
	p.advance()
	if p.current.type_ != .lparen {
		return p.error_and_return('Expected "(" after function name')
	}
	arguments := p.do_parse_parenthesized_argument_list()!
	resolved_module := if module_name == '_' { p.current_module_name } else { module_name }
	return ast.new_external_function_call(p.get_next_id(), resolved_module, function_name,
		arguments, pos)
}

fn (mut p Parser) parse_lambda_call(lambda ast.Node) !ast.Node {
	if p.current.type_ != .lparen {
		return p.error_and_return('Expected "(" for lambda call')
	}
	pos := p.current.position
	args := p.do_parse_parenthesized_argument_list()!
	return ast.new_lambda_call(p.get_next_id(), lambda, args, pos)
}

fn (mut p Parser) parse_pipeline_expression(left ast.Node) !ast.Node {
	pos := p.current.position
	p.advance() // Skip '|>'
	for p.current.type_ == .newline {
		p.advance()
	}
	right := p.parse_prefix_expression()!

	if left.kind == .variable_binding {
		bound_value := left.children[0]
		injected := p.inject_into_call(right, bound_value, pos)!
		return ast.new_variable_binding(p.get_next_id(), left.value, injected, left.position)
	}

	return p.inject_into_call(right, left, pos)
}

fn (mut p Parser) inject_into_call(call_node ast.Node, left ast.Node, pos ast.Position) !ast.Node {
	mut target := call_node
	// Expand identifier to function call with zero arguments
	if target.kind == .variable_ref || target.kind == .identifier {
		target = ast.new_function_caller(p.get_next_id(), target.value, [], target.position)
	}

	mut args := if target.kind == .lambda_call {
		target.children[1..].clone()
	} else {
		target.children.clone()
	}
	mut replaced := false
	for i in 0 .. args.len {
		if args[i].kind == .variable_ref && args[i].value == '_' {
			args[i] = left
			replaced = true
			break
		}
	}
	if !replaced {
		args.prepend(left)
	}

	return match target.kind {
		.external_function_call {
			module_part := target.value.split(':')[0]
			func_part := target.value.split(':')[1]
			ast.new_external_function_call(p.get_next_id(), module_part, func_part, args,
				pos)
		}
		.function_caller {
			ast.new_function_caller(p.get_next_id(), target.value, args, pos)
		}
		.lambda_call {
			ast.new_lambda_call(p.get_next_id(), target.children[0], args, pos)
		}
		else {
			// Forces a call: right(left)
			ast.new_function_caller(p.get_next_id(), target.value, [left], pos)
		}
	}
}

// ===================================================================
// ==================== Record Analysis ===========================
// ===================================================================

fn (mut p Parser) parse_record_definition() !ast.Node {
	pos := p.current.position
	p.advance() // Skip 'record'
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected record name')
	}
	record_name := p.current.value
	p.advance()
	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected "{" after record name')
	}
	p.advance()
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
			p.advance()

			mut default_value := ast.Node{}
			mut has_default := false
			mut field_type_node := ast.Node{}

			if p.current.type_ == .bind {
				p.advance() // Skip '='
				default_value = p.parse_expression()!
				has_default = true
				if p.current.type_ == .double_colon {
					p.advance()
					field_type_node = p.parse_type_expression()!
				}
			} else {
				if p.current.type_ != .double_colon {
					return p.error_and_return_with_suggestion('Expected "::" after field name',
						'Add a type annotation: ${field_name} :: integer')
				}
				p.advance()
				field_type_node = p.parse_type_expression()!
			}

			field_node := if has_default {
				ast.new_record_field(p.get_next_id(), field_name, field_type_node, default_value,
					pos)
			} else {
				ast.new_record_field_without_default(p.get_next_id(), field_name, field_type_node,
					pos)
			}
			fields << field_node

			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .rbrace {
				break
			}
			if p.current.type_ == .comma {
				p.advance()
				for p.current.type_ == .newline {
					p.advance()
				}
				continue
			}
			if p.current.type_ != .identifier {
				return p.error_and_return('Expected comma or "}"')
			}
		}
	}
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected "}"')
	}
	p.advance()
	return ast.new_record_definition(p.get_next_id(), record_name, fields, pos)
}

// parse_record_literal is the unified function to parse record literals.
// REFACTORED: Unifies `parse_record_literal` and `parse_record_literal_with_name`.
fn (mut p Parser) parse_record_literal(record_name string, pos ast.Position) !ast.Node {
	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected "{" after record name')
	}
	p.advance() // Skip '{'
	mut field_values := []ast.Node{}
	if p.current.type_ != .rbrace {
		for {
			if p.current.type_ != .identifier {
				return p.error_and_return('Expected field name')
			}
			field_name := p.current.value
			p.advance()
			if p.current.type_ != .colon {
				return p.error_and_return_with_suggestion('Expected ":" after field name',
					'Syntax: field_name: value')
			}
			p.advance() // Skip ':'
			field_value := p.parse_expression()!
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
				return p.error_and_return('Expected comma or "}"')
			}
			p.advance() // Skip comma
		}
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected "}"')
	}
	p.advance() // Skip '}'
	return ast.new_record_literal(p.get_next_id(), record_name, field_values, pos)
}

fn (mut p Parser) parse_record_access(node ast.Node) !ast.Node {
	if p.current.type_ != .dot {
		return error('Expected "." for record field access')
	}
	p.advance() // Skip '.'
	if p.current.type_ == .lparen {
		return p.parse_lambda_call(node)!
	}
	if p.current.type_ != .identifier {
		return error('Expected field name after "."')
	}
	field_name := p.current.value
	p.advance()
	return ast.new_record_access(p.get_next_id(), node, field_name, node.position)
}

fn (mut p Parser) parse_record_update() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '%'

	if p.current.type_ != .lbrace {
		return error('Expected opening brace after %')
	}
	p.advance() // Skip '{'

	record_expr := p.parse_expression()!

	if p.current.type_ != .pipe {
		return error('Expected | after record expression')
	}
	p.advance() // Skip '|'

	if p.current.type_ != .identifier {
		return error('Expected field name after |')
	}
	field_name := p.current.value
	p.advance()

	if p.current.type_ != .colon {
		return error('Expected colon after field name')
	}
	p.advance() // Skip ':'

	field_value := p.parse_expression()!

	if p.current.type_ != .rbrace {
		return error('Expected closing brace')
	}
	p.advance()

	return ast.new_record_update(p.get_next_id(), 'unknown', record_expr, field_name,
		field_value, pos)
}

fn (mut p Parser) parse_record_update_with_name() !ast.Node {
	pos := p.current.position
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected record name after %')
	}
	record_name := p.current.value
	p.advance()

	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected opening brace after record name')
	}
	p.advance()

	record_expr := p.parse_expression()!

	if p.current.type_ != .pipe {
		return p.error_and_return('Expected | after record expression')
	}
	p.advance()

	mut field_updates := []ast.Node{}
	for {
		if p.current.type_ != .identifier {
			return p.error_and_return('Expected field name after |')
		}
		field_name := p.current.value
		p.advance()

		if p.current.type_ != .colon {
			return p.error_and_return_with_suggestion('Expected colon after field name',
				'Record update syntax: field_name: value')
		}
		p.advance()

		field_value := p.parse_expression()!
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
			return p.error_and_return('Expected comma or closing brace')
		}
		p.advance()
	}

	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing brace')
	}
	p.advance()

	if field_updates.len == 0 {
		return p.error_and_return('Expected at least one field update')
	}

	// TODO: Support multiple updates
	first_field := field_updates[0]
	return ast.new_record_update(p.get_next_id(), record_name, record_expr, first_field.value,
		first_field.children[0], pos)
}

// ===================================================================
// ==================== Type System Analysis ==================
// ===================================================================

fn (mut p Parser) parse_type_def() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'type'

	mut is_opaque := false
	mut is_nominal := false
	if p.current.type_ == .identifier && p.current.value == 'opaque' {
		is_opaque = true
		p.advance()
	}
	if p.current.type_ == .identifier && p.current.value == 'nominal' {
		is_nominal = true
		p.advance()
	}

	if p.current.type_ != .identifier {
		return p.error_and_return('Expected type name')
	}
	type_name := p.current.value
	p.advance()

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
				p.advance()
			}
		}
		if p.current.type_ != .rparen {
			return p.error_and_return('Expected closing parenthesis')
		}
		p.advance()
	}

	if p.current.type_ != .double_colon {
		return p.error_and_return('Expected "::" after type name')
	}
	p.advance()

	type_def := p.parse_type_expression()!

	full_type_name := if params.len > 0 { '${type_name}(${params.join(', ')})' } else { type_name }

	if is_opaque {
		return ast.new_opaque_type(p.get_next_id(), full_type_name, type_def, start_pos)
	}
	if is_nominal {
		return ast.new_nominal_type(p.get_next_id(), full_type_name, type_def, start_pos)
	}
	return ast.new_type_def(p.get_next_id(), full_type_name, [type_def], start_pos)
}

fn (mut p Parser) parse_type_annotation() !ast.Node {
	return p.parse_type_expression()
}

fn (mut p Parser) parse_type_expression() !ast.Node {
	return p.parse_union_type()
}

fn (mut p Parser) parse_union_type() !ast.Node {
	mut variants := [p.parse_single_type()!]
	for p.current.type_ == .pipe {
		p.advance()
		variants << p.parse_single_type()!
	}
	if variants.len == 1 {
		return variants[0]
	}
	return ast.new_union_type(p.get_next_id(), variants, variants[0].position)
}

fn (mut p Parser) parse_single_type() !ast.Node {
	pos := p.current.position
	match p.current.type_ {
		.atom {
			val := p.current.value
			p.advance()
			return ast.new_atom(p.get_next_id(), val, pos)
		}
		.lbracket {
			p.advance()
			if p.current.type_ != .rbracket {
				return p.error_and_return('Expected "]" for empty list type')
			}
			p.advance()
			return ast.new_list_literal(p.get_next_id(), [], pos)
		}
		.lbrace {
			return p.parse_tuple_type()
		}
		.lparen {
			p.advance()
			inner_type := p.parse_union_type()!
			if p.current.type_ != .rparen {
				return p.error_and_return('Expected ")" after type')
			}
			p.advance()
			return inner_type
		}
		.identifier {
			type_name := p.current.value
			p.advance()
			if p.current.type_ == .lparen {
				p.advance()
				mut params := []ast.Node{}
				if p.current.type_ != .rparen {
					for {
						params << p.parse_union_type()!
						if p.current.type_ == .rparen {
							break
						}
						if p.current.type_ != .comma {
							return p.error_and_return('Expected comma or ")" in type parameters')
						}
						p.advance()
					}
				}
				if p.current.type_ != .rparen {
					return p.error_and_return('Expected ")" in type expression')
				}
				p.advance()
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
		else {
			return p.error_and_return('Expected type name, atom, tuple, or list')
		}
	}
}

fn (mut p Parser) parse_tuple_type() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'
	mut elements := []ast.Node{}
	if p.current.type_ != .rbrace {
		for {
			elements << p.parse_union_type()!
			if p.current.type_ == .rbrace {
				break
			}
			if p.current.type_ != .comma {
				return p.error_and_return('Expected comma or "}" in tuple type')
			}
			p.advance()
		}
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected "}" in tuple type')
	}
	p.advance()
	return ast.new_tuple_literal(p.get_next_id(), elements, pos)
}

// ===================================================================
// ======== Pattern Matching and Generic Clauses Analysis ========
// ===================================================================

struct ClauseConfig {
	require_parens    bool
	allow_guard       bool
	end_tokens        []lexer.TokenType
	pattern_end_token lexer.TokenType
}

fn (mut p Parser) parse_generic_clause(config ClauseConfig) !ast.Node {
	start_pos := p.current.position
	mut pattern := ast.Node{}
	if config.require_parens {
		if p.current.type_ != .lparen {
			return p.error_and_return('Expected "(" for pattern')
		}
		p.advance()
		mut patterns := []ast.Node{}
		if p.current.type_ != .rparen {
			for {
				patterns << p.parse_expression()!
				if p.current.type_ == .rparen {
					break
				}
				if p.current.type_ != .comma {
					return p.error_and_return("Expected ',' or ')'")
				}
				p.advance()
			}
		}

		pattern = ast.new_block(p.get_next_id(), patterns, start_pos)
		if p.current.type_ != .rparen {
			return p.error_and_return('Expected ")" for pattern')
		}
		p.advance()
	} else {
		pattern = p.parse_pattern()!
	}
	mut guard := ast.Node{}
	if config.allow_guard && p.current.type_ == .when {
		p.advance()
		guard = p.parse_expression()!
	}
	if p.current.type_ != config.pattern_end_token {
		return p.error_and_return('Expected "${config.pattern_end_token}" after pattern')
	}
	p.advance()

	for p.current.type_ == .newline {
		p.advance()
	}
	mut tokens_ending := config.end_tokens.clone()
	tokens_ending << config.pattern_end_token
	body := p.parse_clause_body(tokens_ending)!

	if guard.id != 0 {
		return ast.new_case_clause_with_guard(p.get_next_id(), pattern, guard, body, start_pos)
	}
	return ast.new_case_clause(p.get_next_id(), pattern, body, start_pos)
}

fn (mut p Parser) parse_generic_clauses(config ClauseConfig) ![]ast.Node {
	mut clauses := []ast.Node{}
	for p.current.type_ !in config.end_tokens && p.current.type_ != .eof {
		for p.current.type_ == .newline {
			p.advance()
		}
		if p.current.type_ in config.end_tokens {
			break
		}
		clauses << p.parse_generic_clause(config)!
		if p.current.type_ == .semicolon {
			p.advance()
		}
	}
	return clauses
}

fn (mut p Parser) parse_clause_body(end_tokens []lexer.TokenType) !ast.Node {
	mut expressions := []ast.Node{}
	start_pos := p.current.position

	for {
		if p.current.type_ in end_tokens || p.current.type_ == .eof {
			break
		}
		if p.at_line_start && p.looks_like_clause_pattern() {
			break
		}
		for p.current.type_ == .newline {
			p.advance()
		}
		if p.current.type_ in end_tokens || p.current.type_ == .eof {
			break
		}
		if p.at_line_start && p.looks_like_clause_pattern() {
			break
		}

		expressions << p.parse_expression()!
		if p.current.type_ == .semicolon || p.current.type_ == .newline {
			p.advance()
		} else {
			if p.current.type_ in end_tokens || p.current.type_ == .eof
				|| p.looks_like_clause_pattern() {
				// We advance here because the outer loop condition in `parse_generic_clauses` might miss an implicit end if we don't.
				// This handles cases like `... -> expr end` where `expr` is not followed by a newline or semicolon.
				// By advancing, the next token will be `end`, and the outer loop will correctly terminate.
				p.advance()
				break
			}
		}
	}

	if expressions.len == 1 {
		return expressions[0]
	}
	return ast.new_block(p.get_next_id(), expressions, start_pos)
}

fn (mut p Parser) parse_pattern() !ast.Node {
	return p.parse_expression() // In most cases, a pattern is parsed as an expression
}

fn (mut p Parser) parse_list_pattern() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '['
	if p.current.type_ == .rbracket {
		p.advance()
		return ast.new_list_literal(p.get_next_id(), [], pos)
	}
	first_element := p.parse_pattern()!
	if p.current.type_ == .pipe {
		p.advance()
		tail := p.parse_pattern()!
		if p.current.type_ != .rbracket {
			return p.error_and_return('Expected "]" after list cons pattern')
		}
		p.advance()
		return ast.new_list_cons(p.get_next_id(), first_element, tail, pos)
	}
	mut elements := [first_element]
	for p.current.type_ == .comma {
		p.advance()
		elements << p.parse_pattern()!
	}
	if p.current.type_ != .rbracket {
		return p.error_and_return('Expected "]" to close list pattern')
	}
	p.advance()
	return ast.new_list_literal(p.get_next_id(), elements, pos)
}

fn (mut p Parser) parse_tuple_pattern() !ast.Node {
	pos := p.current.position
	p.advance() // Skip '{'
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ == .rbrace {
		p.advance()
		return ast.new_tuple_literal(p.get_next_id(), [], pos)
	}
	mut patterns := [p.parse_pattern()!]
	for p.current.type_ == .comma {
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		patterns << p.parse_pattern()!
	}
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing brace in tuple pattern')
	}
	p.advance()
	return ast.new_tuple_literal(p.get_next_id(), patterns, pos)
}

// ===================================================================
// ==================== Control Flow Analysis =================
// ===================================================================

fn (mut p Parser) parse_if_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'if'
	condition := p.parse_expression()!
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after if condition')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	then_expr := p.parse_block()!
	mut else_expr := ?ast.Node(none)
	if p.current.type_ == .else_ {
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		else_expr = p.parse_block()!
	}
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close if expression')
	}
	p.advance()
	return ast.new_if_expr(p.get_next_id(), condition, then_expr, else_expr, start_pos)
}

fn (mut p Parser) parse_case_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'case'
	expr := p.parse_expression()!
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after case expression')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	config := ClauseConfig{
		require_parens:    false
		allow_guard:       true
		end_tokens:        [.end, .eof]
		pattern_end_token: .arrow
	}
	clauses := p.parse_generic_clauses(config)!
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close case expression')
	}
	p.advance()
	return ast.new_case_expression(p.get_next_id(), expr, clauses, start_pos)
}

fn (mut p Parser) parse_with_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'with'
	mut clauses := []ast.Node{}
	for {
		pattern := p.parse_expression()!
		if p.current.type_ != .left_arrow {
			return p.error_and_return('Expected "<-" after with pattern')
		}
		p.advance()
		expr := p.parse_expression()!
		clauses << ast.Node{
			id:       p.get_next_id()
			kind:     .pattern_match
			children: [pattern, expr]
			position: start_pos
		}
		if p.current.type_ == .comma {
			p.advance()
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
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	body := p.parse_block()!
	mut else_body := ?ast.Node(none)
	if p.current.type_ == .else_ {
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		if p.looks_like_case_clause() {
			else_body = p.parse_case_clauses_as_block()!
		} else {
			else_body = p.parse_block()!
		}
	}
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close with expression')
	}
	p.advance()
	return ast.new_with_expr_multi(p.get_next_id(), clauses, body, else_body, start_pos)
}

fn (mut p Parser) parse_match_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'match'
	pattern := p.parse_expression()!
	if p.current.type_ != .left_arrow {
		return p.error_and_return('Expected "<-" after match pattern')
	}
	p.advance()
	expr := p.parse_expression()!

	mut continuation_exprs := []ast.Node{}
	mut rescue_body := ?ast.Node(none)

	if p.current.type_ == .rescue {
		p.advance() // Skip 'rescue'
		error_pattern := p.parse_expression()!
		if p.current.type_ != .do {
			return p.error_and_return('Expected "do" after rescue pattern')
		}
		p.advance()
		for p.current.type_ == .newline {
			p.advance()
		}
		rescue_body_expr := p.parse_block()!
		rescue_body = ast.Node{
			id:       p.get_next_id()
			kind:     .tuple_literal
			children: [error_pattern, rescue_body_expr]
			position: start_pos
		}
		if p.current.type_ != .end {
			return p.error_and_return('Expected "end" to close match rescue expression')
		}
		p.advance()
	}

	if p.current.type_ == .semicolon {
		p.advance()
	}
	for p.current.type_ == .newline {
		p.advance()
	}

	for p.current.type_ != .end && p.current.type_ != .eof && p.current.type_ != .else_ {
		if p.current.type_ == .match {
			continuation_exprs << p.parse_match_expression()!
			break
		}
		continuation_exprs << p.parse_expression()!
		if p.current.type_ == .semicolon || p.current.type_ == .newline {
			p.advance()
		} else {
			break
		}
	}
	pattern_match := ast.new_pattern_match_with_expr(p.get_next_id(), pattern, expr, start_pos)

	if continuation_exprs.len > 0 {
		continuation_block := ast.new_block(p.get_next_id(), continuation_exprs, start_pos)
		return ast.new_match_expr(p.get_next_id(), pattern_match, continuation_block,
			rescue_body, start_pos)
	}
	continuation_block := ast.new_block(p.get_next_id(), [expr], start_pos)
	return ast.new_match_expr(p.get_next_id(), pattern_match, continuation_block, rescue_body,
		start_pos)
}

fn (mut p Parser) looks_like_case_clause() bool {
	return p.current.type_ != .end && (p.current.type_ == .identifier
		|| p.current.type_ == .lbrace || p.current.type_ == .lbracket
		|| p.current.type_ == .integer || p.current.type_ == .string
		|| p.current.type_ == .atom)
}

fn (mut p Parser) parse_case_clauses_as_block() !ast.Node {
	start_pos := p.current.position
	config := ClauseConfig{
		require_parens:    false
		allow_guard:       true
		end_tokens:        [.end, .eof]
		pattern_end_token: .arrow
	}
	clauses := p.parse_generic_clauses(config)!
	dummy_expr := ast.new_identifier(p.get_next_id(), 'Error', start_pos)
	return ast.new_case_expression(p.get_next_id(), dummy_expr, clauses, start_pos)
}

// ===================================================================
// ===================== Concurrency Analysis =====================
// ===================================================================

fn (mut p Parser) parse_spawn_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'spawn'
	if p.current.type_ != .lparen {
		return p.error_and_return('Expected "(" after spawn')
	}
	p.advance()
	func_expr := p.parse_expression()!
	if p.current.type_ != .rparen {
		return p.error_and_return('Expected ")" after spawn expression')
	}
	p.advance()
	return ast.new_spawn_expr(p.get_next_id(), func_expr, start_pos)
}

fn (mut p Parser) parse_receive_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'receive'
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after receive')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	config := ClauseConfig{
		require_parens:    false
		allow_guard:       false
		end_tokens:        [.end, .eof]
		pattern_end_token: .arrow
	}
	clauses := p.parse_generic_clauses(config)!
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close receive expression')
	}
	p.advance()
	return ast.new_receive_expr(p.get_next_id(), clauses, start_pos)
}

// ===================================================================
// =============== Binary and Comprehension Analysis ================
// ===================================================================

fn (mut p Parser) parse_lambda_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'fn'
	mut params := []ast.Node{}
	mut body := ast.Node{}
	mut expect_end := false

	if p.current.type_ == .do {
		// Multi-head lambda: fn do (p1) -> b1; (p2) -> b2 end
		p.advance() // Skip 'do'
		for p.current.type_ == .newline {
			p.advance()
		}
		config := ClauseConfig{
			require_parens:    true
			allow_guard:       true
			end_tokens:        [.end, .eof]
			pattern_end_token: .arrow
		}
		heads := p.parse_generic_clauses(config)!
		body = ast.new_block(p.get_next_id(), heads, heads[0].position)
		if p.current.type_ != .end {
			return p.error_and_return("Expected 'end' to close multi-head lambda")
		}
		p.advance()
	} else {
		// Single-head lambda: fn (a, b) -> a + b
		if p.current.type_ != .lparen {
			return p.error_and_return("Expected '(' or 'do' after 'fn'")
		}
		p.advance() // Skip '('
		if p.current.type_ != .rparen {
			for {
				expr := p.parse_expression()!
				if expr.kind == .variable_ref {
					params << ast.Node{
						...expr
						kind: .identifier
					}
				} else {
					params << expr
				}
				if p.current.type_ == .rparen {
					break
				}
				if p.current.type_ != .comma {
					return p.error_and_return("Expected ',' or ')' in lambda params")
				}
				p.advance()
			}
		}
		if p.current.type_ != .rparen {
			return p.error_and_return("Expected ')' in lambda params")
		}
		p.advance() // Skip ')'

		if p.current.type_ != .arrow && p.current.type_ != .do {
			return p.error_and_return("Expected '->' or 'do' after lambda parameters")
		}
		expect_end = p.current.type_ == .do
		p.advance() // Skip '->'

		if expect_end {
			body = p.parse_block()!
			if p.current.type_ != .end {
				return p.error_and_return("Expected 'end' to close lambda")
			}
			p.advance()
		} else {
			body = p.parse_expression()!
		}
	}
	return ast.new_lambda_expression(p.get_next_id(), params, body, start_pos)
}

fn (mut p Parser) parse_binary_literal() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip '<<'
	for p.current.type_ == .newline {
		p.advance()
	}
	mut segments := []ast.Node{}
	if p.current.type_ != .double_gt {
		for {
			for p.current.type_ == .newline {
				p.advance()
			}
			segments << p.parse_binary_segment()!
			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .comma {
				p.advance()
				for p.current.type_ == .newline {
					p.advance()
				}
			} else {
				break
			}
		}
	}
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .double_gt {
		return p.error_and_return('Expected ">>" to close binary literal')
	}
	p.advance()
	return ast.new_binary_literal(p.get_next_id(), segments, start_pos)
}

fn (mut p Parser) parse_binary_segment() !ast.Node {
	start_pos := p.current.position
	value := p.parse_binary_expression()!
	mut size := ?ast.Node(none)
	mut options := []string{}
	if p.current.type_ == .colon {
		p.advance()
		size = p.parse_binary_expression()!
	}
	if p.current.type_ == .slash {
		p.advance()
		if p.current.type_ != .identifier {
			return p.error_and_return('Invalid binary segment option: expected identifier after /')
		}
		options << p.current.value
		p.advance()
		for p.current.type_ == .identifier && p.current.value == '-' {
			p.advance()
			options << p.current.value
			p.advance()
		}
	}
	return ast.new_binary_segment(p.get_next_id(), value, size, options, start_pos)
}

fn (mut p Parser) parse_binary_expression() !ast.Node {
	return match p.current.type_ {
		.integer, .float, .string, .true_, .false_, .nil_, .atom {
			p.parse_literal()
		}
		.identifier {
			identifier := p.current.value
			pos := p.current.position
			p.advance()
			if p.current.type_ == .lparen {
				p.parse_function_call(identifier, pos)
			} else {
				ast.new_identifier(p.get_next_id(), identifier, pos)
			}
		}
		.lparen {
			p.advance()
			expr := p.parse_expression()!
			if p.current.type_ != .rparen {
				return p.error_and_return('Expected ")" after expression')
			}
			p.advance()
			expr
		}
		else {
			p.error_and_return('Invalid expression in binary segment')
		}
	}
}

fn (mut p Parser) parse_binary_pattern() !ast.Node {
	// Pending implementation
	return p.error_and_return('Binary pattern parsing not implemented yet')
}

fn (mut p Parser) parse_list_comprehension() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'for'
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected variable name after "for"')
	}
	var_name := p.current.value
	var_pos := p.current.position
	p.advance()
	if p.current.type_ != .in {
		return p.error_and_return('Expected "in" after variable name')
	}
	p.advance()
	list_expr := p.parse_expression()!
	mut condition_expr := ast.Node{}
	mut has_condition := false
	if p.current.type_ == .when {
		p.advance()
		condition_expr = p.parse_expression()!
		has_condition = true
	}
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after list expression')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	body_expr := p.parse_expression()!
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close list comprehension')
	}
	p.advance()
	var_node := ast.new_variable_ref(p.get_next_id(), var_name, var_pos)
	mut children := [var_node, list_expr, body_expr]
	if has_condition {
		children << condition_expr
	}
	return ast.new_list_comprehension(p.get_next_id(), children, start_pos)
}

fn (mut p Parser) parse_function_capture(initial string, pos ast.Position) !ast.Node {
	if initial.contains('/') && !initial.contains(':') {
		parts := initial[1..].split('/')
		if parts.len != 2 {
			return p.error_and_return('Invalid function capture syntax')
		}
		function_name := parts[0]
		arity := parts[1].int()
		return p.build_anonymous_function_capture(function_name, arity, '', pos)
	}

	module_with_amp := initial[1..]
	module_name := if module_with_amp == '_' { p.current_module_name } else { module_with_amp }
	if p.current.type_ != .colon {
		return p.error_and_return('Expected ":" after module name in function capture')
	}
	p.advance()
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected function/arity after module name in function capture')
	}
	fn_token_val := p.current.value
	p.advance()

	mut function_name := ''
	mut arity := -1

	if fn_token_val.contains('/') {
		parts := fn_token_val.split('/')
		if parts.len != 2 {
			return p.error_and_return('Invalid function capture syntax')
		}
		function_name = parts[0]
		arity = parts[1].int()
	} else if p.current.type_ == .slash {
		function_name = fn_token_val
		p.advance()
		if p.current.type_ != .integer {
			return p.error_and_return('Expected integer arity after "/" in function capture')
		}
		arity = p.current.value.int()
		p.advance()
	} else {
		return p.error_and_return('Expected "/" with arity in function capture')
	}

	return p.build_anonymous_function_capture(function_name, arity, module_name, pos)
}

fn (mut p Parser) build_anonymous_function_capture(func_name string, arity int, module_name string, pos ast.Position) !ast.Node {
	if arity < 0 {
		return p.error_and_return('Invalid arity in function capture')
	}
	mut params := []ast.Node{}
	mut args := []ast.Node{}
	for i in 0 .. arity {
		param_name := 'arg${i + 1}'
		params << ast.new_identifier(p.get_next_id(), param_name, pos)
		args << ast.new_variable_ref(p.get_next_id(), param_name, pos)
	}

	body := if module_name == '' {
		ast.new_function_caller(p.get_next_id(), func_name, args, pos)
	} else {
		ast.new_external_function_call(p.get_next_id(), module_name, func_name, args,
			pos)
	}
	return ast.new_anonymous_function(p.get_next_id(), params, body, pos)
}

// ===================================================================
// ================= Module System Analysis (OTP) =============
// ===================================================================

fn (mut p Parser) parse_application_config() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'application'
	if p.current.type_ != .lbrace {
		return p.error_and_return('Expected "{" after application')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	mut entries := []ast.Node{}
	if p.current.type_ != .rbrace {
		for {
			if p.current.type_ != .identifier && p.current.type_ != .deps {
				return p.error_and_return('Expected key identifier in application block')
			}
			key_name := p.current.value
			key_pos := p.current.position
			p.advance()
			if p.current.type_ != .colon {
				return p.error_and_return('Expected colon after application key')
			}
			p.advance()
			value_expr := p.parse_expression()!
			entries << ast.new_atom(p.get_next_id(), key_name, key_pos)
			entries << value_expr
			for p.current.type_ == .newline {
				p.advance()
			}
			if p.current.type_ == .rbrace {
				break
			}
			if p.current.type_ == .comma {
				p.advance()
				for p.current.type_ == .newline {
					p.advance()
				}
				if p.current.type_ == .rbrace {
					break
				}
				continue
			}
			return p.error_and_return('Expected comma or closing brace in application block')
		}
	}
	for p.current.type_ == .newline {
		p.advance()
	}
	if p.current.type_ != .rbrace {
		return p.error_and_return('Expected closing "}" for application block')
	}
	p.advance()
	return ast.new_application_config(p.get_next_id(), entries, start_pos)
}

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
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	p.in_supervisor_context = true
	body := p.parse_block()!
	p.in_supervisor_context = false
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close supervisor definition')
	}
	p.advance()
	return ast.new_supervisor_def(p.get_next_id(), name, body, start_pos)
}

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
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	body := p.parse_block()!
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close worker definition')
	}
	p.advance()
	return ast.new_worker_def(p.get_next_id(), name, body, start_pos)
}

// ===================================================================
// ================== Directive and Test Analysis ==================
// ===================================================================

fn (mut p Parser) parse_directive_new() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip '@'
	if p.current.type_ != .identifier {
		return p.error_and_return('Expected directive name after @')
	}
	name := p.current.value
	p.advance()
	mut args := []ast.Node{}
	if p.current.type_ == .lparen {
		args = p.do_parse_parenthesized_argument_list()!
	} else if p.current.type_ == .string {
		args << p.parse_expression()!
	}
	match name {
		'moduledoc' { p.directives_table.update_moduledoc(args[0].value) }
		'doc' { p.temp_doc_node = args[0] }
		else {}
	}
	return ast.new_directive(p.get_next_id(), name, args, start_pos)
}

fn (mut p Parser) parse_assert_expression() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'assert'
	condition := p.parse_expression()!
	return ast.new_function_caller(p.get_next_id(), 'assert', [condition], start_pos)
}

fn (mut p Parser) parse_test_block() !ast.Node {
	start_pos := p.current.position
	p.advance() // Skip 'test'
	if p.current.type_ != .string {
		return p.error_and_return('Expected test name string')
	}
	name := p.current.value
	p.advance()
	if p.current.type_ != .do {
		return p.error_and_return('Expected "do" after test name')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	body := p.parse_block()!
	if p.current.type_ != .end {
		return p.error_and_return('Expected "end" to close test block')
	}
	p.advance()
	return ast.new_test_block(p.get_next_id(), name, body, start_pos)
}

fn (mut p Parser) parse_describe_block() ![]ast.Node {
	p.advance() // Skip 'describe'
	if p.current.type_ != .string {
		return error('Expected test suite name string')
	}
	_ := p.current.value
	p.advance()
	if p.current.type_ != .do {
		return error('Expected "do" after describe name')
	}
	p.advance()
	for p.current.type_ == .newline {
		p.advance()
	}
	mut test_functions := []ast.Node{}
	for p.current.type_ != .end && p.current.type_ != .eof {
		for p.current.type_ == .newline {
			p.advance()
		}
		if p.current.type_ == .end {
			break
		}
		if p.current.type_ == .test {
			test_functions << p.parse_test_block()!
		} else {
			return error('Only test blocks are allowed inside describe blocks')
		}
		for p.current.type_ == .newline {
			p.advance()
		}
	}
	if p.current.type_ != .end {
		return error('Expected "end" to close describe block')
	}
	p.advance()
	return test_functions
}

// ===================================================================
// ==================== Verification Functions (Helpers) ===============
// ===================================================================

fn (p Parser) is_valid_directive(name string) bool {
	return name in ['print', 'type']
}

fn (p Parser) is_infix_function(name string) bool {
	function_info := kernel.get_function_info(name) or { return false }
	return function_info.fixity == .infix
}

fn (p Parser) is_single_arg_prefix_function(identifier string) bool {
	return identifier == 'not'
}

fn (mut p Parser) looks_like_clause_pattern() bool {
	if !p.at_line_start {
		return false
	}
	non_patterns := [lexer.TokenType.end, .eof, .else_, .if_, .with, .case, .fn, .spawn, .receive,
		.do, .rescue]
	if p.current.type_ in non_patterns {
		return false
	}
	if p.next.type_ in [.arrow, .when] {
		return true
	}
	if p.current.type_ in [.lparen, .lbrace, .lbracket] {
		return true
	}
	if p.current.type_ == .identifier && p.next.type_ == .lbrace {
		return true
	}
	return false
}
