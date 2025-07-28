module parser

import lexer
import ast
import errors

pub struct Parser {
mut:
	lexer          lexer.Lexer
	current        lexer.Token
	error_reporter errors.ErrorReporter
	next_ast_id    int = 1
}

pub fn new_parser(mut l lexer.Lexer) Parser {
	mut p := Parser{
		lexer:          l
		error_reporter: errors.new_error_reporter()
	}
	p.advance()
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
	p.current = p.lexer.next_token()
}

fn (mut p Parser) error(msg string) {
	p.error_reporter.report(.parser, msg, p.current.position)
}

fn (mut p Parser) parse_module() !ast.Node {
	mut functions := []ast.Node{}
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

		func := p.parse_function()!
		functions << func
	}

	return ast.new_module(module_id, 'main', functions, start_pos)
}

pub fn (mut p Parser) parse_with_modname(modname string) !ast.Node {
	return p.parse_module_with_name(modname)
}

fn (mut p Parser) parse_module_with_name(modname string) !ast.Node {
	mut functions := []ast.Node{}
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

		func := p.parse_function()!
		functions << func
	}

	return ast.new_module(module_id, modname, functions, start_pos)
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
		p.error('Expected "(", got "${p.current.value}"')
		return error('Expected (')
	}
	p.advance()

	if p.current.type_ != .rparen {
		p.error('Function parameters not supported in Task 1')
		return error('Function parameters not supported')
	}
	p.advance()

	if p.current.type_ != .do {
		p.error('Expected "do", got "${p.current.value}"')
		return error('Expected do')
	}
	p.advance()

	for p.current.type_ == .newline {
		p.advance()
	}

	body := p.parse_literal()!

	for p.current.type_ == .newline {
		p.advance()
	}

	if p.current.type_ != .end {
		p.error('Expected "end", got "${p.current.value}"')
		return error('Expected end')
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
