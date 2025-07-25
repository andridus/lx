module parser

import lexer
import ast

pub struct Parser {
mut:
    lexer   lexer.Lexer
    current lexer.Token
    errors  []string
}

pub fn new_parser(mut l lexer.Lexer) Parser {
    mut p := Parser{
        lexer: l
    }
    p.advance() // Lê primeiro token
    return p
}

pub fn (mut p Parser) parse() !ast.Node {
    return p.parse_module()
}

pub fn (p Parser) get_errors() []string {
    return p.errors
}

fn (mut p Parser) advance() {
    p.current = p.lexer.next_token()
}

fn (mut p Parser) error(msg string) {
    full_msg := 'Parse error at ${p.current.position}: ${msg}'
    p.errors << full_msg
}

fn (mut p Parser) parse_module() !ast.Node {
    mut functions := []ast.Node{}
    start_pos := ast.new_position(1, 1, p.current.position.file)

    for p.current.type_ != .eof {
        if p.current.type_ == .newline {
            p.advance()
            continue
        }

        if p.current.type_ == .error {
            return error('Lexical error: ${p.current.value}')
        }

        func := p.parse_function()!
        functions << func
    }

    return ast.new_module(functions, start_pos)
}

fn (mut p Parser) parse_function() !ast.Node {
    // Expect 'def'
    if p.current.type_ != .def {
        return error('Expected "def", got "${p.current.value}"')
    }
    start_pos := p.current.position
    p.advance()

    // Function name (identifier)
    if p.current.type_ != .identifier {
        return error('Expected function name, got "${p.current.value}"')
    }
    func_name := p.current.value
    p.advance()

    // Parameters (must be empty for Task 1)
    if p.current.type_ != .lparen {
        return error('Expected "(", got "${p.current.value}"')
    }
    p.advance()

    if p.current.type_ != .rparen {
        return error('Function parameters not supported in Task 1')
    }
    p.advance()

    // 'do' keyword
    if p.current.type_ != .do {
        return error('Expected "do", got "${p.current.value}"')
    }
    p.advance()

    // Skip optional newlines
    for p.current.type_ == .newline {
        p.advance()
    }

    // Function body (single literal)
    body := p.parse_literal()!

    // Skip optional newlines
    for p.current.type_ == .newline {
        p.advance()
    }

    // 'end' keyword
    if p.current.type_ != .end {
        return error('Expected "end", got "${p.current.value}"')
    }
    p.advance()

    return ast.new_function(func_name, body, start_pos)
}

fn (mut p Parser) parse_literal() !ast.Node {
    pos := p.current.position

    return match p.current.type_ {
        .integer {
            value := p.current.value.int()
            p.advance()
            ast.new_integer(value, pos)
        }
        .float {
            value := p.current.value.f64()
            p.advance()
            ast.new_float(value, pos)
        }
        .string {
            value := p.current.value
            p.advance()
            ast.new_string(value, pos)
        }
        .true_ {
            p.advance()
            ast.new_boolean(true, pos)
        }
        .false_ {
            p.advance()
            ast.new_boolean(false, pos)
        }
        .atom {
            value := p.current.value
            p.advance()
            ast.new_atom(value, pos)
        }
        .nil_ {
            p.advance()
            ast.new_nil(pos)
        }
        else {
            error('Expected literal, got "${p.current.value}"')
        }
    }
}