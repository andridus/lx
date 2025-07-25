module lexer

import ast

pub struct Lexer {
mut:
    input    string
    position int
    line     int
    column   int
    file     string
}

pub fn new_lexer(input string, file string) Lexer {
    return Lexer{
        input: input,
        file: file,
        line: 1,
        column: 1
    }
}

pub fn (mut l Lexer) next_token() Token {
    l.skip_whitespace()

    if l.position >= l.input.len {
        return l.make_token(.eof, '')
    }

    ch := l.input[l.position]

    return match ch {
        `(` { l.advance_and_return(.lparen, '(') }
        `)` { l.advance_and_return(.rparen, ')') }
        `"` { l.read_string() }
        `:` { l.read_atom() }
        `0`...`9` { l.read_number() }
        `a`...`z`, `A`...`Z`, `_` { l.read_identifier() }
        `\n` { l.advance_and_return(.newline, '\n') }
        else { l.make_error('Unexpected character: ${ch.ascii_str()}') }
    }
}

fn (mut l Lexer) skip_whitespace() {
    for l.position < l.input.len {
        ch := l.input[l.position]
        if ch == ` ` || ch == `\t` || ch == `\r` {
            l.advance()
        } else if ch == `#` {
            // Skip comment until end of line
            for l.position < l.input.len && l.input[l.position] != `\n` {
                l.advance()
            }
        } else {
            break
        }
    }
}

fn (mut l Lexer) advance() {
    if l.position < l.input.len && l.input[l.position] == `\n` {
        l.line++
        l.column = 1
    } else {
        l.column++
    }
    l.position++
}

fn (mut l Lexer) advance_and_return(token_type TokenType, value string) Token {
    pos := l.current_position()
    l.advance()
    return l.make_token_at(token_type, value, pos)
}

fn (mut l Lexer) current_position() ast.Position {
    return ast.new_position(l.line, l.column, l.file)
}

fn (mut l Lexer) make_token(token_type TokenType, value string) Token {
    return new_token(token_type, value, l.current_position())
}

fn (mut l Lexer) make_token_at(token_type TokenType, value string, pos ast.Position) Token {
    return new_token(token_type, value, pos)
}

fn (mut l Lexer) make_error(msg string) Token {
    return l.make_token(.error, msg)
}

fn (mut l Lexer) read_string() Token {
    start_pos := l.current_position()
    l.advance() // Skip opening quote

    mut value := ''

    for l.position < l.input.len && l.input[l.position] != `"` {
        ch := l.input[l.position]
        if ch == `\\` {
            l.advance()
            if l.position >= l.input.len {
                return l.make_error('Unterminated string literal')
            }

            escape_ch := l.input[l.position]
            value += match escape_ch {
                `n` { '\n' }
                `t` { '\t' }
                `r` { '\r' }
                `\\` { '\\' }
                `"` { '"' }
                else { escape_ch.ascii_str() }
            }
        } else {
            value += ch.ascii_str()
        }
        l.advance()
    }

    if l.position >= l.input.len {
        return l.make_error('Unterminated string literal')
    }

    l.advance() // Skip closing quote
    return l.make_token_at(.string, value, start_pos)
}

fn (mut l Lexer) read_atom() Token {
    start_pos := l.current_position()
    l.advance() // Skip ':'

    if l.position >= l.input.len || !l.input[l.position].is_letter() {
        return l.make_error('Invalid atom: must start with letter')
    }

    mut value := ''

    for l.position < l.input.len {
        ch := l.input[l.position]
        if ch.is_alnum() || ch == `_` {
            value += ch.ascii_str()
            l.advance()
        } else {
            break
        }
    }

    return l.make_token_at(.atom, value, start_pos)
}

fn (mut l Lexer) read_number() Token {
    start_pos := l.current_position()
    start := l.position

    // Read integer part
    for l.position < l.input.len && l.input[l.position].is_digit() {
        l.advance()
    }

    // Check for float
    if l.position < l.input.len && l.input[l.position] == `.` {
        // Look ahead to see if next char is digit
        if l.position + 1 < l.input.len && l.input[l.position + 1].is_digit() {
            l.advance() // Skip '.'

            for l.position < l.input.len && l.input[l.position].is_digit() {
                l.advance()
            }

            value := l.input[start..l.position]
            return l.make_token_at(.float, value, start_pos)
        }
    }

    value := l.input[start..l.position]
    return l.make_token_at(.integer, value, start_pos)
}

fn (mut l Lexer) read_identifier() Token {
    start_pos := l.current_position()
    start := l.position

    for l.position < l.input.len {
        ch := l.input[l.position]
        if ch.is_alnum() || ch == `_` {
            l.advance()
        } else {
            break
        }
    }

    value := l.input[start..l.position]

    // Check for keywords
    token_type := match value {
        'def' { TokenType.def }
        'do' { TokenType.do }
        'end' { TokenType.end }
        'true' { TokenType.true_ }
        'false' { TokenType.false_ }
        'nil' { TokenType.nil_ }
        else { TokenType.identifier }
    }

    return l.make_token_at(token_type, value, start_pos)
}