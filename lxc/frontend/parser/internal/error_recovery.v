module internal

import ast
import lexer
import errors

pub fn (mut p Parser) check_and_handle_error_token() bool {
	if p.current is lexer.ErrorToken {
		pos := p.current.get_position()
		ast_pos := ast.new_position(pos.line, pos.column, pos.filename)
		comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
			message:  (p.current as lexer.ErrorToken).message
			expected: 'expression'
			found:    p.current.str()
		}), ast_pos, 'Unexpected token ' + p.current.str())
		p.errors << comp_error
		p.advance()
		return true
	}
	return false
}

pub fn (mut p Parser) synchronize_after_error() {
	p.advance()
	for !p.is_at_end() {
		if p.current is lexer.KeywordToken {
			keyword_token := p.current as lexer.KeywordToken
			if keyword_token.value == .def || keyword_token.value == .defp
				|| keyword_token.value == .record || keyword_token.value == .worker
				|| keyword_token.value == .supervisor || keyword_token.value == .spec
				|| keyword_token.value == .describe || keyword_token.value == .test_ {
				return
			}
		}
		if p.current is lexer.PunctuationToken {
			punct_token := p.current as lexer.PunctuationToken
			if punct_token.value == .semicolon {
				p.advance()
				return
			}
		}
		if p.current is lexer.ErrorToken {
			pos := p.current.get_position()
			ast_pos := ast.new_position(pos.line, pos.column, pos.filename)
			comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
				message:  (p.current as lexer.ErrorToken).message
				expected: 'statement or block start'
				found:    p.current.str()
			}), ast_pos, 'Unexpected token during error recovery: ' + p.current.str())
			p.errors << comp_error
		}
		p.advance()
	}
}
