module internal

import ast
import lexer
import errors

pub fn (mut p Parser) check_and_handle_error_token() bool {
	if p.current is lexer.ErrorToken {
		pos := ast.Position{
			line:   1
			column: p.position + 1
		}
		comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
			message:  (p.current as lexer.ErrorToken).message
			expected: 'expression'
			found:    p.current.str()
		}), pos, 'Unexpected token ' + p.current.str())
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
			if keyword_token == lexer.KeywordToken.def || keyword_token == lexer.KeywordToken.defp
				|| keyword_token == lexer.KeywordToken.record
				|| keyword_token == lexer.KeywordToken.worker
				|| keyword_token == lexer.KeywordToken.supervisor
				|| keyword_token == lexer.KeywordToken.spec
				|| keyword_token == lexer.KeywordToken.describe
				|| keyword_token == lexer.KeywordToken.test_ {
				return
			}
		}
		if p.current is lexer.PunctuationToken {
			punct_token := p.current as lexer.PunctuationToken
			if punct_token == lexer.PunctuationToken.semicolon {
				p.advance()
				return
			}
		}
		if p.current is lexer.ErrorToken {
			pos := ast.Position{
				line:   1
				column: p.position + 1
			}
			comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
				message:  (p.current as lexer.ErrorToken).message
				expected: 'statement or block start'
				found:    p.current.str()
			}), pos, 'Unexpected token during error recovery: ' + p.current.str())
			p.errors << comp_error
		}
		p.advance()
	}
}
