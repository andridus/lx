module parser

import strings
import color

fn (p Parser) gen_syntax_error() string {
	line, col := 1, 4
	term := p.next_token.value()
	term_before := p.current_token.value()
	source := 'source'
	mut sb := strings.new_builder(100)
	sb.write_string(color.fg(.red, .default, '\n** (SyntaxError)'))
	sb.write_string(color.fg(.default, .default, ' [${source}:${line}:${col}] '))
	sb.write_string('syntax error before "${term}"\n')
	sb.writeln('  |')
	sb.writeln('${line} | ${term_before} ${term}')
	sb.writeln('  |${' '.repeat(term_before.len + 2)}^\n')
	return sb.str()
}

fn (mut p Parser) gen_token_error() string {
	return p.lexer.gen_token_error(1, 4)
}
