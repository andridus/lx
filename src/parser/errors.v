module parser

import lexer
import strings

fn (p Parser) gen_syntax_error() string {
	line, number := 1, 4
	term := p.next_token.value()
	term_before := p.current_token.value()
	source := 'source'
	mut sb := strings.new_builder(100)
	sb.write_string('\n** (SyntaxError)')
	sb.write_string(' [${source}:${line}:${number}] ')
	sb.write_string('syntax error before "${term}"\n')
	sb.writeln('  |')
	sb.writeln('${line} | ${term_before} ${term}')
	sb.writeln('  |${' '.repeat(term_before.len + 2)}^\n')
	return sb.str()
}
