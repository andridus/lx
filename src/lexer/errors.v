module lexer

import strings
import color

pub fn (mut lexer0 Lexer) gen_token_error(line u8, col u8) string {
	lines := lexer0.source.get_lines_about(5)
	source := 'source'
	mut sb := strings.new_builder(100)
	sb.write_string(color.fg(.red, .default, '\n** (UnexpectedToken)'))
	sb.write_string(color.fg(.default, .default, ' [${source}:${line}:${col}] '))
	sb.write_string('unexpected token "${[lexer0.source.current()].bytestr()}"\n')
	for l in lines {
		sb.writeln(l)
	}
	return sb.str()
}
