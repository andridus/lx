module main

import lexer

fn main() {
	println('Testing is_keyword function:')
	println('is_keyword("x"): ${lexer.is_keyword('x')}')
	println('is_keyword("def"): ${lexer.is_keyword('def')}')
	println('is_keyword("if"): ${lexer.is_keyword('if')}')
	println('is_keyword("test"): ${lexer.is_keyword('test')}')
	println('is_keyword("42"): ${lexer.is_keyword('42')}')
	println('is_keyword(""): ${lexer.is_keyword('')}')

	println('\nAll keywords:')
	for keyword in lexer.get_all_keywords() {
		println('- "${keyword}"')
	}
}
