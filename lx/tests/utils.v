module main

import compile

fn compile_lx(code string) string {
	compiled := compile.compile_string(code, 'test.lx') or { return 'falha' }
	return compiled
}

fn compile_lx_with_error(code string) string {
	compiled := compile.compile_string(code, 'test.lx') or { err.msg() }
	return compiled
}
