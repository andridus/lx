module main

import compile

fn compile_lx(code string) string {
	compiled := compile.compile_string(code, 'test.lx', false, false) or { return 'falha' }
	return compiled
}
