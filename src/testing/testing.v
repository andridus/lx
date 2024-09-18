module testing

import color
import os
import parser

const root_path = 'test'

pub fn run_suite() bool {
	paths := os.ls('${@VMODROOT}/${root_path}') or {
		println(color.fg(.dark_red, .default, 'ERROR: ') + err.msg())
		exit(1)
	}
	println('Test Suite: Run compile over ${paths.len} files \n----')
	for path in paths {
		println(color.fg(.white, .default, 'Compiling ${root_path}/${path}'))
		source := os.read_bytes('${root_path}/${path}') or { return false }
		_ := parser.parse_stmts(source) or { exit(1) }
		println(color.fg(.dark_green, .default, '->> ${root_path}/${path} compiled with success!'))
	}
	exit(1)
	return false
}
