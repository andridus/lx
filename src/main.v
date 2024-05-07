module main

import os
import parser

fn main() {
	path := os.args[1]
	source := os.read_file(path) or { panic(err) }
	response := parser.parse_stmt(source) or {
		println(err.msg())
		exit(1)
	}
	println(response)
	exit(0)
}
