module main

import os
import parser

fn main() {
	if os.args.len == 1 {
		println('file not valid')
		exit(1)
	}
	path := os.args[1]
	source := os.read_bytes(path) or { panic(err) }
	response := parser.parse_stmt(source) or { exit(1) }
	println(response.elixir())
	println('finish with success!')
}
