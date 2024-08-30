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
	nodes := parser.parse_stmts(source) or { exit(1) }
	for node in nodes {
		println(node.elixir())
	}
	println('finish with success!')
}
