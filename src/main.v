module main

import os
import parser
import color
import testing
import flag

const version = '0.1.0'

@[xdoc: 'Lx compiler']
@[version: version]
@[name: 'lx']
@[footer: '']
struct Config {
	show_version bool @[short: v; xdoc: 'Show version and exit']
	show_help    bool @[short: h; xdoc: 'Show help']
	test_suite   bool @[long: suite; xdoc: 'Run tests suite']
	file         string
}

fn main() {
	config, no_matches := flag.to_struct[Config](os.args, skip: 1)!

	if config.show_help {
		documentation := flag.to_doc[Config]()!
		println(documentation)
		exit(0)
	}
	if config.show_version {
		println('Lx compiler ${version}')
		exit(0)
	}
	if config.test_suite {
		testing.run_suite()
	}

	if no_matches.len != 1 {
		println('Invalid command, show help with `-h`')
		exit(0)
	}
	path := os.args[1]
	source := os.read_bytes(path) or {
		println('File \'${path}\' not found')
		exit(1)
	}
	nodes := parser.parse_stmts(source, path ) or {
		println(err.msg())
		exit(1)
	}
	for node in nodes {
		println(node.elixir(true))
	}
	println(color.fg(.dark_green, .default, '->> compiled with success!'))
}
