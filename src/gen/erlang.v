module gen

import parser
import ast
import strings

pub struct ErlangGen {
mut:
	out strings.Builder
}

pub fn generate_erlang(text string) !string {
	ast0 := parser.parse_stmt(text)!
	g := generate_erlang_from_ast(ast0)
	return g.build_scripting()
}

fn generate_erlang_from_ast(node ast.Node) ErlangGen {
	mut g := ErlangGen{
		out: strings.new_builder(1000)
	}
	g.writeln(g.generate_from_node(node))
	return g
}

@[inline]
fn (mut g ErlangGen) write(str string) {
	g.out.write_string(str)
}

@[inline]
fn (mut g ErlangGen) writeln(str string) {
	g.out.write_string(str)
	g.out.writeln('.')
}

fn (mut g ErlangGen) generate_from_node(node ast.Node) string {
	println(node)
	kind := node.kind
	return match kind {
		ast.Nil {
			node.left.to_str()
		}
		ast.Atom {
			node.left.to_str()
		}
		ast.Float {
			node.left.to_str()
		}
		ast.String {
			node.left.to_str()
		}
		ast.Integer {
			node.left.to_str()
		}
		ast.Function {
			if kind.position == .infix {
				function := node.left.to_str()
				left := g.generate_from_node(node.nodes[0])
				right := g.generate_from_node(node.nodes[1])
				'${left} ${function} ${right}'
			} else {
				panic('undefined function')
			}
		}
	}
}

fn (g ErlangGen) build_scripting() string {
	return '
	-module(script).
	-export([run/0]).

	run() -> ${g.out}'
}
