// Copyright (c) 2023 Helder de Sousa. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import compiler_v.types
import compiler_v.ast

pub fn (p Parser) node_string_left(node string) ast.NodeLeft {
	return ast.NodeLeft(node)
}

pub fn (p Parser) node_left(node ast.Node) ast.NodeLeft {
	return ast.NodeLeft(node)
}

pub fn (p Parser) node(meta ast.Meta, left ast.NodeLeft, nodes []ast.Node) ast.Node {
	kind := match left {
		string {
			match left {
				'defmodule' {
					ast.NodeKind(ast.Module{})
				}
				'def' {
					ast.NodeKind(ast.Ast{
						lit: 'def'
					})
				}
				'when' {
					ast.NodeKind(ast.Ast{
						lit: 'when'
					})
				}
				'__aliases__' {
					ast.NodeKind(ast.Alias{})
				}
				'.' {
					ast.NodeKind(ast.Ast{
						lit: '.'
					})
				}
				'|' {
					ast.NodeKind(ast.Ast{
						lit: 'or_match'
					})
				}
				'%{}' {
					ast.NodeKind(ast.Ast{
						lit: '%{}'
					})
				}
				'__block__' {
					ast.NodeKind(ast.Ast{
						lit: 'block'
					})
				}
				else {
					ast.NodeKind(ast.Nil{})
				}
			}
		}
		int {
			ast.NodeKind(ast.Integer{})
		}
		f64 {
			ast.NodeKind(ast.Float{})
		}
		ast.Atom {
			ast.NodeKind(left)
		}
		ast.Node {
			left.kind
		}
	}
	mut left0 := left
	if left is string {
		s := left as string
		left0 = ast.NodeLeft(ast.Atom{
			name: s
		})
	}
	return ast.Node{
		left: left0
		kind: kind
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_underscore(meta ast.Meta, name string) ast.Node {
	return ast.Node{
		left: ast.Atom{
			name: '_${name}'
		}
		kind: ast.NodeKind(ast.Underscore{
			name: name
		})
		meta: meta
		nodes: []
	}
}

pub fn (p Parser) node_case(meta ast.Meta, ty ast.Case) ast.Node {
	mut clauses0 := []ast.Node{}
	for n := 0; n < ty.clauses.len; n++ {
		clause0 := ty.clauses[n]
		expr0 := ty.exprs[n]
		clauses0 << ast.Node{
			left: ast.NodeLeft(ast.Atom{
				name: '->'
			})
			kind: ast.NodeKind(ast.Ast{
				lit: 'case_clause'
			})
			meta: meta
			nodes: [p.node_list(meta, [clause0]), expr0]
		}
	}

	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: 'case'
		})
		kind: ty
		meta: meta
		nodes: [
			ty.eval,
			p.node_list(meta, [
				p.node_tuple(meta, [p.node_atomic('do'), p.node_list(meta, clauses0)]),
			]),
		]
	}
}

pub fn (p Parser) node_struct_field(meta ast.Meta, name string) ast.Node {
	name0 := p.node_atomic(name)
	type_ := p.node_atomic(meta.ti.kind.str())
	return p.node_tuple(meta, [name0, type_])
}

pub fn (p Parser) node_struct(meta ast.Meta, ty ast.Struct) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: 'defstruct'
		})
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: ty.fields.values()
	}
}

pub fn (p Parser) node_enum(meta ast.Meta, nodes []ast.Node, ty ast.Enum) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: 'defenum'
		})
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: [p.node_list(meta, nodes)]
	}
}

pub fn (p Parser) node_caller_enum(mut meta ast.Meta, value string, ty ast.Enum) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '@'
		})
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: [
			p.node(meta, '__aliases__', [p.node_atomic(ty.name)]),
			ast.Node{
				left: ast.NodeLeft(ast.Atom{
					name: value
				})
				kind: ast.NodeKind(ast.Atom{})
				meta: meta
			},
		]
	}
}

pub fn (p Parser) node_caller_struct(meta ast.Meta, ty ast.Struct) ast.Node {
	mut fields := []ast.Node{}
	for key, value in ty.exprs {
		fld := ty.fields[key].nodes[0]
		fields << p.node_tuple(meta, [fld, value])
	}
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '%'
		})
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: [
			p.node(meta, '__aliases__', [p.node_atomic(ty.name)]),
			p.node(meta, '%{}', fields),
		]
	}
}

pub fn (p Parser) node_function(meta ast.Meta, nodes []ast.Node, ty ast.Function) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: 'def'
		})
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_caller_to_string(meta ast.Meta, node ast.Node) ast.Node {
	module_name := 'FFI.v'
	fun_name := 'any_to_string'
	fun_node := p.node(meta, '.', [
		p.node(meta, '__aliases__', [p.node_atomic(module_name)]),
		p.node_atomic(fun_name),
	])
	arg_nodes := [node]

	mut arities := []string{}
	for an in arg_nodes {
		arities << an.meta.ti.str()
	}
	return p.node_function_caller(meta, p.node_left(fun_node), arg_nodes, ast.FunctionCaller{
		name: fun_name
		return_ti: types.string_ti
		module_name: module_name
		args: arg_nodes
		arity: arities
	})
}

pub fn (p Parser) node_function_caller(meta ast.Meta, left ast.NodeLeft, nodes []ast.Node, ty ast.FunctionCaller) ast.Node {
	mut left0 := left
	if left is string {
		atom := left as string
		left0 = ast.NodeLeft(ast.Atom{
			name: atom
		})
	}
	return ast.Node{
		left: left0
		kind: ast.NodeKind(ty)
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_var(meta ast.Meta, left string, nodes []ast.Node) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: left
		})
		kind: ast.NodeKind(ast.Ast{
			lit: 'var'
		})
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_assign(meta ast.Meta, ident string, node ast.Node) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '='
		})
		kind: ast.NodeKind(ast.Ast{
			lit: 'assign'
		})
		meta: meta
		nodes: [p.node_var(meta, ident, []), node]
	}
}

pub fn (p Parser) node_match(meta ast.Meta, left ast.Node, right ast.Node) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '='
		})
		kind: ast.NodeKind(ast.Ast{
			lit: 'match'
		})
		meta: meta
		nodes: [left, right]
	}
}

pub fn (p Parser) node_bang(meta ast.Meta, node ast.Node) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '!'
		})
		kind: ast.NodeKind(ast.Ast{
			lit: 'bang'
		})
		meta: meta
		nodes: [node]
	}
}

pub fn (p Parser) node_atom(mut meta ast.Meta, atom string) ast.Node {
	meta.put_ti(types.atom_ti)
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: atom
		})
		kind: ast.NodeKind(ast.Atom{})
		meta: meta
	}
}

pub fn (p Parser) node_nil() ast.Node {
	meta := p.meta_w_ti(types.nil_ti)
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: 'nil'
		})
		kind: ast.NodeKind(ast.Nil{})
		meta: meta
	}
}

pub fn (p Parser) node_string(mut meta ast.Meta, str string) ast.Node {
	meta.put_ti(types.string_ti)
	return ast.Node{
		left: ast.NodeLeft(str)
		kind: ast.NodeKind(ast.String{})
		meta: meta
	}
}

pub fn (p Parser) node_string_concat(mut meta ast.Meta, left ast.Node, right ast.Node) ast.Node {
	meta.put_ti(types.string_ti)
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: '<<>>'
		})
		kind: ast.NodeKind(ast.Ast{
			lit: 'string_concat'
		})
		meta: meta
		nodes: [left, right]
	}
}

pub fn (p Parser) node_integer(mut meta ast.Meta, val int) ast.Node {
	meta.put_ti(types.integer_ti)
	return ast.Node{
		left: ast.NodeLeft(val)
		kind: ast.NodeKind(ast.Integer{})
		meta: meta
	}
}

pub fn (p Parser) node_float(mut meta ast.Meta, val f64) ast.Node {
	meta.put_ti(types.float_ti)
	return ast.Node{
		left: ast.NodeLeft(val)
		kind: ast.NodeKind(ast.Float{})
		meta: meta
	}
}

pub fn (p Parser) node_tuple(meta ast.Meta, nodes []ast.Node) ast.Node {
	return ast.Node{
		kind: ast.NodeKind(ast.Tuple{})
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_list(meta ast.Meta, nodes []ast.Node) ast.Node {
	return ast.Node{
		kind: ast.NodeKind(ast.List{})
		meta: meta
		nodes: nodes
	}
}

pub fn (p Parser) node_atomic(atom string) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: atom
		})
		kind: ast.NodeKind(ast.Atomic{})
		nodes: []
	}
}

pub fn (p Parser) node_boolean(meta ast.Meta, atom string) ast.Node {
	return ast.Node{
		left: ast.NodeLeft(ast.Atom{
			name: atom
		})
		kind: ast.NodeKind(ast.Boolean{})
		meta: meta
		nodes: []
	}
}

pub fn (p Parser) node_default() ast.Node {
	return ast.Node{
		left: ast.NodeLeft('default')
		kind: ast.NodeKind(ast.Atomic{})
		nodes: []
	}
}

pub fn (p Parser) meta() ast.Meta {
	return ast.Meta{
		line: p.lexer.current_line
		start_pos: p.tok.pos
		inside_parens: p.inside_parens
	}
}

pub fn (p Parser) meta_w_ti(ti types.TypeIdent) ast.Meta {
	return ast.Meta{
		line: p.lexer.current_line
		start_pos: p.tok.pos
		inside_parens: p.inside_parens
		ti: ti
	}
}
