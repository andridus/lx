module parser

import ast

fn (mut p Parser) parse_function() !ast.Node {
	mut meta := p.meta()
	mut code := []ast.Node{}
	mut args := []ast.Literal{}
	p.expect(._def)!
	function_name := p.expect(._ident)!
	if p.current_token.kind == ._lpar {
		p.expect(._lpar)!
		p.in_args = true
		p.ignore_next_newline()
		for {
			arg := p.expr()!
			args << arg.get_meta_literal()
			ex0 := p.expect_one_of([._rpar, ._comma])!
			match ex0.kind {
				._comma {
					continue
				}
				._rpar {
					break
				}
				else {}
			}
		}
		p.in_args = false
	}
	ex := p.expect_one_of([._do, ._comma])!
	match ex.kind {
		._do {
			p.ignore_next_newline()
			for p.current_token.kind != ._end {
				node := p.expr()!
				code << node
				p.ignore_next_newline()
			}
			p.expect(._end)!
		}
		._comma {
			p.expect_keyword('do')!
			code << p.stmt()!
		}
		else {}
	}
	mut returns := ast.Literal.l_nil
	if code.len > 0 {
		last := code.last()
		meta.copy_literal_from_node(last)
		returns = last.get_meta_literal()
	}
	fun_val := p.fun_table.insert(function_name.value(), returns, args)!
	meta.set_kind(.k_function_def)
	meta.set_function_attributes(ast.FunctionAttributes{ idx: fun_val, fun_table: p.fun_table })
	return ast.new_node(function_name.value(), meta, code)
}
