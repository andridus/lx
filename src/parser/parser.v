module parser

import ast
import lexer
import token
import table

const ending_kinds = [token.Kind.eof, .newline]

struct Options {
	build_path string = '_build'
}

struct Parser {
	filename string
	options  &Options = unsafe { nil }
mut:
	lexer              &lexer.Lexer
	stmts              []ast.Node
	var_table          &table.VarTable
	fun_table          &ast.FunTable
	type_table         &table.TypeTable
	current_module     string = '__empty__'
	current_token      token.Token
	next_token         token.Token
	brackets_delimiter []token.Kind
	in_keyword_list    bool
	in_args            bool
	scope_context      u32
}

pub fn parse_stmts(data []u8) ![]ast.Node {
	mut l := lexer.Lexer.init(data)!
	mut var_table := table.VarTable.init()!
	mut fun_table := ast.FunTable.init()!
	mut type_table := table.TypeTable.init()!
	mut p := Parser{
		lexer:      &l
		var_table:  var_table
		fun_table:  fun_table
		type_table: type_table
	}
	p.call_next_token()!
	p.call_next_token()!
	for p.current_token.kind != .eof {
		p.ignore_next_newline()
		stmt := p.stmt()!

		if !stmt.is_comment() { // ignore comment
			p.stmts << stmt
		}
		p.ignore_next_newline()
	}

	return p.stmts
}

pub fn parse_stmt(data []u8) !ast.Node {
	mut l := lexer.Lexer.init(data)!
	mut var_table := table.VarTable.init()!
	mut fun_table := ast.FunTable.init()!
	mut type_table := table.TypeTable.init()!
	mut p := Parser{
		lexer:      &l
		var_table:  var_table
		fun_table:  fun_table
		type_table: type_table
	}
	p.current_token = p.lexer.read_next_token()!
	p.next_token = p.lexer.read_next_token()!
	return p.stmt()!
}

fn (mut p Parser) call_next_token() ! {
	p.current_token = p.next_token
	p.next_token = p.lexer.read_next_token() or { token.Token{} }
}

fn (mut p Parser) expect_keyword(keyword0 string) !token.Token {
	keyword := p.expect(._keyword_atom)!
	if keyword.value == keyword0 {
		return keyword
	} else {
		return error(p.lexer.show_error_custom_error('expect keyword atom `${keyword0}` but received `${keyword.value}`'))
	}
}

fn (mut p Parser) expect(kind token.Kind) !token.Token {
	p.ignore_next_newline()
	curr := p.current_token
	if p.current_token.kind == kind {
		p.ignore_next_newline()

		p.call_next_token() or {}
		return curr
	} else {
		return error(p.lexer.show_error_custom_error(p.gen_expect_error(kind)))
	}
}

fn (mut p Parser) expect_one_of(kinds []token.Kind) !token.Token {
	curr := p.current_token
	if p.current_token.kind in kinds {
		p.ignore_next_newline()
		p.call_next_token() or {}
		return curr
	} else {
		return error(p.lexer.show_error_custom_error(p.gen_expect_one_of_error(kinds)))
	}
}

fn (mut p Parser) call_match_and_next_token(kind token.Kind) ! {
	if p.current_token.kind == kind {
		p.call_next_token()!
	} else {
		error(p.lexer.show_error_custom_error(p.gen_expect_error(kind)))
	}
}

fn (mut p Parser) stmt() !ast.Node {
	p.ignore_next_newline()
	curr := p.current_token
	match curr.kind() {
		._defmodule {
			return p.parse_module()
		}
		._def {
			return p.parse_function()
		}
		.eof {
			return error('eof')
		}
		else {
			return p.expr()
		}
	}
}

fn (mut p Parser) expr() !ast.Node {
	p.ignore_next_newline()

	curr := p.current_token
	node := match curr.kind() {
		._nil {
			mut meta := p.meta()
			p.expect(._nil)!
			p.update_meta(mut meta)
			meta.set_literal(.l_nil)
			meta.set_kind(.k_literal)
			ast.new_node('nil', meta, none)
		}
		._ident {
			p.parse_def_var()!
		}
		._int {
			mut meta := p.meta()
			p.expect(._int)!
			p.update_meta(mut meta)
			meta.set_literal(.l_integer)
			meta.set_kind(.k_literal)
			ast.new_node(curr.value(), meta, none)
		}
		._float, ._float_e {
			mut meta := p.meta()
			p.expect_one_of([._float, ._float_e])!
			p.update_meta(mut meta)
			meta.set_literal(.l_float)
			meta.set_kind(.k_literal)
			ast.new_node(curr.value(), meta, none)
		}
		._string {
			mut meta := p.meta()
			p.expect(._string)!
			p.update_meta(mut meta)
			meta.set_literal(.l_string)
			meta.set_kind(.k_literal)
			ast.new_node(curr.value(), meta, none)
		}
		._aliases {
			p.parse_aliases()!
		}
		._charlist {
			mut meta := p.meta()
			p.expect(._charlist)!
			p.update_meta(mut meta)
			meta.set_literal(.l_list)
			meta.set_literal_accepts([.l_char])
			meta.set_kind(.k_literal)
			ast.new_node(curr.value(), meta, none)
		}
		._lsbr {
			p.parse_list()!
		}
		._lcbr {
			p.parse_tuple()!
		}
		._true {
			mut meta := p.meta()
			p.expect(._true)!
			p.update_meta(mut meta)
			meta.set_literal(.l_boolean)
			meta.set_kind(.k_literal)
			ast.new_node('true', meta, none)
		}
		._false {
			mut meta := p.meta()
			p.expect(._false)!
			p.update_meta(mut meta)
			meta.set_literal(.l_boolean)
			meta.set_kind(.k_literal)
			ast.new_node('false', meta, none)
		}
		._atom {
			mut meta := p.meta()
			p.expect(._atom)!
			p.update_meta(mut meta)
			meta.set_literal(.l_atom)
			meta.set_kind(.k_literal)
			ast.new_node(curr.value(), meta, none)
		}
		._keyword_atom {
			p.parse_keyword_list()!
		}
		._mod_attr {
			p.parse_attribute()!
		}
		._comment {
			mut meta := p.meta()
			p.expect(._comment)!
			p.update_meta(mut meta)
			meta.set_literal(.l_string)
			meta.set_kind(.k_comment)
			ast.new_node(curr.value(), meta, none)
		}
		else {
			return error(p.lexer.show_error_custom_error('not parsed kind `${curr.kind()}`'))
		}
	}
	return p.maybe_apply_precendence(node)!
}

fn (mut p Parser) parse_keyword_item() !ast.Node {
	mut meta := p.meta()
	p.in_keyword_list = true
	key := p.expect(._keyword_atom)!
	p.update_meta(mut meta)
	meta.set_literal(.l_atom)
	meta.set_kind(.k_literal)
	value := p.expr()!
	key_node := ast.new_node(key.value(), meta, none)
	p.update_meta(mut meta)
	meta.set_literal(.l_tuple)
	meta.set_literal_accepts([.l_atom, value.meta.literal()])
	meta.set_kind(.k_keyword_list)
	return ast.new_node('{}', meta, [key_node, value])
}

fn (mut p Parser) parse_keyword_list() !ast.Node {
	mut meta := p.meta()
	mut nodes := [p.parse_keyword_item()!]
	for p.current_token.kind == ._comma {
		p.expect(._comma)!
		nodes << p.parse_keyword_item() or { break }
	}
	p.update_meta(mut meta)
	meta.set_literal(.l_list)
	meta.set_literal_accepts([.l_tuple])
	meta.set_kind(.k_keyword_list)
	return ast.new_node('[]', meta, nodes)
}

fn (mut p Parser) parse_attribute() !ast.Node {
	mut meta := p.meta()
	key := p.expect(._mod_attr)!
	p.update_meta(mut meta)
	meta.set_literal(.l_atom)
	meta.set_kind(.k_literal)
	key_node := ast.new_node(key.value(), meta, none)
	value := p.expr()!
	p.update_meta(mut meta)
	meta.copy_literal_from_node(value)
	meta.set_kind(.k_attribute)
	return ast.new_node('@', meta, [key_node, value])
}

fn (mut p Parser) parse_aliases() !ast.Node {
	mut meta := p.meta()
	mut alias := p.expect(._aliases)!
	mut nodes := []ast.Node{}
	for p.current_token.kind == ._dot {
		p.expect(._dot)!
		p.update_meta(mut meta)
		alias = p.expect(._aliases)!
		nodes << ast.new_node(alias.value(), meta, none)
	}
	p.update_meta(mut meta)
	meta.set_literal(.l_atom)
	meta.set_kind(.k_alias)
	return ast.new_node('__aliases__', meta, nodes)
}

fn (mut p Parser) ignore_next_newline() {
	for {
		if p.current_token.kind == .newline {
			p.call_next_token() or { break }
		} else {
			break
		}
	}
}

fn (mut p Parser) find_keyword() ?ast.Node {
	return none
}

fn (mut p Parser) maybe_apply_precendence(node ast.Node) !ast.Node {
	mut meta := p.meta()
	if prec := p.current_token.precedence() {
		left := node
		function_token := p.current_token
		p.call_next_token()!
		right := p.expr()!

		function_caller := ast.FunctionCallerAttributes.new(prec.get_precedence(), .infix)
		match prec.get_assoc() {
			.left {
				return p.insert_node_deep_left(function_token.value(), function_caller,
					left, right, mut meta)
			}
			else {
				p.update_meta(mut meta)
				meta.set_literal(p.promote_types(left, right))
				meta.set_kind(.k_function_caller)
				meta.set_function_caller_attributes(function_caller)
				return ast.new_node(function_token.value(), meta, [left, right])
			}
		}
	} else {
		p.check_end_token()!
		return node
	}
}

fn (mut p Parser) insert_node_deep_left(name string, function_caller ast.FunctionCallerAttributes, left ast.Node, right ast.Node, mut meta ast.Meta) !ast.Node {
	if right.meta.kind == .k_function_caller {
		if function_caller_attributes := right.meta.function_caller_attributes {
			if function_caller_attributes.precedence > 0
				&& function_caller_attributes.precedence <= function_caller.precedence {
				if nodes := right.nodes {
					if nodes.len == 2 {
						left0 := nodes[0]
						right0 := nodes[1]
						node_left := p.insert_node_deep_left(name, function_caller, left,
							left0, mut meta)!
						name0 := right.left.to_str()
						mut meta0 := right.meta
						meta0.set_literal(p.promote_types(left, right))
						meta.set_kind(.k_literal)
						return ast.new_node(name0, meta0, [node_left, right0])
					}
				}
			}
		}
	}
	meta.set_literal(p.promote_types(left, right))
	meta.set_kind(.k_literal)
	meta.set_kind(.k_function_caller)
	meta.set_function_caller_attributes(function_caller)
	return ast.new_node(name, meta, [left, right])
}

fn (mut p Parser) promote_types(left ast.Node, right ast.Node) ast.Literal {
	left_lit := left.meta.literal()
	right_lit := right.meta.literal()
	if left_lit == right_lit && left_lit in [.l_integer, .l_float] {
		return left_lit
	} else if left_lit != right_lit && left_lit in [.l_integer, .l_float]
		&& right_lit in [.l_integer, .l_float] {
		return .l_float
	}
	return left_lit
}

fn (mut p Parser) check_end_token() !bool {
	if p.in_args {
		return true
	}
	if p.brackets_delimiter.len > 0 {
		if p.current_token.kind == ._comma {
			return true
		}
		if p.brackets_delimiter.last() == p.current_token.kind {
			return true
		}
	}
	if p.in_keyword_list {
		if p.current_token.kind == ._comma {
			return true
		}
	}
	if p.current_token.kind() in ending_kinds {
		return true
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) check_not_end_token() !bool {
	if p.current_token.kind() !in ending_kinds {
		return true
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) parse_list() !ast.Node {
	mut meta := p.meta()
	mut literals := []ast.Literal{}
	mut nodes := []ast.Node{}
	p.brackets_delimiter << ._rsbr
	_ := p.expect(._lsbr)!
	p.ignore_next_newline()
	if p.current_token.kind == ._rsbr {
		p.expect(._rsbr)!
		return ast.new_node('[]', meta, [])
	}
	for {
		node := p.expr()!
		if node.meta.literal() !in literals {
			literals << node.meta.literal()
		}
		nodes << node
		end_token := p.expect_one_of([._comma, ._rsbr])!
		if end_token.kind == ._rsbr {
			break
		}
	}
	p.brackets_delimiter.delete_last()
	if nodes.len == 1 && nodes[0].get_kind() == .k_keyword_list {
		return nodes[0]
	}
	p.update_meta(mut meta)
	meta.set_literal(.l_list)
	meta.set_literal_accepts(literals)
	meta.set_kind(.k_literal)
	return ast.new_node('[]', meta, nodes)
}

fn (mut p Parser) parse_tuple() !ast.Node {
	mut meta := p.meta()
	mut literals := []ast.Literal{}
	mut nodes := []ast.Node{}
	p.brackets_delimiter << ._rcbr
	_ := p.expect(._lcbr)!
	for {
		node := p.expr()!
		literals << node.meta.literal()
		nodes << node
		end_token := p.expect_one_of([._comma, ._rcbr])!
		if end_token.kind == ._rcbr {
			break
		}
	}
	p.update_meta(mut meta)
	meta.set_literal(.l_tuple)
	meta.set_literal_accepts(literals)
	meta.set_kind(.k_literal)
	p.brackets_delimiter.delete_last()
	return ast.new_node('{}', meta, nodes)
}

fn (mut p Parser) meta() ast.Meta {
	pos, line := p.lexer.current_position()
	return ast.Meta.new(line, pos)
}

fn (mut p Parser) update_meta(mut meta ast.Meta) {
	pos, line := p.lexer.current_position()
	meta.update_pos(line, pos)
}
