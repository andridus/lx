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
	current_module     string = '__empty__'
	current_token      token.Token
	next_token         token.Token
	brackets_delimiter []token.Kind
	in_keyword_list    bool
}

pub fn parse_stmts(data []u8) ![]ast.Node {
	mut l := lexer.Lexer.init(data)!
	mut var_table := table.VarTable.init()!
	mut p := Parser{
		lexer:     &l
		var_table: var_table
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
	mut p := Parser{
		lexer:     &l
		var_table: var_table
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
		._ident {
			p.parse_def_var()!
		}
		._int {
			p.expect(._int)!
			ast.new_node_2(curr.value(), ast.Integer{})
		}
		._float, ._float_e {
			p.expect_one_of([._float, ._float_e])!
			ast.new_node_2(curr.value(), ast.Float{})
		}
		._string {
			p.expect(._string)!
			ast.new_node_2(curr.value(), ast.String{})
		}
		._aliases {
			p.parse_aliases()!
		}
		._charlist {
			p.expect(._charlist)!
			ast.new_node_2(curr.value(), ast.Charlist{})
		}
		._lsbr {
			p.parse_list()!
		}
		._lcbr {
			p.parse_tuple()!
		}
		._true {
			p.expect(._true)!
			ast.new_node(ast.Boolean.new(true))
		}
		._false {
			p.expect(._false)!
			ast.new_node(ast.Boolean.new(false))
		}
		._atom {
			p.expect(._atom)!
			ast.new_atom_node(curr.value())
		}
		._keyword_atom {
			p.parse_keyword_list()!
		}
		._mod_attr {
			p.parse_attribute()!
		}
		._comment {
			p.expect(._comment)!
			ast.new_node_2(curr.value(), ast.Comment{})
		}
		else {
			return error(p.lexer.show_error_custom_error('not parsed kind `${curr.kind()}`'))
		}
	}
	return p.maybe_apply_precendence(node)!
}

fn (mut p Parser) parse_keyword_item() !ast.Node {
	p.in_keyword_list = true
	key := p.expect(._keyword_atom)!
	value := p.expr()!
	return ast.new_keyword_node(key.value, value)
}

fn (mut p Parser) parse_keyword_list() !ast.Node {
	mut nodes := [p.parse_keyword_item()!]
	for p.current_token.kind == ._comma {
		p.expect(._comma)!
		nodes << p.parse_keyword_item() or { break }
	}
	return ast.new_keyword_list(nodes)
}

fn (mut p Parser) parse_attribute() !ast.Node {
	key := p.expect(._mod_attr)!
	value := p.expr()!
	return ast.new_attribute_node(key.value, value)
}

fn (mut p Parser) parse_aliases() !ast.Node {
	mut alias := p.expect(._aliases)!
	mut aliases := [alias.value()]
	for p.current_token.kind == ._dot {
		p.expect(._dot)!
		alias = p.expect(._aliases)!
		aliases << alias.value()
	}
	return ast.new_aliases_node(aliases)
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
	if prec := p.current_token.precedence() {
		left := node
		function_token := p.current_token
		p.call_next_token()!
		right := p.expr()!

		caller_function_kind := ast.CallerFunction{
			precedence: prec.get_precedence()
			position:   .infix
		}
		match prec.get_assoc() {
			.left {
				return p.insert_node_deep_left(function_token.value(), caller_function_kind,
					left, right)
			}
			else {
				return ast.new_node_3(function_token.value(), caller_function_kind, [
					left,
					right,
				])
			}
		}
	} else {
		p.check_end_token()!
		return node
	}
}

fn (mut p Parser) insert_node_deep_left(name string, caller_function ast.CallerFunction, left ast.Node, right ast.Node) !ast.Node {
	if right.kind is ast.CallerFunction {
		function0 := right.kind as ast.CallerFunction
		if function0.precedence > 0 && function0.precedence <= caller_function.precedence {
			if nodes := right.nodes {
				if nodes.len == 2 {
					left0 := nodes[0]
					right0 := nodes[1]
					node_left := p.insert_node_deep_left(name, caller_function, left,
						left0)!
					name0 := right.left.to_str()
					return ast.new_node_3(name0, function0, [node_left, right0])
				}
			}
		}
	}
	return ast.new_node_3(name, caller_function, [left, right])
}

fn (mut p Parser) check_end_token() !bool {
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
	if p.current_token.kind() in parser.ending_kinds {
		return true
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) check_not_end_token() !bool {
	if p.current_token.kind() !in parser.ending_kinds {
		return true
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) parse_list() !ast.Node {
	mut nodes := []ast.Node{}
	mut kind := ast.NodeKind(ast.Nil{})
	p.brackets_delimiter << ._rsbr
	_ := p.expect(._lsbr)!
	p.ignore_next_newline()
	if p.current_token.kind == ._rsbr {
		p.expect(._rsbr)!
		return ast.new_node_3('[]', ast.List.new(kind), [])
	}
	for {
		node := p.expr()!
		if nodes.len == 0 {
			kind = node.kind
		} else {
			if mut kind is ast.Mixed {
				kind.put_if_required(node.kind)
			} else if node.kind != kind {
				kind = ast.NodeKind(ast.Mixed.new([kind, node.kind]))
			}
		}
		nodes << node
		end_token := p.expect_one_of([._comma, ._rsbr])!
		if end_token.kind == ._rsbr {
			break
		}
	}
	p.brackets_delimiter.delete_last()
	if nodes.len == 1 && nodes[0].kind is ast.KeywordList {
		return nodes[0]
	}
	return ast.new_node_3('[]', ast.List.new(kind), nodes)
}

fn (mut p Parser) parse_tuple() !ast.Node {
	mut nodes := []ast.Node{}
	mut kinds := []ast.NodeKind{}
	p.brackets_delimiter << ._rcbr
	_ := p.expect(._lcbr)!
	for {
		node := p.expr()!
		kinds << node.kind
		nodes << node
		end_token := p.expect_one_of([._comma, ._rcbr])!
		if end_token.kind == ._rcbr {
			break
		}
	}
	p.brackets_delimiter.delete_last()
	return ast.new_node_3('{}', ast.Tuple.new(kinds), nodes)
}
