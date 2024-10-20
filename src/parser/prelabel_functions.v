module parser

import ast
import time
import utils

struct Xref {
	mut:
	functions []ast.FunctionLabel
	caller_functions []ast.FunctionCallerLabel
	source &Source
}
fn (mut p Parser) prelabel_functions() ! {
	mut source := Source{data: p.lexer.source()}
	start := time.now()
	mut xref := Xref{
		source: &source
	}
	for {
		mut mod := []u8{}
		mut term := []u8{}
		xref.source.next_match('defmodule ') or { break }
		xref.source.skip_white() or { break }
		mod = xref.source.get_until(` `) or { break }
		xref.source.next_match('do') or { break }
		xref.source.ignore_comment() or { break }
		for {
			xref.source.ignore_comment() or { break }
			if xref.source.has_blank_line() { xref.source.i++;  continue }
			term = xref.source.get_next_word() or { break }
			match term.bytestr() {
				'@moduledoc' {
					xref.source.skip_white()!
					if xref.source.peek_next( '\"\"\"' ) {
						xref.source.next_match('\"\"\"') or { break }
					} else {
						_ = xref.source.get_until( `\n`) or { break }
					}
					continue
				}
				'@doc' {
					xref.source.skip_white()!
					if xref.source.peek_next('\"\"\"' ) {
						xref.source.next_match( '\"\"\"') or { break }
					} else {
					  _ = xref.source.get_until(`\n`) or { break }
					}
					continue
				}
				'def', 'defp' {
					xref.analyze_function(mod, term == 'defp'.bytes()) or { break }
				}
				'' {
					break
				}
				else {
					_ = xref.source.get_until( `\n`) or { break }
					continue
				}
			}
		}
		xref.source.next_match('end') or { break }
		xref.source.i++
	}
	elapsed := time.since(start)
	println(xref)
	println('Tempo de execução: $elapsed')
}

struct Source {
	data []u8
	mut:
	i int
}


fn (mut xref Xref) analyze_function(mod []u8, is_private bool) ! {
	mut args := []string{}
	mut returns := []u8{}
	mut pos_start := 0
	mut pos_end := 0
	mut next_stmt := false
	name := xref.source.get_next_word()!
	// maybe get args of function
	if xref.source.peek_next('(' ) {
		for xref.source.i < xref.source.data.len {
			arg, end_char := xref.source.get_until_or([u8(`,`), `)`])!
			args << arg.bytestr()
			if end_char == `)` {
				break
			}
			xref.source.i++
		}
	}
		xref.source.skip_white()!
		if xref.source.peek_next('::' ) {
			returns = xref.source.get_until(` `)!
		} else {
			returns = [u8(`a`), `n`, `y`]
		}
		xref.source.skip_white()!
		mut expect_ends := 0
		pos_start = xref.source.i
		if xref.source.peek_next(',' ) {
			next_stmt = true
		}
		xref.source.skip_white()!

		for {
			xref.source.ignore_comment()!

			if xref.source.peek_next('do' ){
				if xref.source.peek_next(':' ){
				next_stmt = true
				} else {
					expect_ends++
				}
			} else if xref.source.peek_next('end'){
				expect_ends--
			} else {
				c1 := xref.source.get_caller_function(mod, name.bytestr())
				if c := c1 {
					xref.caller_functions << c
				}
			}
			xref.source.i++
			if next_stmt == true {
				xref.source.get_next_stmt()!
			}
			if expect_ends == 0 {
				break
			}
		}

		pos_end = xref.source.i
		xref.functions << ast.FunctionLabel{
			name:      name.bytestr()
			mod:       mod.bytestr()
			pos_start: u32(pos_start)
			pos_end:   u32(pos_end)
			args:      args
			returns:   returns.bytestr()
			is_private: is_private
		}
}
fn (mut s Source) ignore_comment() ! {
	if s.i < s.data.len && s.data[s.i] == `#` {
		s.get_until(`\n`)!
	}
}
fn (mut s Source) has_blank_line() bool {
	return s.i+1 < s.data.len && s.data[s.i] == `\n`
}

fn (mut s Source) peek_next(cs string) bool {
	u8s := cs.bytes()
	mut r := false

	for idx,c in u8s {
		if s.i + idx < s.data.len && s.data[s.i + idx] == c {
			if (idx+1) == cs.len {
				r = true
				s.i += idx+1
				break
			}
			continue
		} else {
			break
		}

	}
	return r
}
fn (mut s Source) get_caller_function(mod []u8, fun_name string) ?ast.FunctionCallerLabel {
	if s.data[s.i] == `(` {
		mut fun_caller_name := []u8{}
		mut caller_args := []string{}
		mut pos_start_caller := 0
		mut pos_end_caller := 0
		mut j := s.i - 1
		for j > 0 {
			if s.data[j] != ` ` {
				fun_caller_name << s.data[j]
				j--
			} else {
				pos_start_caller = j
				break
			}
		}
		s.i++
		for s.i < s.data.len {
			mut arg := []u8{}
			mut end_char := u8(0)
			arg, end_char = s.get_until_with_parens([u8(`,`), `)`]) or {break}
			caller_args << arg.bytestr()
			if end_char == `)` {
				break
			}
			s.i++
		}
		pos_end_caller = s.i
		return ast.FunctionCallerLabel{
			name:       fun_caller_name.reverse().bytestr()
			pos_start:  u32(pos_start_caller)
			pos_end:    u32(pos_end_caller)
			args:       caller_args
			parent_fun: fun_name
			mod:        mod.bytestr()
		}
	}
	return none
}

fn (mut s Source) skip_white()! {
	for s.i < s.data.len {
		if s.data[s.i] == 32 {
			s.i++
		} else {
			break
		}
	}
}

fn (mut s Source) get_next_stmt()!{
	for s.i < s.data.len {
		if s.data[s.i] in [`\n`] {
			s.i++
		} else {
			break
		}
	}
}
fn (mut s Source) next_match(c string) ! {
	s.skip_white()!
	for {
		if s.i + c.len >= s.data.len {
			return error('eof')
		}
		if s.data[s.i..(s.i + c.len)] == c.bytes() {
			s.i += c.len
			break
		}
		s.i++
	}
}

fn (mut s Source) get_next_word() ![]u8 {
	mut word := []u8{}
	s.skip_white()!
	for s.i < s.data.len {
		if utils.is_alpha(s.data[s.i]) || s.data[s.i] in [`@`, `:`]  {
			word << s.data[s.i]
		} else {
			break
		}
		s.i++
	}
	return word
}

fn (mut s Source) get_until(c u8) ![]u8 {
	s.skip_white()!
	mut data := []u8{}
	for s.i < s.data.len {
		if s.data[s.i] == c {
			break
		}
		data << s.data[s.i]
		s.i++
	}
	return data
}
fn (mut s Source) get_until_with_parens(c []u8) !([]u8, u8) {
	s.skip_white()!
	mut data := []u8{}
	mut parens := 0
	for s.i < s.data.len {
		if s.data[s.i] == `(` {
			parens++
		}
		if s.data[s.i] == `)` {
			parens--
		}
		if s.data[s.i] in c {
			// $dbg
			if parens == -1 {
				break
			}
		}
		data << s.data[s.i]
		s.i++
	}
	return data, s.data[s.i]
}
fn (mut s Source) get_until_or(c []u8) !([]u8, u8) {
	mut data := []u8{}
	for s.i < s.data.len {
		if s.data[s.i] in c {
			break
		}
		data << s.data[s.i]
		s.i++
	}
	return data, s.data[s.i]
}

fn printc(c u8) {
	println([c].bytestr())
}
fn printc1(c []u8) {
	println(c.bytestr())
}