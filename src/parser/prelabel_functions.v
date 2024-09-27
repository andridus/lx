module parser

import ast
import utils

fn (mut p Parser) prelabel_functions() ! {
	mut functions := []ast.FunctionLabel{}
	mut caller_functions := []ast.FunctionCallerLabel{}
	source := p.lexer.source()

	mut i := 0

	for {
		mut mod := []u8{}
		i = next_match(i, source, 'defmodule ') or { break }
		skip_white(i, source) or { break }
		i, mod = get_until(i, source, ` `) or { break }
		i = next_match(i, source, 'do') or { break }
		for {
			mut name := []u8{}
			mut args := []string{}
			mut returns := []u8{}
			mut pos_start := 0
			mut pos_end := 0
			i = next_match(i, source, 'def ') or { break }
			// get name
			i, name = get_until(i, source, `(`) or { break }
			i = next_match(i, source, '(') or { break }

			// get args
			for i < source.len {
				mut arg := []u8{}
				mut end_char := u8(0)
				i, arg, end_char = get_until_or(i, source, [u8(`,`), `)`]) or { break }
				args << arg.bytestr()
				if end_char == `)` {
					break
				}
				i++
			}
			// get return
			skip_white(i, source) or { break }
			if i + 1 < source.len && source[i + 1] == `:` {
				i = next_match(i, source, '::') or { break }
				i, returns = get_until(i, source, ` `) or { break }
			} else {
				returns = [u8(`a`), `n`, `y`]
			}
			i++
			i = skip_white(i, source) or { break }
			mut expect_ends := 0
			pos_start = i
			for {
				if i <= source.len && source[i] == `d` {
					if i + 1 <= source.len && source[i + 1] == `o` {
						i++
						expect_ends++
					}
				} else if i <= source.len && source[i] == `e` {
					if i + 1 <= source.len && source[i + 1] == `n` {
						if i + 2 <= source.len && source[i + 2] == `d` {
							expect_ends--
							i += 2
						}
					}
				} else {
					j, c1 := get_caller_function(i, source, mod, name.bytestr())
					i = j
					if c := c1 {
						caller_functions << c
					}
				}
				i++
				if expect_ends == 0 {
					break
				}
			}

			pos_end = i
			functions << ast.FunctionLabel{
				name:      name.bytestr()
				mod:       mod.bytestr()
				pos_start: u32(pos_start)
				pos_end:   u32(pos_end)
				args:      args
				returns:   returns.bytestr()
			}
			println(source[i..].bytestr())
			println('----------')
			i = skip_white(i, source) or { break }
			i++
			if i + 3 < source.len {
				if source[i..i + 3] == [u8(`e`), `n`, `d`] {
					break
				}
			}
		}
		println('next module--------------------')
		println(source[i..].bytestr())
		println('close module ${mod}')
		i = next_match(i, source, 'end') or { break }
		i++
	}
	println(functions)
	println(caller_functions)
}

fn get_caller_function(k int, source []u8, mod []u8, fun_name string) (int, ?ast.FunctionCallerLabel) {
	mut i := k
	if source[i] == `(` {
		mut fun_caller_name := []u8{}
		mut caller_args := []string{}
		mut pos_start_caller := 0
		mut pos_end_caller := 0
		mut j := i - 1
		for j > 0 {
			if source[j] != ` ` {
				fun_caller_name << source[j]
				j--
			} else {
				pos_start_caller = j
				break
			}
		}
		i++
		for i < source.len {
			mut arg := []u8{}
			mut end_char := u8(0)
			i, arg, end_char = get_until_or(i, source, [u8(`,`), `)`]) or { break }
			caller_args << arg.bytestr()
			if end_char == `)` {
				break
			}
			i++
		}
		pos_end_caller = i
		return i, ast.FunctionCallerLabel{
			name:       fun_caller_name.reverse().bytestr()
			pos_start:  u32(pos_start_caller)
			pos_end:    u32(pos_end_caller)
			args:       caller_args
			inside_fun: fun_name
			mod:        mod.bytestr()
		}
	}
	return i, none
}

fn skip_white(i int, source []u8) !int {
	mut i0 := i
	for i0 < source.len {
		if source[i0] == ` ` {
			i0++
		} else {
			break
		}
	}
	return i0
}

fn next_match(i int, source []u8, c string) !int {
	mut i0 := skip_white(i, source)!
	for {
		if i0 + c.len > source.len {
			return error('eof')
		}
		if source[i0..i0 + c.len] == c.bytes() {
			i0 += c.len
			break
		}
		i0++
	}
	return i0
}

fn get_until(i int, source []u8, c u8) !(int, []u8) {
	mut i0 := skip_white(i, source)!
	mut data := []u8{}
	for i0 < source.len {
		if source[i0] == c {
			break
		}
		data << source[i0]
		i0++
	}
	return i0, data
}

fn get_until_or(i int, source []u8, c []u8) !(int, []u8, u8) {
	mut i0 := skip_white(i, source)!
	mut data := []u8{}
	for i0 < source.len {
		if source[i0] in c {
			break
		}
		data << source[i0]
		i0++
	}
	return i0, data, source[i0]
}
