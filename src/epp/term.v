module epp

import ast
import binary

const tag_version = u8(131)
interface Term {
	tag u8
	len int
	to_string() string
}

interface Number {
	Term
	is_number bool
}

pub enum E_Atom_Kind {
	cache_ref = 78
	ext = 100
	small_ext = 115
	utf8_ext = 118
}

pub enum E_Integer_Kind {
	uint8 = 97
	int32 = 98
	small_big = 110
	large_big = 111
}

pub struct E_Atom {
	tag u8 = u8(E_Atom_Kind.utf8_ext)
	len int = 2
	opt E_Atom_Kind = .utf8_ext
	value string
}

pub struct E_Nil {
	tag u8 = u8(100)
	len int = 2
	value u8 = u8(0)
}

pub struct E_Boolean {
	tag u8 = u8(100)
	len int = 2
	value bool
}

pub struct E_Integer {
	tag u8 = u8(E_Integer_Kind.int32)
	len int = 4
	opt E_Integer_Kind = .int32
	value int
}

pub struct E_Float {
	tag u8 = u8(99)
	len int = 31
	value f64
}

pub struct E_Tuple {
	tag u8
	len int
pub mut:
	value []Term
}

pub struct E_List {
	tag u8
	len int
pub mut:
	value []Term
}

pub fn new_list(list []Term) Term {
	return E_List{value: list}
}

pub fn new_integer(n int) Term {
	if n < 256 {
		return E_Integer{value: u8(n), len: 1, opt: .uint8, tag: u8(E_Integer_Kind.uint8)}
	} else {
		return E_Integer{value: n, len: 4, opt: .int32, tag: u8(E_Integer_Kind.int32)}
	}
	// return match kind {
	// 	.uint8 {  }
	// 	.int32 {  }
	// 	// .small_big { E_Integer{value: n, len: 11, opt: .small_big, tag: u8(E_Integer_Kind.small_big)}}
	// 	// .large_big { E_Integer{value: n, len: 15, opt: .large_big, tag: u8(E_Integer_Kind.large_big)}}
	// }
}

pub fn new_float(n f64) Term {
	return E_Float{value: n}
}

pub fn new_tuple(list []Term) Term {
	return E_Tuple{value: list}
}

pub fn new_atom(s string) Term {
	return E_Atom{value: s}
}

fn (mut t Term) append(element Term) {
	match t {
		E_Tuple {
			mut tuple := t as E_Tuple
			tuple.append(element)
		}
		E_List {
			mut list := t as E_List
			list.append(element)
		}
		else {}
	}
}

fn (mut t E_Tuple) append(element Term) {
	t.value << element
}

fn (mut l E_List) append(element Term) {
	l.value << element
}

pub fn (term E_Integer) to_string() string {
	return term.value.str()
}

pub fn (term E_Float) to_string() string {
	return term.value.str()
}

pub fn (term E_Atom) to_string() string {
	return term.value.str()
}

pub fn (term E_Tuple) to_string() string {
	items := term.value.map(|e| e.to_string()).join(',')
	return '{${items}}'
}

@[inline]
pub fn (term E_List) to_string() string {
	mut items := term.value.map(|e| e.to_string()).join(',')
	return '[${items}]'
}

@[inline]
pub fn (many []Term) to_string() string {
	return many.map(|m| m.to_string()).join('.\n') + '.'
}



///// functions

pub fn term_to_binary(term Term) []u8 {
	return match term {
		E_Integer {
			match term.opt {
				.uint8 { [tag_version, term.tag, u8(term.value)]}
				.int32 {
					mut buf := binary.write_int(term.value, binary.big_endian)
					buf.prepend(term.tag)
					buf.prepend(tag_version)
					buf
				}
				.small_big { [tag_version, term.tag, u8(term.value)]}
				.large_big { [tag_version, term.tag, u8(term.value)]}
			}

		}
		else {
			[u8(0)]
		}
	}
}