module epp

import ast

interface Term {
	tag u8
	len int
	to_string() string
}

interface Number {
	Term
	is_number bool
}

pub struct E_Atom {
	tag u8 = u8(100)
	len int = 2
	opt E_Atom_Kind = .utf8_ext
	value string
}

pub struct E_Integer {
	tag u8 = u8(98)
	len int = 4
	opt E_Integer_Kind = .int32
	value int
}

pub struct E_Float {
	tag u8 = u8(99)
	len int = 31
	value f64
}

pub enum E_Atom_Kind {
	ext
	small_ext
	cache_ref
	utf8_ext
}

pub enum E_Integer_Kind {
	int32
	uint8
	big
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

pub fn new_integer(n int, kind E_Integer_Kind) Term {
	return match kind {
		.uint8 { E_Integer{value: n, opt: .uint8} }
		.int32 { E_Integer{value: u8(n), opt: .int32} }
		.big { E_Integer{value: n, opt: .big}}
	}
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
