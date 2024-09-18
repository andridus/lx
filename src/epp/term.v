module epp

// import ast
import binary
import reader
import math
// import math.big
import strconv

const tag_version = u8(131)
const tag_compressed_zlib = u8(80)
const tag_new_float_ext = u8(70)
const tag_bit_binary_ext = u8(77)
const tag_atom_cache_ref = u8(78)
const tag_new_pid_ext = u8(88)
const tag_new_port_ext = u8(89)
const tag_newer_reference_ext = u8(90)
const tag_small_integer_ext = u8(97)
const tag_integer_ext = u8(98)
const tag_float_ext = u8(99)
const tag_atom_ext = u8(100)
const tag_reference_ext = u8(101)
const tag_port_ext = u8(102)
const tag_pid_ext = u8(103)
const tag_small_tuple_ext = u8(104)
const tag_large_tuple_ext = u8(105)
const tag_nil_ext = u8(106)
const tag_string_ext = u8(107)
const tag_list_ext = u8(108)
const tag_binary_ext = u8(109)
const tag_small_big_ext = u8(110)
const tag_large_big_ext = u8(111)
const tag_new_fun_ext = u8(112)
const tag_export_ext = u8(113)
const tag_new_reference_ext = u8(114)
const tag_small_atom_ext = u8(115)
const tag_map_ext = u8(116)
const tag_fun_ext = u8(117)
const tag_atom_utf8_ext = u8(118)
const tag_small_atom_utf8_ext = u8(119)
const tag_v4_port_ext = u8(120)
const tag_local_ext = u8(121)

interface Term {
	tag   u8
	bytes int
	to_string() string
}

// interface Number {
// 	Term
// 	is_number bool
// }

pub struct E_Atom {
	tag   u8  = tag_small_atom_utf8_ext
	bytes int = 2
	value string
}

pub struct E_Nil {
	tag   u8  = tag_nil_ext
	bytes int = 2
	value u8  = u8(0)
}

pub struct E_Boolean {
	tag   u8  = tag_small_atom_utf8_ext
	bytes int = 2
	value bool
}

pub struct E_Integer {
	tag   u8  = tag_small_integer_ext
	bytes int = 4
	value int
}

pub struct E_Float {
	tag   u8  = tag_new_float_ext
	bytes int = 8
	value f64
}

pub struct E_String {
	tag   u8  = tag_string_ext
	bytes int = 8
	value string
}

pub struct E_Tuple {
	tag   u8 = tag_small_tuple_ext
	bytes int
pub mut:
	value []Term
}

pub struct E_List {
	tag   u8 = tag_list_ext
	bytes int
pub mut:
	value []Term
}

pub fn new_list(list []Term) Term {
	return E_List{
		value: list
	}
}

pub fn new_integer(n int) Term {
	if n < 256 {
		return E_Integer{
			value: u8(n)
			bytes: 1
			tag:   tag_small_integer_ext
		}
	} else {
		return E_Integer{
			value: n
			bytes: 4
			tag:   tag_integer_ext
		}
	}
	// return match kind {
	// 	.uint8 {  }
	// 	.int32 {  }
	// 	// .small_big { E_Integer{value: n, len: 11, opt: .small_big, tag: u8(E_Integer_Kind.small_big)}}
	// 	// .large_big { E_Integer{value: n, len: 15, opt: .large_big, tag: u8(E_Integer_Kind.large_big)}}
	// }
}

pub fn new_nil() Term {
	return E_Nil{}
}

pub fn new_float(n f64) Term {
	return E_Float{
		value: n
	}
}

pub fn new_tuple(list []Term) Term {
	return E_Tuple{
		value: list
	}
}

pub fn new_atom(s string) Term {
	return E_Atom{
		value: s
	}
}

pub fn new_boolean(s bool) Term {
	return E_Boolean{
		value: s
	}
}

pub fn new_string(s string) Term {
	return E_String{
		value: s
	}
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

pub fn (term E_String) to_string() string {
	return term.value.str()
}

pub fn (term E_Boolean) to_string() string {
	if term.value {
		return 'true'
	} else {
		return 'false'
	}
}

pub fn (term E_Nil) to_string() string {
	return 'nil'
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

pub fn term_to_binary(term Term) ![]u8 {
	return match term {
		E_Nil {
			[tag_version, tag_small_atom_utf8_ext, 3, 110, 105, 108]
		}
		E_Float {
			match term.tag {
				tag_new_float_ext {
					mut buf := binary.big_endian.put_u64(u64(math.f64_bits(f64(term.value))))
					buf.prepend(tag_new_float_ext)
					buf.prepend(tag_version)
					buf
				}
				tag_float_ext {
					fstr := strconv.f64_to_str_l(term.value).bytes()
					mut buf := []u8{len: 31 - fstr.len}
					buf.prepend(fstr)
					buf.prepend(tag_float_ext)
					buf.prepend(tag_version)
					buf
				}
				else {
					[u8(0)]
				}
			}
		}
		E_Integer {
			match term.tag {
				tag_small_integer_ext {
					[tag_version, term.tag, u8(term.value)]
				}
				tag_integer_ext {
					mut buf := binary.write_int(term.value, binary.big_endian)
					buf.prepend(term.tag)
					buf.prepend(tag_version)
					buf
				}
				tag_small_big_ext {
					[tag_version, term.tag, u8(term.value)]
				}
				tag_large_big_ext {
					[tag_version, term.tag, u8(term.value)]
				}
				else {
					[u8(0)]
				}
			}
		}
		E_Atom {
			length := term.value.len
			mut buf := term.value.bytes()
			if length < u64(1) << 8 {
				buf.prepend(binary.big_endian.put_u8(u8(length)))
				buf.prepend(term.tag) // is it Atom only small?
				buf.prepend(tag_version)
				buf
			} else {
				error('atom length must be less than system: ${term.value}')
			}
		}
		E_Boolean {
			mut value := 'true'
			if !term.value {
				value = 'false'
			}
			length := value.len
			mut buf := value.bytes()
			buf.prepend(binary.big_endian.put_u8(u8(length)))
			buf.prepend(term.tag) // is it Atom only small?
			buf.prepend(tag_version)
			buf
		}
		E_String {
			length := term.value.len
			if length == 0 {
				[tag_version, tag_binary_ext, 0, 0, 0, 0]
			} else {
				mut buf := term.value.bytes()
				buf.prepend(binary.big_endian.put_u32(u32(length)))
				buf.prepend(tag_binary_ext)
				buf.prepend(tag_version)
				buf
			}
		}
		else {
			return error('ERROR')
		}
	}
}

pub fn binary_to_term(bin []u8) !Term {
	size := bin.len
	if size <= 1 {
		return error('invalid external representation of a term')
	}
	mut r := reader.new_reader(bin)
	version := r.read_byte() or { return err }
	if version != tag_version {
		return error('invalid version')
	}
	term := do_binary_to_term(size, mut r) or { return err }
	return term
}

fn do_binary_to_term(size int, mut r reader.Reader) !Term {
	mut total_bytes := 1
	tag := r.read_byte() or { return err }
	total_bytes++
	term := match tag {
		tag_small_atom_ext, tag_small_atom_utf8_ext {
			val := binary.read_u8(mut r, binary.big_endian)!
			bytes := int(val)
			total_bytes += bytes + 1
			mut value := []u8{len: bytes}

			if bytes > 0 {
				value = r.read_bytes(bytes)!
			}
			str := value.bytestr()
			match str {
				'nil' {
					new_nil()
				}
				'true' {
					new_boolean(true)
				}
				'false' {
					new_boolean(false)
				}
				'undefined' {
					new_nil()
				}
				else {
					match tag {
						tag_atom_ext, tag_atom_utf8_ext, tag_small_atom_utf8_ext {
							new_atom(str)
						}
						else {
							return error('Invalid tag clause')
						}
					}
				}
			}
		}
		tag_small_integer_ext {
			bytes := 1
			val := r.read_byte() or { return err }
			total_bytes++
			Term(E_Integer{
				value: val
				bytes: bytes
				tag:   tag_small_integer_ext
			})
		}
		tag_integer_ext {
			bytes := 4
			val := binary.read_i32(mut r, binary.big_endian)!
			total_bytes += bytes
			Term(E_Integer{
				value: val
				bytes: bytes
				tag:   tag_integer_ext
			})
		}
		tag_new_float_ext {
			bytes := 8
			val := binary.read_f64(mut r, binary.big_endian)!
			total_bytes += bytes
			Term(E_Float{
				value: val
				bytes: bytes
				tag:   tag_new_float_ext
			})
		}
		tag_float_ext {
			bytes := 31
			val := r.read_bytes(bytes)!
			fvalue := strconv.atof64(val.bytestr())!
			total_bytes += bytes
			Term(E_Float{
				value: fvalue
				bytes: bytes
				tag:   tag_float_ext
			})
		}
		tag_string_ext {
			val := binary.read_u16(mut r, binary.big_endian)!
			bytes := int(val)
			total_bytes += bytes
			mut value := []u8{len: bytes}

			if bytes > 0 {
				value = r.read_bytes(bytes)!
			}
			str := value.bytestr()
			Term(E_String{
				value: str
				bytes: bytes
			})
		}
		tag_binary_ext {
			val := binary.read_u32(mut r, binary.big_endian)!
			bytes := int(val)
			total_bytes += bytes + 4
			mut value := []u8{len: bytes}
			if bytes > 0 {
				value = r.read_bytes(bytes)!
			}
			new_string(value.bytestr())
		}
		else {
			return error('Invalid TAG')
		}
	}
	if total_bytes == size {
		return term
	} else {
		return error('Unparsed Data')
	}
}
