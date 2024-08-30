module utils

import encoding.binary

pub struct DataBytes {
mut:
	data         []u8
	current_pos  int
	current_line int
}

pub fn DataBytes.init(source []u8) DataBytes {
	return DataBytes{
		data: source
	}
}

pub fn (mut b DataBytes) is_empty_file() bool {
	for i in 0 .. b.data.len {
		if b.data[i] !in [32, 10] {
			return false
		}
	}
	return true
}

pub fn (mut b DataBytes) backwards_bytes(num int) {
	if b.current_pos > 0 {
		b.current_pos -= num
	}
}

pub fn (mut b DataBytes) length() int {
	return b.data.len
}

pub fn (mut b DataBytes) current_line() int {
	return b.current_line
}

pub fn (mut b DataBytes) current_pos() int {
	return b.current_pos
}

pub fn (mut b DataBytes) eof() bool {
	return b.current_pos == b.data.len
}

pub fn (mut b DataBytes) current() u8 {
	if b.current_pos == 0 {
		return b.data[0]
	} else {
		return b.data[b.current_pos - 1]
	}
}

pub fn (mut b DataBytes) peek_next() !u8 {
	if b.eof() {
		return error('EOF')
	}
	return b.data[b.current_pos]
}

pub fn (mut b DataBytes) get_next_byte() !u8 {
	bytes := b.get_next_bytes(1)!
	return bytes[0]
}

pub fn (mut b DataBytes) get_while_number() []u8 {
	mut val := b.get_next_byte() or { return [] }
	mut numbers := []u8{}
	for val >= 48 && val <= 57 {
		numbers << val
		val = b.get_next_byte() or { return numbers }
	}
	return numbers
}

pub fn (mut b DataBytes) get_next_bytes(bytes u32) ![]u8 {
	from := b.current_pos
	to := b.current_pos + bytes
	if to <= b.data.len {
		b.current_pos = to
		r := b.data[from..to]
		if 10 in r {
			b.current_line += 1
		}
		return r
	} else {
		return error('EOF')
	}
}

pub fn (mut b DataBytes) get_all_next_bytes() ![]u8 {
	from := b.current_pos
	if from < b.data.len {
		return b.data[from..]
	}
	return error('EOF')
}

pub fn (mut b DataBytes) get_next_u32() !u32 {
	t := b.get_next_bytes(4)!
	return binary.big_endian_u32(t)
}

pub fn to_big_endian_32(t []u8) u32 {
	return binary.big_endian_u32(t)
}

pub fn big_endian_32_to_bytes(u u32) []u8 {
	mut a := []u8{}
	binary.big_endian_put_u32(mut a, u)
	return a
}

pub fn (mut b DataBytes) get_next_u16() !u16 {
	t := b.get_next_bytes(2)!
	return binary.big_endian_u16(t)
}

pub fn (mut b DataBytes) expect_match(list []u8) ! {
	next := b.get_next_bytes(u8(list.len))!
	if next != list {
		return error('doesn\'t match term ${next} with ${list}')
	}
}

pub fn (mut b DataBytes) peek_expect_match(list []u8) bool {
	return b.data[(b.current_pos - 1)..(b.current_pos + list.len - 1)] == list
}

pub fn (mut b DataBytes) ignore_bytes(total int) ! {
	b.get_next_bytes(u8(total))!
}
