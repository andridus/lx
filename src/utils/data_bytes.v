module utils

import encoding.binary
import color

pub struct DataBytes {
mut:
	data            []u8
	current_pos     int
	total_lines     int
	lines_start_pos []int
	current_line    int = 1
}

pub fn DataBytes.init(source []u8) DataBytes {
	total_lines, lines_start_pos := count_lines(source)
	return DataBytes{
		data:            source
		total_lines:     total_lines
		lines_start_pos: lines_start_pos
	}
}

pub fn count_lines(source []u8) (int, []int) {
	mut total_lines := 0
	mut lines_start_pos := [0]
	mut idx := 0
	for i in source {
		if i == 10 {
			total_lines++
			lines_start_pos << idx
		}
		idx++
	}
	return total_lines, lines_start_pos
}

pub fn (mut b DataBytes) get_lines_about(num int) []string {
	mut lines := []string{}
	if b.total_lines == 1 {
		mut line := []u8{}
		mut line2 := []u8{}
		mut pos := 0
		for i in b.data {
			if i != 10 {
				line << i
				pos++
				if b.current_pos == pos {
					line2 << `^`
				} else {
					line2 << ` `
				}
			}
		}
		lines << line.bytestr()
		lines << line2.bytestr()
	} else {
		mut lines_before := b.current_line - num
		mut lines_after := b.current_line + num
		if lines_before <= 0 {
			lines_before = 1
		}
		if lines_after > b.total_lines {
			lines_after = b.total_lines
		}
		mut actual_line := lines_before
		mut pos := b.lines_start_pos[actual_line - 1] + 1
		for actual_line <= lines_after {
			mut line := []u8{}
			mut line2 := []u8{}
			mut add_line_2 := false

			for i in b.data[b.lines_start_pos[actual_line - 1]..b.lines_start_pos[actual_line]] {
				if i != 10 {
					if b.current_pos == pos {
						line << color.fg(.red, .default, [i].bytestr()).bytes()
						line2 << color.fg(.red, .default, '^').bytes()
						add_line_2 = true
					} else {
						line << i
						line2 << ` `
					}
				}
				pos++
			}
			lines << '   ${actual_line}\t| ${line.bytestr()}'
			if add_line_2 {
				lines << '   \t| ${line2.bytestr()}'
			}
			actual_line++
		}
	}

	return lines
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
