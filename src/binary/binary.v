module binary

import reader
import math

pub const little_endian = LittleEndian{}
pub const big_endian = BigEndian{}

interface ByteOrder {
	u16([]u8) u16
	u32([]u8) u32
	u64([]u8) u64
	put_u64(u64) []u8
	put_u32(u32) []u8
	put_u8(u8) []u8
}

pub struct LittleEndian {}

pub fn (_le LittleEndian) u16(b []u8) u16 {
	return u16(b[0]) | u16(b[1]) << 8
}

pub fn (_le LittleEndian) u32(b []u8) u32 {
	return u32(b[0]) | u32(b[1]) << 8 | u32(b[2]) << 16 | u32(b[3]) << 24
}

pub fn (_le LittleEndian) u64(b []u8) u64 {
	return u64(b[0]) | u64(b[1]) << 8 | u64(b[2]) << 16 | u64(b[3]) << 24 | u64(b[4]) << 32 | u64(b[5]) << 40 | u64(b[6]) << 48 | u64(b[7]) << 56
}

pub fn (_be LittleEndian) put_u32(v u32) []u8 {
	mut b := []u8{}
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	return b
}

pub fn (_be LittleEndian) put_u64(v u64) []u8 {
	mut b := []u8{}
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	b << u8(v >> 32)
	b << u8(v >> 40)
	b << u8(v >> 48)
	b << u8(v >> 56)
	return b
}

pub fn (_be LittleEndian) put_u16(v u16) []u8 {
	mut b := []u8{}
	b << u8(v)
	b << u8(v >> 8)
	return b
}

pub fn (_be LittleEndian) put_u8(v u8) []u8 {
	mut b := []u8{}
	b << u8(v)
	return b
}

pub struct BigEndian {}

pub fn (_be BigEndian) u16(b []u8) u16 {
	return u16(b[1]) | u16(b[0]) << 8
}

pub fn (_be BigEndian) u32(b []u8) u32 {
	return u32(b[3]) | u32(b[2]) << 8 | u32(b[1]) << 16 | u32(b[0]) << 24
}

pub fn (_be BigEndian) u64(b []u8) u64 {
	return u64(b[7]) | u64(b[6]) << 8 | u64(b[5]) << 16 | u64(b[4]) << 24 | u64(b[3]) << 32 | u64(b[2]) << 40 | u64(b[1]) << 48 | u64(b[0]) << 56
}

pub fn (_be BigEndian) put_u64(v u64) []u8 {
	mut b := []u8{}
	b << u8(v >> 56)
	b << u8(v >> 48)
	b << u8(v >> 40)
	b << u8(v >> 32)
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
	return b
}

pub fn (_be BigEndian) put_u32(v u32) []u8 {
	mut b := []u8{}
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
	return b
}

pub fn (_be BigEndian) put_u16(v u16) []u8 {
	mut b := []u8{}
	b << u8(v >> 8)
	b << u8(v)
	return b
}

pub fn (_be BigEndian) put_u8(v u8) []u8 {
	mut b := []u8{}
	b << u8(v)
	return b
}

pub fn write_u8(value u8, order ByteOrder) []u8 {
	return order.put_u8(value)
}

pub fn write_u32(value u32, order ByteOrder) []u8 {
	return order.put_u32(value)
}

pub fn write_int(value int, order ByteOrder) []u8 {
	return order.put_u32(u32(value))
}

fn read_from(mut r reader.Reader, data_size int) ![]u8 {
	mut buf := []u8{cap: data_size}

	for _ in 0 .. data_size {
		buf << r.read_byte()!
	}
	return buf
}

pub fn read_bool(mut r reader.Reader) !bool {
	data_size := 1
	bs := read_from(mut r, data_size)!
	return bs[0] != 0
}

pub fn read_f32(mut r reader.Reader, order ByteOrder) !f32 {
	data_size := 4
	bs := read_from(mut r, data_size)!
	return math.f32_from_bits(order.u32(bs))
}

pub fn read_f64(mut r reader.Reader, order ByteOrder) !f64 {
	data_size := 8
	bs := read_from(mut r, data_size)!
	return math.f64_from_bits(order.u64(bs))
}

// pub fn read_i8(r reader.Reader, order ByteOrder) i8 {
// 	data_size := 1

// }
// pub fn read_i16(r reader.Reader, order ByteOrder) i16 {
// 	data_size := 2
// }
pub fn read_i32(mut r reader.Reader, order ByteOrder) !int {
	data_size := 4
	bs := read_from(mut r, data_size)!
	return int(order.u32(bs))
}

// pub fn read_i64(r reader.Reader, order ByteOrder) i64 {
// 	data_size := 8
// }
pub fn read_u8(mut r reader.Reader, order ByteOrder) !u8 {
	data_size := 1
	bs := read_from(mut r, data_size)!
	return bs[0]
}

pub fn read_u16(mut r reader.Reader, order ByteOrder) !u16 {
	data_size := 2
	bs := read_from(mut r, data_size)!
	return order.u16(bs)
}

pub fn read_u32(mut r reader.Reader, order ByteOrder) !u32 {
	data_size := 4
	bs := read_from(mut r, data_size)!
	return order.u32(bs)
}
