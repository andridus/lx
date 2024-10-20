module utils

pub fn show(msg string) string {
	println(msg)
	return msg
}

pub fn is_digit(a u8) bool {
	return a >= `0` && a <= `9`
}

pub fn has_char_before_number(a u8, b u8, c u8) bool {
	return a == b && is_digit(c)
}

pub fn is_letter(a u8) bool {
	return (a >= `a` && a <= `z`) || (a >= `A` && a <= `Z`) || a == `_`
}

pub fn is_alpha(a u8) bool {
	return is_digit(a) || is_letter(a)
}
