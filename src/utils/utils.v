module utils

pub fn utils_show(msg string) string {
	println(msg)
	return msg
}

pub fn utils_is_digit(a u8) bool {
	return a >= `0` && a <= `9`
}

pub fn utils_has_char_before_number(a u8, b u8, c u8) bool {
	return a == b && utils_is_digit(c)
}

pub fn utils_is_letter(a u8) bool {
	return (a >= `a` && a <= `z`) || (a >= `A` && a <= `Z`) || a == `_`
}

pub fn utils_is_alpha(a u8) bool {
	return utils_is_digit(a) || utils_is_letter(a)
}
