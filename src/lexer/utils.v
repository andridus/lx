module lexer

fn utils_show(msg string) string {
	println(msg)
	return msg
}

fn utils_is_digit(a u8) bool {
	return a >= `0` && a <= `9`
}

fn utils_has_underscore_before_number(a u8, b u8) bool {
	return a == `_` && utils_is_digit(b)
}

fn utils_is_letter(a u8) bool {
	return (a >= `a` && a <= `z`) || (a >= `A` && a <= `Z`) || a == `_`
}

fn utils_is_alpha(a u8) bool {
	return utils_is_digit(a) || utils_is_letter(a)
}
