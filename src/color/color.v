// Copyright (c) 2023 Helder de Sousa. All rights reserved/
// Use of this source code is governed by a MIT license
// that can be found in the LICENSE file
module color

pub enum Color {
	black = 30
	dark_red
	dark_green
	dark_yellow
	dark_blue
	dark_magenta
	dark_cyan
	default    = 39
	light_gray = 37
	dark_gray  = 90
	red
	green
	orange
	blue
	magenta
	cyan
	white
}

pub enum Decoration {
	default = 0
	bold
	light
	underline = 4
	blinking
}

pub const prefix = '\033['
pub const suffix = 'm'
pub const reset = '\033[0m'

pub fn fg(c Color, decor Decoration, text string) string {
	return '${color.prefix}${int(decor)}${color.suffix}${color.prefix}${int(c)}${color.suffix}${text}${color.reset}'
}
