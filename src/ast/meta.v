module ast

pub struct Meta {
	line      int
	start_pos int
	end_pos   int
}

pub fn new_meta(line int, start_pos int, end_pos int) Meta {
	return Meta{line, start_pos, end_pos}
}
