module ast

// Position represents a specific location in source code
pub struct Position {
pub:
	line     int    // 1-indexed line number
	column   int    // 1-indexed column number
	filename string // Source file name
}

// Span represents a range of positions in source code
pub struct Span {
pub:
	start Position // Start position
	end   Position // End position
}

// new_position creates a new Position
pub fn new_position(line int, column int, filename string) Position {
	return Position{
		line:     line
		column:   column
		filename: filename
	}
}

// new_span creates a new Span from start and end positions
pub fn new_span(start Position, end Position) Span {
	return Span{
		start: start
		end:   end
	}
}

// str returns a string representation of Position
pub fn (p Position) str() string {
	return '${p.filename}:${p.line}:${p.column}'
}

// str returns a string representation of Span
pub fn (s Span) str() string {
	return '${s.start.str()}-${s.end.str()}'
}

// is_valid checks if the position is valid (positive line and column)
pub fn (p Position) is_valid() bool {
	return p.line > 0 && p.column > 0
}

// is_valid checks if the span is valid
pub fn (s Span) is_valid() bool {
	return s.start.is_valid() && s.end.is_valid()
}
