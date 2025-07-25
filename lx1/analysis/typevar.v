module analysis

pub struct TypeVar {
	id   int
	name string
}

pub fn new_type_var(id int) TypeVar {
	return TypeVar{
		id:   id
		name: 'T${id}'
	}
}
