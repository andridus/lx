module kernel

import ast

pub struct FunctionInfo {
pub:
	precedence    int
	associativity Associativity
	fixity        Fixity
	signatures    []TypeSignature
	gen           []map[string]string
}

pub struct TypeSignature {
pub:
	parameters  []ast.Type
	return_type ast.Type
}

pub enum Associativity {
	left
	right
}

pub enum Fixity {
	infix
	prefix
	postfix
}

// Native functions table with type signatures
pub const native_functions = {
	// Arithmetic operators (infix)
	'+':          FunctionInfo{
		precedence:    1
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'float'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 + $2'
		}]
	}
	'-':          FunctionInfo{
		precedence:    1
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'float'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 - $2'
		}]
	}
	'*':          FunctionInfo{
		precedence:    2
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'float'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 * $2'
		}]
	}
	'/':          FunctionInfo{
		precedence:    2
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'float'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 / $2'
		}]
	}
	// Comparison operators (infix)
	'==':         FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 == $2'
		}]
	}
	'!=':         FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 != $2'
		}]
	}
	'<':          FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 < $2'
		}]
	}
	'<=':         FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 <= $2'
		}]
	}
	'>':          FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 > $2'
		}]
	}
	'>=':         FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
			TypeSignature{
				parameters:  [ast.Type{
					name:   'float'
					params: []
				}, ast.Type{
					name:   'float'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 >= $2'
		}]
	}
	// Logical operators (infix)
	'and':        FunctionInfo{
		precedence:    4
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'boolean'
					params: []
				}, ast.Type{
					name:   'boolean'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 andalso $2'
		}]
	}
	'or':         FunctionInfo{
		precedence:    4
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'boolean'
					params: []
				}, ast.Type{
					name:   'boolean'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 orelse $2'
		}]
	}
	// Logical negation operator (prefix)
	'not':        FunctionInfo{
		precedence:    4
		associativity: .right
		fixity:        .prefix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'boolean'
					params: []
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': 'not $1'
		}]
	}
	// Bitwise operators (infix) - apenas integer
	'&&&':        FunctionInfo{
		precedence:    5
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 band $2'
		}]
	}
	'|||':        FunctionInfo{
		precedence:    5
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 bor $2'
		}]
	}
	'^^^':        FunctionInfo{
		precedence:    5
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 bxor $2'
		}]
	}
	'<<<':        FunctionInfo{
		precedence:    6
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 bsl $2'
		}]
	}
	'>>>':        FunctionInfo{
		precedence:    6
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'integer'
					params: []
				}]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': '$1 bsr $2'
		}]
	}
	// List concatenation operator
	'++':         FunctionInfo{
		precedence:    1
		associativity: .right
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [
					ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					},
					ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					},
				]
				return_type: ast.Type{
					name:   'list'
					params: [
						ast.Type{
							name:   'any'
							params: []
						},
					]
				}
			},
		]
		gen:           [{
			'erl': '$1 ++ $2'
		}]
	}
	// List length function
	'length':     FunctionInfo{
		precedence:    0
		associativity: .left
		fixity:        .prefix
		signatures:    [
			TypeSignature{
				parameters:  [
					ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					},
				]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': 'length($1)'
		}]
	}
	// List membership operator
	'in':         FunctionInfo{
		precedence:    3
		associativity: .left
		fixity:        .infix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'any'
					params: []
				}, ast.Type{
					name:   'list'
					params: [ast.Type{
						name:   'any'
						params: []
					}]
				}]
				return_type: ast.Type{
					name:   'boolean'
					params: []
				}
			},
		]
		gen:           [{
			'erl': 'lists:member($1, $2)'
		}]
	}
	// Tuple size function
	'tuple_size': FunctionInfo{
		precedence:    0
		associativity: .left
		fixity:        .prefix
		signatures:    [
			TypeSignature{
				parameters:  [
					ast.Type{
						name:   'tuple'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					},
				]
				return_type: ast.Type{
					name:   'integer'
					params: []
				}
			},
		]
		gen:           [{
			'erl': 'tuple_size($1)'
		}]
	}
	// Element access function
	'element':    FunctionInfo{
		precedence:    0
		associativity: .left
		fixity:        .prefix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'tuple'
					params: [ast.Type{
						name:   'any'
						params: []
					}]
				}]
				return_type: ast.Type{
					name:   'any'
					params: []
				}
			},
		]
		gen:           [{
			'erl': 'element($1, $2)'
		}]
	}
	// Set element function
	'setelement': FunctionInfo{
		precedence:    0
		associativity: .left
		fixity:        .prefix
		signatures:    [
			TypeSignature{
				parameters:  [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'tuple'
					params: [ast.Type{
						name:   'any'
						params: []
					}]
				}, ast.Type{
					name:   'any'
					params: []
				}]
				return_type: ast.Type{
					name:   'tuple'
					params: [ast.Type{
						name:   'any'
						params: []
					}]
				}
			},
		]
		gen:           [{
			'erl': 'setelement($1, $2, $3)'
		}]
	}
}

pub fn get_function_info(function_name string) ?FunctionInfo {
	return native_functions[function_name] or { return none }
}

pub fn get_precedence(function_name string) !int {
	function_info := get_function_info(function_name) or {
		return error('Unknown function: ${function_name}')
	}
	return function_info.precedence
}
