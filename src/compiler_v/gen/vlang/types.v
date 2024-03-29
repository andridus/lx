// Copyright (c) 2023 Helder de Sousa. All rights reserved/
// Use of this source code is governed by a MIT license
// that can be found in the LICENSE file
module vlang

import compiler_v.types

fn parse_type(kind types.Kind) string {
	return match kind {
		.atom_ { 'Atom' }
		.void_ { 'Nil' }
		.nil_ { 'Nil' }
		.any_ { 'AnyType' }
		.pointer_ { 'void *' }
		.enum_ { 'enum' }
		.struct_ { 'struct' }
		.result_ { 'result' }
		.integer_ { 'int' }
		.float_ { 'f64' }
		.string_ { 'string' }
		.char_ { 'rune' }
		.bool_ { 'bool' }
		.list_ { 'list' }
		.list_fixed_ { 'list' }
		.tuple_ { 'tuple' }
		.map_ { 'map' }
		.sum_ { 'sum' }
	}
}
