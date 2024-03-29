import compiler_v
import os

pub fn test_sum_ex_file() {
	file := 'sum'
	root := os.abs_path('')
	path := '${root}/src/tests/${file}'
	filepath := '${path}/${file}.ex'
	mut bin := compiler_v.generate(path)

	// Generate HelloWorld Module
	assert 'SumTest' == bin.program.modules['SumTest'].name
	assert '{{:defmodule, [line: 1], [{:__aliases__, [line: 1], [:SumTest]},[{:do,{:def, [line: 2,type: SUM::integer], [{:main, [line: 2,type: SUM::integer], []},[{:do,{:+, [line: 3,type: integer], [1,2]}}]]}}]]}}' == bin.program.modules['SumTest'].str()
	assert '3' == compiler_v.execute(mut bin)
}
