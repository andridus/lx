import compiler_v
import os

pub fn test_precedence_promote_ex_file() {
	file := 'precedence_promote'
	root := os.abs_path('')
	path := '${root}/src/tests/${file}'
	filepath := '${path}/${file}.ex'
	mut bin := compiler_v.generate(path)

	// Generate HelloWorld Module
	assert 'PrecedenceTest' == bin.program.modules['PrecedenceTest'].name
	assert '{{:defmodule, [line: 1], [{:__aliases__, [line: 1], [:PrecedenceTest]},[{:do,{:def, [line: 2,type: SUM::float], [{:main, [line: 2,type: SUM::float], []},[{:do,{:+, [line: 3,type: float], [1.0,2.1]}}]]}}]]}}' == bin.program.modules['PrecedenceTest'].str()
	assert '3.1' == compiler_v.execute(mut bin)
}
