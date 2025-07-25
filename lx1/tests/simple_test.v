module main

import lx1.lexer
import lx1.parser
import lx1.analysis
import lx1.generator

fn test_simple_integer() {
    lx_code := 'def answer() do
    42
end'

    expected := '-module(main).
-export([answer/0]).

answer() ->
    42.
'

    result := compile_lx(lx_code) or {
        assert false, 'Compilation failed: ${err}'
        return
    }

    assert result == expected
    println('✓ Simple integer test passed')
}

fn test_simple_string() {
    lx_code := 'def greeting() do
    "Hello"
end'

    result := compile_lx(lx_code) or {
        assert false, 'Compilation failed: ${err}'
        return
    }

    assert result.contains('greeting() ->')
    assert result.contains('<<"Hello"/utf8>>')
    println('✓ Simple string test passed')
}

fn compile_lx(code string) !string {
    mut lex := lexer.new_lexer(code, 'test.lx')
    mut p := parser.new_parser(mut lex)
    ast_node := p.parse()!

    mut analyzer := analysis.new_analyzer()
    analyzed_ast := analyzer.analyze(ast_node)!

    mut gen := generator.new_generator()
    return gen.generate(analyzed_ast)!
}

fn main() {
    println('Running simple tests...')
    test_simple_integer()
    test_simple_string()
    println('✅ All tests passed!')
}