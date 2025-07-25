module main

import os
import lexer
import parser
import analysis
import generator

fn main() {
    if os.args.len < 2 {
        eprintln('Usage: lx1 <file.lx>')
        eprintln('       lx1 --version')
        eprintln('       lx1 --help')
        exit(1)
    }

    arg := os.args[1]

    match arg {
        '--version' {
            println('LX1 Compiler v0.1.0 - Task 1: Functions with Literals')
            exit(0)
        }
        '--help' {
            print_help()
            exit(0)
        }
        else {
            compile_file(arg)
        }
    }
}

fn print_help() {
    println('LX1 Compiler - Task 1: Functions with Literals')
    println('')
    println('USAGE:')
    println('    lx1 <file.lx>    Compile LX file to Erlang')
    println('    lx1 --version    Show version information')
    println('    lx1 --help       Show this help message')
    println('')
    println('SUPPORTED SYNTAX (Task 1):')
    println('    def function_name() do')
    println('        literal_value')
    println('    end')
    println('')
    println('SUPPORTED LITERALS:')
    println('    - Integers: 42, -10, 0')
    println('    - Floats: 3.14, -2.5, 0.0')
    println('    - Strings: "Hello, World!"')
    println('    - Booleans: true, false')
    println('    - Atoms: :ok, :error, :success')
    println('    - Nil: nil')
    println('')
    println('EXAMPLES:')
    println('    def answer() do')
    println('        42')
    println('    end')
    println('')
    println('    def greeting() do')
    println('        "Hello, World!"')
    println('    end')
}

fn compile_file(file_path string) {
    // Check if file exists
    if !os.exists(file_path) {
        eprintln('Error: File "${file_path}" not found')
        exit(1)
    }

    // Read file content
    content := os.read_file(file_path) or {
        eprintln('Error reading file "${file_path}": ${err}')
        exit(1)
    }

    if content.trim_space().len == 0 {
        eprintln('Error: File "${file_path}" is empty')
        exit(1)
    }

    // Compile the content
    result := compile_lx_code(content, file_path) or {
        eprintln('Compilation failed: ${err}')
        exit(1)
    }

    // Output the result
    println(result)
}

fn compile_lx_code(code string, file_path string) !string {
    // Lexing
    mut lex := lexer.new_lexer(code, file_path)

    // Parsing
    mut p := parser.new_parser(mut lex)
    ast_node := p.parse() or {
        errors := p.get_errors()
        if errors.len > 0 {
            return error('Parse errors:\n${errors.join('\n')}')
        }
        return error('Parse error: ${err}')
    }

    // Check for parser errors
    parser_errors := p.get_errors()
    if parser_errors.len > 0 {
        return error('Parse errors:\n${parser_errors.join('\n')}')
    }

    // Analysis
    mut analyzer := analysis.new_analyzer()
    analyzed_ast := analyzer.analyze(ast_node) or {
        errors := analyzer.get_errors()
        if errors.len > 0 {
            return error('Analysis errors:\n${errors.join('\n')}')
        }
        return error('Analysis error: ${err}')
    }

    // Check for analysis errors
    analysis_errors := analyzer.get_errors()
    if analysis_errors.len > 0 {
        return error('Analysis errors:\n${analysis_errors.join('\n')}')
    }

    // Generation
    mut gen := generator.new_generator()
    erlang_code := gen.generate(analyzed_ast) or {
        errors := gen.get_errors()
        if errors.len > 0 {
            return error('Generation errors:\n${errors.join('\n')}')
        }
        return error('Generation error: ${err}')
    }

    // Check for generation errors
    generation_errors := gen.get_errors()
    if generation_errors.len > 0 {
        return error('Generation errors:\n${generation_errors.join('\n')}')
    }

    return erlang_code
}