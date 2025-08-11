.PHONY: all clean compile test generate-lexer generate-parser generate

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit

# Generate lexer with custom functions
generate-lexer:
	erlc -o src leex/lx_lexer.xrl
	sh scripts/patch_lexer.sh

# Generate parser
generate-parser:
	erlc -o src yecc/lx_parser.yrl

# Generate both lexer and parser
generate: generate-lexer generate-parser
	erlc -I include src/lx_lexer.erl src/lx_parser.erl

# Run example
example:
	rebar3 shell --eval "lx:compile(\"x = 42\")."