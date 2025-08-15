.PHONY: all clean compile test install uninstall

all: compile

compile: generate-parser
	rebar3 compile

generate-parser:
	@echo "Generating parser from yecc..."
	@erlc -o src/ src/lx_parser.yrl
	@echo "Generating lexer from leex..."
	@erlc -o src/ src/lx_lexer.xrl
	@echo "Parser and lexer generated successfully!"

clean:
	rebar3 clean

test: compile
	rebar3 eunit

test-verbose: compile
	rebar3 eunit -v

test-coverage: compile
	rebar3 eunit --cover

install: compile
	rebar3 escriptize
	@echo "Installing lx to /usr/local/bin..."
	@sudo cp _build/default/bin/lx /usr/local/bin/
	@sudo chmod +x /usr/local/bin/lx
	@echo "lx installed successfully! You can now use 'lx' from anywhere."

uninstall:
	@echo "Uninstalling lx..."
	@sudo rm -f /usr/local/bin/lx
	@echo "lx uninstalled successfully!"

run-example: compile
	./lx examples/literals.lx

run-macro-test: compile
	./lx examples/macro_test.lx