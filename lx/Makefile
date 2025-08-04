.PHONY: all clean compile escript install uninstall

all: compile escript

compile:
	rebar3 compile

escript: compile
	rebar3 escriptize
	chmod +x _build/default/bin/lx

clean:
	rebar3 clean
	rm -f _build/default/bin/lx

install: escript
	@echo "Installing lx to /usr/local/bin..."
	@sudo chmod +x /usr/local/bin/lx
	@echo "lx installed successfully! You can now use 'lx' from anywhere."

uninstall:
	@echo "Uninstalling lx..."
	@sudo rm -f /usr/local/bin/lx
	@echo "lx uninstalled successfully!"

test: compile
	rebar3 eunit

test-verbose: compile
	rebar3 eunit -v

test-coverage: compile
	rebar3 eunit --cover

run-example: escript
	./_build/default/bin/lx run examples/literals.lx

run-macro-test: escript
	./_build/default/bin/lx run examples/macro_test.lx