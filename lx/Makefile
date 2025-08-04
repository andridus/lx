.PHONY: all clean compile test install uninstall run-example run-macro-test

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

test: compile
	rebar3 eunit

test-verbose: compile
	rebar3 eunit -v

test-coverage: compile
	rebar3 eunit --cover

install: compile
	@echo "Installing lx to /usr/local/bin..."
	@sudo cp lx /usr/local/bin/
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