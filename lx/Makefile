.PHONY: all clean compile escript

all: compile escript

compile:
	rebar3 compile

escript: compile
	rebar3 escriptize
	chmod +x lx

clean:
	rebar3 clean
	rm -f lx

test: compile
	rebar3 eunit

test-verbose: compile
	rebar3 eunit -v

test-coverage: compile
	rebar3 eunit --cover

run-example: escript
	./lx run examples/literals.lx