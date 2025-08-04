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

run-example: escript
	./lx run examples/literals.lx