-module(lx_cli).
-export([main/1]).

main(["run", Filename | _]) ->
    case lx_compiler:compile_file(Filename) of
        {ok, AST} ->
            io:format("Compilation successful!~n"),
            io:format("AST: ~p~n", [AST]),
            halt(0);
        {error, empty_source} ->
            io:format("Nothing to compile: the file is empty or contains only comments/whitespace.~n"),
            halt(0);
        {error, Reason} ->
            io:format("Compilation failed: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: lx run <filename>~n"),
    io:format("Example: lx run examples/literals.lx~n"),
    halt(1).