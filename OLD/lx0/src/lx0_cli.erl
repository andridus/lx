-module(lx0_cli).
-include("../include/lx0.hrl").

-export([main/1, get_module_name/1]).

main(Args) ->
    case parse_args(Args) of
        {compile, Filename, Options} ->
            compile_file(Filename, Options);
        {parse, Filename} ->
            parse_file(Filename);
        {help} ->
            show_help();
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            show_help(),
            halt(1)
    end.

parse_args([]) ->
    {help};
parse_args(["-h" | _]) ->
    {help};
parse_args(["--help" | _]) ->
    {help};
parse_args(["compile" | Args]) ->
    parse_compile_args(Args);
parse_args(["parse" | Args]) ->
    parse_parse_args(Args);
parse_args([Filename | _]) ->
    % Default to compile if no command specified
    {compile, Filename, []}.

parse_compile_args([]) ->
    {error, "No filename specified"};
parse_compile_args([Filename | Args]) ->
    Options = parse_options(Args),
    {compile, Filename, Options}.

parse_parse_args([]) ->
    {error, "No filename specified"};
parse_parse_args([Filename | _]) ->
    {parse, Filename}.

parse_options([]) ->
    [];
parse_options(["--debug-tokens" | Rest]) ->
    [{debug_tokens, true} | parse_options(Rest)];
parse_options(["--debug-types" | Rest]) ->
    [{debug_types, true} | parse_options(Rest)];
parse_options(["--output", Output | Rest]) ->
    [{output, Output} | parse_options(Rest)];
parse_options([Unknown | _]) ->
    {error, "Unknown option: " ++ Unknown}.

compile_file(Filename, Options) ->
    case lx0_compiler:compile_file(Filename, Options) of
        {ok, ErlangCode} ->
            OutputFile = get_output_file(Filename, Options),
            case file:write_file(OutputFile, ErlangCode) of
                ok ->
                    io:format("Compiled ~s to ~s~n", [Filename, OutputFile]);
                {error, Reason} ->
                    io:format("Error writing output file: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Compilation error: ~p~n", [Reason]),
            halt(1)
    end.

get_module_name(Filename) ->
    BaseName = filename:basename(Filename, ".lx"),
    LowerName = string:to_lower(BaseName),
    list_to_atom(LowerName).

parse_file(Filename) ->
    case lx0_compiler:parse_file(Filename) of
        {ok, AST} ->
            io:format("Parsed AST:~n~p~n", [AST]);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            halt(1)
    end.

get_output_file(Filename, Options) ->
    case proplists:get_value(output, Options) of
        undefined ->
            % Replace .lx extension with .erl
            case string:split(Filename, ".", trailing) of
                [Base, "lx"] ->
                    Base ++ ".erl";
                _ ->
                    Filename ++ ".erl"
            end;
        Output ->
            Output
    end.

show_help() ->
    io:format("LX0 Compiler v~s~n", [?LX_VERSION]),
    io:format("~n"),
    io:format("Usage:~n"),
    io:format("  lx0 compile <filename.lx> [options]  Compile LX file to Erlang~n"),
    io:format("  lx0 parse <filename.lx>              Parse LX file and show AST~n"),
    io:format("  lx0 -h, --help                       Show this help~n"),
    io:format("~n"),
    io:format("Options:~n"),
    io:format("  --debug-tokens                       Show tokens during compilation~n"),
    io:format("  --debug-types                        Show type information~n"),
    io:format("  --output <filename>                  Specify output filename~n"),
    io:format("~n"),
    io:format("Examples:~n"),
    io:format("  lx0 compile example.lx~n"),
    io:format("  lx0 compile example.lx --output example.erl~n"),
    io:format("  lx0 parse example.lx~n").