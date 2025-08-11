-module(lx_cli).
-export([main/1]).

%% Main entry point for the CLI
main(Args) ->
    case Args of
        [] ->
            show_usage();
        ["run", FilePath] ->
            run_file(FilePath);
        ["generate", FilePath] ->
            generate_erlang(FilePath);
        [FilePath] ->
            % Default behavior: show AST
            show_ast(FilePath);
        _ ->
            show_usage()
    end.

%% Show usage information
show_usage() ->
    io:format("Usage: lx [command] <file.lx>~n"),
    io:format("Commands:~n"),
    io:format("  run <file.lx>     - Compile and run the Lx file~n"),
    io:format("  generate <file.lx> - Generate Erlang code from Lx file~n"),
    io:format("  <file.lx>         - Show AST (default behavior)~n"),
    io:format("~n"),
    io:format("Examples:~n"),
    io:format("  lx run test.lx~n"),
    io:format("  lx generate test.lx~n"),
    io:format("  lx test.lx~n").

%% Run a .lx file directly (compile to beam and execute)
run_file(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            io:format("Running ~s...~n", [FilePath]),
            case lx_compiler:compile_and_run(Source) of
                {ok, Result} ->
                    io:format("Result: ~p~n", [Result]),
                    {ok, Result};
                {error, Reason} ->
                    io:format("Error running ~s: ~p~n", [FilePath, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Generate Erlang code from .lx file
generate_erlang(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            ModuleName = extract_module_name_from_path(FilePath),
            Options = #{module_name => ModuleName},

            case lx_compiler:compile_to_erlang(Source, Options) of
                {ok, ErlangCode} ->
                    % Get the directory of the source file
                    SourceDir = filename:dirname(FilePath),
                    ErlFile = filename:join(SourceDir, atom_to_list(ModuleName) ++ ".erl"),
                    case file:write_file(ErlFile, ErlangCode) of
                        ok ->
                            io:format("Generated ~s from ~s~n", [ErlFile, FilePath]),
                            {ok, ErlFile};
                        {error, Reason} ->
                            io:format("Error writing ~s: ~p~n", [ErlFile, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    io:format("Error generating Erlang code from ~s: ~p~n", [FilePath, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Show AST for .lx file (default behavior)
show_ast(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            ModuleName = extract_module_name_from_path(FilePath),
            case lx_compiler:compile(Source, #{module_name => ModuleName}) of
                {ok, AST} ->
                    io:format("AST: ~p~n", [AST]),
                    {ok, AST};
                {error, Reason} ->
                    io:format("Error compiling ~s: ~p~n", [FilePath, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Read a file
read_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} -> {ok, binary_to_list(Content)};
        {error, Reason} -> {error, Reason}
    end.

%% Extract module name from file path
extract_module_name_from_path(FilePath) ->
    % Get the filename without extension
    Filename = filename:basename(FilePath, ".lx"),
    % Convert to atom (Erlang module name)
    list_to_atom(Filename).