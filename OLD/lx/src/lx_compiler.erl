-module(lx_compiler).
-export([compile/2, compile_to_erlang/2, compile_and_run/1]).

compile(Source, _Options) ->
    % Phase 1: Lexical analysis
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            % Phase 2: Syntactic analysis
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Compile Lx source to Erlang code
compile_to_erlang(Source, Options) ->
    case compile(Source, Options) of
        {ok, AST} ->
            % Transform Lx AST to Core Erlang AST
            case lx_ast_transformer:transform(AST) of
                {ok, CoreAST} ->
                    case lx_generator:generate(CoreAST, Options) of
                        {ok, ErlangCode} ->
                            {ok, ErlangCode};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Compile and run Lx source directly
compile_and_run(Source) ->
    case compile(Source, #{}) of
        {ok, AST} ->
            % Transform Lx AST to Core Erlang AST
            case lx_ast_transformer:transform(AST) of
                {ok, CoreAST} ->
                    % Generate temporary module name
                    ModuleName = generate_temp_module_name(),
                    Options = #{module_name => ModuleName},

                    case lx_generator:generate(CoreAST, Options) of
                        {ok, ErlangCode} ->
                            % Write temporary .erl file
                            ErlFile = atom_to_list(ModuleName) ++ ".erl",
                            case file:write_file(ErlFile, ErlangCode) of
                                ok ->
                                    % Compile to beam
                                    case compile_erlang_file(ErlFile) of
                                        {ok, BeamFile} ->
                                            % Run the module
                                            Result = run_compiled_module(ModuleName),
                                            % Clean up temporary files
                                            cleanup_temp_files(ErlFile, BeamFile),
                                            {ok, Result};
                                        {error, Reason} ->
                                            cleanup_temp_files(ErlFile, ""),
                                            {error, {compilation_error, Reason}}
                                    end;
                                {error, Reason} ->
                                    {error, {file_write_error, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {generation_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Lexical analysis phase
lexical_analysis(Source) ->
    case lx_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, Module, Message}, _EndLine} ->
            {error, {lexer_error, Line, Module, Message}};
        {error, Error, _EndLine} ->
            {error, Error}
    end.

%% Syntactic analysis phase
syntactic_analysis([]) -> {ok, []};
syntactic_analysis(Tokens) ->
    case lx_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {Line, Module, Message}} ->
            {error, {parse_error, Line, Module, Message}};
        {error, Error} ->
            {error, Error}
    end.

%% Generate temporary module name
generate_temp_module_name() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Timestamp = MegaSecs * 1000000 + Secs,
    list_to_atom("lx_temp_" ++ integer_to_list(Timestamp) ++ "_" ++ integer_to_list(MicroSecs)).

%% Compile Erlang file to beam
compile_erlang_file(ErlFile) ->
    case compile:file(ErlFile, [binary, return_errors]) of
        {ok, ModuleName, Beam} ->
            BeamFile = atom_to_list(ModuleName) ++ ".beam",
            case file:write_file(BeamFile, Beam) of
                ok ->
                    % Load the module
                    case code:load_binary(ModuleName, BeamFile, Beam) of
                        {module, ModuleName} -> {ok, BeamFile};
                        {error, Reason} -> {error, {load_error, Reason}}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.

%% Run compiled module
run_compiled_module(ModuleName) ->
    try
        ModuleName:main()
    catch
        Class:Reason:Stack ->
            {error, {runtime_error, Class, Reason, Stack}}
    end.

%% Clean up temporary files
cleanup_temp_files(ErlFile, BeamFile) ->
    file:delete(ErlFile),
    if BeamFile =/= "" -> file:delete(BeamFile); true -> ok end.