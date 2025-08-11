-module(lx0_compiler).
-include("../include/lx0.hrl").

-export([compile/1, compile/2, compile_file/1, compile_file/2]).
-export([parse/1, parse_file/1, generate_erlang/1]).

%% Compile a string containing LX code
compile(Source) ->
    compile(Source, []).

compile(Source, Options) ->
    compile(Source, Options, 'basic').

compile(Source, Options, ModuleName) ->
    case parse(Source) of
        {ok, AST} ->
            case generate_erlang(AST, ModuleName) of
                {ok, ErlangCode} ->
                    {ok, ErlangCode};
                {error, Reason} ->
                    {error, {codegen_error, Reason}}
            end;
        {error, Reason} ->
            {error, {parse_error, Reason}}
    end.

%% Compile a file containing LX code
compile_file(Filename) ->
    compile_file(Filename, []).

compile_file(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            ModuleName = lx0_cli:get_module_name(Filename),
            compile(Source, Options, ModuleName);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% Parse LX source code into AST
parse(Source) ->
    case lx0_lexer:tokenize(Source) of
        {ok, Tokens} ->
            case lx0_parser:parse(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Parse a file containing LX code
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            parse(Source);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% Generate Erlang code from AST
generate_erlang(AST) ->
    generate_erlang(AST, 'basic').

%% Generate Erlang code from AST with module name
generate_erlang(AST, ModuleName) ->
    try
        ErlangCode = lx0_codegen:generate(AST, ModuleName),
        {ok, ErlangCode}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.