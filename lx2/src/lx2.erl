-module(lx2).

-include("lx2.hrl").

-export([compile/1, compile/2]).

%% Main compilation function
compile(Source) ->
    compile(Source, #{}).

compile(Source, Options) ->
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    case semantic_analysis(AST) of
                        {ok, TypedAST, Specs} ->
                            code_generation(TypedAST, Specs, Options);
                        {error, TypeErrors} ->
                            {error, TypeErrors}
                    end;
                {error, SyntaxError} ->
                    {error, SyntaxError}
            end;
        {error, LexicalError} ->
            {error, LexicalError}
    end.

%% Lexical analysis
lexical_analysis(Source) ->
    case lx2_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, leex, {illegal, Char}}, _EndLine} ->
            {error, {lexical_error, Line, "illegal character: " ++ [Char]}};
        {error, {Line, leex, {user, Error}}, _EndLine} ->
            {error, {lexical_error, Line, Error}};
        {error, {Line, _Module, Error}, _EndLine} ->
            {error, {lexical_error, Line, "lexical error: " ++ lists:flatten(io_lib:format("~p", [Error]))}}
    end.

%% Syntactic analysis
syntactic_analysis(Tokens) ->
    case lx2_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {Line, yecc, {syntax_error, Message}}} ->
            {error, {syntax_error, Line, Message}};
        {error, {Line, yecc, {user, Error}}} ->
            {error, {syntax_error, Line, Error}};
        {error, {Line, _Module, Error}} ->
            {error, {syntax_error, Line, Error}}
    end.

%% Semantic analysis (basic for now)
semantic_analysis(AST) ->
    % Check for undefined variables (underscore)
    case check_undefined_variables(AST) of
        {ok, _} ->
            {ok, AST, []};
        {error, {Line, Message}} ->
            {error, {type_error, Line, Message}}
    end.

%% Check for undefined variables
check_undefined_variables(AST) ->
    check_undefined_variables_in_list(AST).

check_undefined_variables_in_list([]) ->
    {ok, []};
check_undefined_variables_in_list([Node | Rest]) ->
    case check_undefined_variables_in_node(Node) of
        {ok, _} ->
            check_undefined_variables_in_list(Rest);
        {error, Error} ->
            {error, Error}
    end.

check_undefined_variables_in_node({function_def, _Name, _Params, Body}) ->
    check_undefined_variables_in_list(Body);
check_undefined_variables_in_node({literal, undefined, _}) ->
    {error, {2, "Undefined variable: _"}};
check_undefined_variables_in_node(_) ->
    {ok, []}.

%% Code generation
code_generation(AST, _Specs, Options) ->
    % Get module name from options or use default
    ModuleName = maps:get(module_name, Options, unamed),

    case maps:get(mode, Options, direct) of
        direct ->
            % Direct compilation to BEAM (default)
            case lx2_codegen:compile_direct(AST, ModuleName) of
                {ModuleName, BeamCode, DebugInfo} ->
                    {ok, ModuleName, BeamCode, DebugInfo};
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end;
        erl ->
            % Generate .erl file
            case lx2_codegen:generate_erl(AST, ModuleName) of
                {ok, ErlCode} ->
                    {ok, ErlCode};
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end;
        both ->
            % Both: BEAM + .erl file for debugging
            case lx2_codegen:compile_direct(AST, ModuleName) of
                {ModuleName, BeamCode, DebugInfo} ->
                    case lx2_codegen:generate_erl(AST, ModuleName) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, ErlCode, DebugInfo};
                        {error, ErlError} ->
                            {error, {erl_generation_error, ErlError}}
                    end;
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end;
        ast ->
            % Return AST for inspection
            {ok, AST}
    end.