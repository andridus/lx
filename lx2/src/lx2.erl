-module(lx2).

-include("lx2.hrl").

-export([compile/1, compile/2]).

%% Main compilation function
compile(Source) ->
    compile(Source, #{}).

%% Compile LX source code
compile(Source, Options) ->
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    case semantic_analysis(AST) of
                        {ok, Specs} ->
                            code_generation(AST, Specs, Options);
                        {error, TypeError} ->
                            {error, {semantic_error, TypeError}}
                    end;
                {error, SyntaxError} ->
                    {error, {syntax_error, SyntaxError}}
            end;
        {error, LexError} ->
            {error, {lexical_error, LexError}}
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
        {error, {syntax_error, Message}} ->
            {error, {syntax_error, 1, Message}};
        {error, {Line, yecc, {syntax_error, Message}}} ->
            {error, {syntax_error, Line, Message}};
        {error, {Line, yecc, {user, Error}}} ->
            {error, {syntax_error, Line, Error}};
        {error, {Line, _Module, Error}} ->
            {error, {syntax_error, Line, Error}}
    end.

%% Semantic analysis with type inference
semantic_analysis(AST) ->
    % Infer types using Hindley-Milner
    case lx2_types:infer_types(AST) of
        {ok, Specs} ->
            {ok, Specs};
        {error, TypeErrors} ->
            {error, TypeErrors}
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
        {error, NodeError} ->
            {error, NodeError}
    end.

check_undefined_variables_in_node({function_def, _Name, _Params, Body}) ->
    check_undefined_variables_in_list(Body);
check_undefined_variables_in_node({literal, undefined, _}) ->
    {error, {2, "Undefined variable: _"}};
check_undefined_variables_in_node({variable_ref, _Var}) ->
    % For now, we'll check this during type inference
    {ok, []};
check_undefined_variables_in_node({variable_binding, _Var, Expr}) ->
    check_undefined_variables_in_node(Expr);
check_undefined_variables_in_node(_) ->
    {ok, []}.

%% Type inference
infer_types(AST) ->
    % Create initial type environment
    Env = lx2_types:new_type_env(),

    % Infer types for all functions
    case infer_function_types(AST, Env) of
        {ok, TypedAST, Specs} ->
            {ok, TypedAST, Specs};
        {error, TypeErrors} ->
            {error, TypeErrors}
    end.

infer_function_types([], _Env) ->
    {ok, [], []};
infer_function_types([Fun | Rest], Env) ->
    case infer_function_type(Fun, Env) of
        {ok, TypedFun, Spec} ->
            case infer_function_types(Rest, Env) of
                {ok, TypedRest, Specs} ->
                    {ok, [TypedFun | TypedRest], [Spec | Specs]};
                {error, RestError} ->
                    {error, RestError}
            end;
        {error, FunError} ->
            {error, FunError}
    end.

infer_function_type({function_def, Name, Params, Body}, _Env) ->
    % Create local environment for function
    LocalEnv = lx2_types:new_type_env(),

    % Infer types for function body
    {ReturnType, _Sub} = lx2_types:infer_block(Body, LocalEnv),
    Spec = {Name, length(Params), ReturnType},
    {ok, {function_def, Name, Params, Body}, Spec}.

%% Code generation
code_generation(AST, Specs, Options) ->
    ModuleName = maps:get(module_name, Options, unknown),
    case maps:get(mode, Options, both) of
        beam ->
            lx2_codegen:compile_direct(AST, ModuleName);
        source ->
            lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs);
        erl ->
            lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs);
        ast ->
           {ok, AST};
        both ->
            case lx2_codegen:compile_direct(AST, ModuleName) of
                {ModuleName, BeamCode, Meta} ->
                    case lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, Meta#{source => ErlCode}};
                        {error, SourceError} ->
                            {error, SourceError}
                    end;
                {error, BeamError} ->
                    {error, BeamError}
            end
    end.