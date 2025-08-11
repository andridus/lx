-module(lx2_macros).

-export([expand_macros_with_types/2, register_macro/3, get_macro_definition/2, new_macro_env/0]).

-include("lx2.hrl").

%% Expand macros with type checking
expand_macros_with_types(AST, MacroEnv) ->
    case expand_ast_macros(AST, MacroEnv) of
        {ok, ExpandedAST} ->
            {ok, ExpandedAST};
        {error, Error} ->
            {error, {macro_expansion_error, Error}}
    end.

%% Expand macros in AST
expand_ast_macros([], _MacroEnv) ->
    {ok, []};
expand_ast_macros([Node | Rest], MacroEnv) ->
    case expand_node_macros(Node, MacroEnv) of
        {ok, ExpandedNode} ->
            case expand_ast_macros(Rest, MacroEnv) of
                {ok, ExpandedRest} ->
                    {ok, [ExpandedNode | ExpandedRest]};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Expand macros in a single node
expand_node_macros({Type, Meta, Args}, MacroEnv) ->
    case Type of
        % Primitives - no expansion needed
        defmacro ->
            {ok, {Type, Meta, Args}};
        '__block__' ->
            expand_block_macros({Type, Meta, Args}, MacroEnv);

        % Macro calls - expand these
        macro_call ->
            expand_macro_call({Type, Meta, Args}, MacroEnv);

        % Other nodes - recursively expand arguments if they are lists
        _ when is_list(Args) ->
            case expand_ast_macros(Args, MacroEnv) of
                {ok, ExpandedArgs} ->
                    {ok, {Type, Meta, ExpandedArgs}};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {ok, {Type, Meta, Args}}
    end.

%% Expand block macros
expand_block_macros({'__block__', Meta, Body}, MacroEnv) ->
    case expand_ast_macros(Body, MacroEnv) of
        {ok, ExpandedBody} ->
            {ok, {'__block__', Meta, ExpandedBody}};
        {error, Error} ->
            {error, Error}
    end.

%% Expand macro call
expand_macro_call({macro_call, _Meta, [MacroName, Args]}, MacroEnv) ->
    case get_macro_definition(MacroName, MacroEnv) of
        {ok, MacroDef} ->
            case apply_macro(MacroDef, Args, MacroEnv) of
                {ok, Expanded} ->
                    {ok, Expanded};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Get macro definition from environment
get_macro_definition(MacroName, #{macros := Macros}) ->
    case maps:find(MacroName, Macros) of
        {ok, [MacroDef | _]} ->
            {ok, MacroDef};
        {ok, []} ->
            {error, {macro_not_found, MacroName}};
        error ->
            {error, {macro_not_found, MacroName}}
    end.

%% Apply macro definition
apply_macro({MacroName, Args, Body}, CallArgs, MacroEnv) ->
    case length(Args) =:= length(CallArgs) of
        true ->
            % Create argument substitution
            ArgSub = maps:from_list(lists:zip(Args, CallArgs)),

            % Substitute arguments in body
            case substitute_macro_body(Body, ArgSub) of
                {ok, SubstitutedBody} ->
                    % Expand the substituted body
                    expand_node_macros(SubstitutedBody, MacroEnv);
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {error, {arity_mismatch, MacroName, length(Args), length(CallArgs)}}
    end.

%% Substitute arguments in macro body
substitute_macro_body({Type, Meta, Args}, ArgSub) when is_list(Args) ->
    case substitute_args(Args, ArgSub) of
        {ok, SubstitutedArgs} ->
            {ok, {Type, Meta, SubstitutedArgs}};
        {error, Error} ->
            {error, Error}
    end;
substitute_macro_body({Type, Meta, Args}, ArgSub) ->
    case substitute_single_arg(Args, ArgSub) of
        {ok, SubstitutedArg} ->
            {ok, {Type, Meta, SubstitutedArg}};
        {error, Error} ->
            {error, Error}
    end.

%% Substitute arguments in list
substitute_args([], _ArgSub) ->
    {ok, []};
substitute_args([Arg | Rest], ArgSub) ->
    case substitute_single_arg(Arg, ArgSub) of
        {ok, SubstitutedArg} ->
            case substitute_args(Rest, ArgSub) of
                {ok, SubstitutedRest} ->
                    {ok, [SubstitutedArg | SubstitutedRest]};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Substitute single argument
substitute_single_arg({ident, Meta, Name}, ArgSub) ->
    case maps:find(Name, ArgSub) of
        {ok, SubstitutedArg} ->
            {ok, SubstitutedArg};
        error ->
            {ok, {ident, Meta, Name}}
    end;
substitute_single_arg({Type, Meta, Args}, ArgSub) when is_list(Args) ->
    case substitute_args(Args, ArgSub) of
        {ok, SubstitutedArgs} ->
            {ok, {Type, Meta, SubstitutedArgs}};
        {error, Error} ->
            {error, Error}
    end;
substitute_single_arg(Arg, _ArgSub) ->
    {ok, Arg}.

%% Register new macro
register_macro(Name, Definition, MacroEnv) ->
    #{macros := Macros} = MacroEnv,
    NewMacros = maps:update_with(Name,
        fun(Definitions) -> [Definition | Definitions] end,
        [Definition],
        Macros),
    {ok, MacroEnv#{macros := NewMacros}}.

%% Create new macro environment
new_macro_env() ->
    #{
        macros => #{}
    }.