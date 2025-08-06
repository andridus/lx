-module(lx_macros).
-export([register_macro_with_params/5, expand_macros/1, get_macro/2, substitute_parameters/2]).

% Registry to store defined macros
% Format: {macro_name, {arity, precedence, associativity, modifiers}, macro_body}
-record(macro_registry, {
    macros = #{}
}).

% Initialize macro registry
init() ->
    #macro_registry{}.



% Register a new macro with parameter names
register_macro_with_params(Name, Metadata, Body, ParamNames, Registry) ->
    % Store macro with parameter names
    MacroInfo = {Metadata, Body, ParamNames},
    Registry#macro_registry{macros = maps:put(Name, MacroInfo, Registry#macro_registry.macros)}.

% Get a macro by name
get_macro(Name, Registry) ->
    case maps:get(Name, Registry#macro_registry.macros, not_found) of
        not_found -> not_found;
        {Metadata, Body, ParamNames} -> {Metadata, Body, ParamNames}
    end.

% Expand macros in AST
expand_macros(AST) ->
    Registry = init(),
    % First pass: register macros and expand obvious macro calls
    {AST1, Registry1} = expand_macros_pass1(AST, Registry),
    % Second pass: detect and combine macro calls (atom followed by args)
    case expand_macros_pass2(AST1, Registry1) of
        {error, Error} -> {error, Error};
        AST2 -> {ok, AST2}
    end.

% First pass: register macros and expand obvious macro calls
expand_macros_pass1([], Registry) ->
    {[], Registry};
expand_macros_pass1([Node | Rest], Registry) ->
    {ExpandedNode, Registry1} = expand_node_pass1(Node, Registry),
    {ExpandedRest, Registry2} = expand_macros_pass1(Rest, Registry1),
    {[ExpandedNode | ExpandedRest], Registry2};
expand_macros_pass1(Node, Registry) when is_tuple(Node) ->
    % Handle single node case
    {ExpandedNode, Registry1} = expand_node_pass1(Node, Registry),
    {ExpandedNode, Registry1}.

% Second pass: detect and combine macro calls
expand_macros_pass2([], _Registry) ->
    [];
expand_macros_pass2([Node | Rest], Registry) ->
    case Node of
        {macro_call, Line, Name, Args} ->
            case get_macro(Name, Registry) of
                not_found ->
                    {error, {undefined_macro, Line, Name, Args}};
                {_, Body, ParamNames} ->
                    ExpandedMacro = expand_macro_call(Name, [Args], Body, Line, Registry),
                    [ExpandedMacro | expand_macros_pass2(Rest, Registry)]
            end;
        {atom, Line, Name} ->
            case get_macro(Name, Registry) of
                not_found ->
                    [expand_node_pass2(Node, Registry) | expand_macros_pass2(Rest, Registry)];
                {_, Body, ParamNames} ->
                    Arity = length(ParamNames),
                    {Args, RestNodes} = lists:split(Arity, Rest),
                    ExpandedMacro = expand_macro_call(Name, Args, Body, Line, Registry),
                    [ExpandedMacro | expand_macros_pass2(RestNodes, Registry)]
            end;
        _ ->
            [expand_node_pass2(Node, Registry) | expand_macros_pass2(Rest, Registry)]
    end;
expand_macros_pass2(Node, Registry) ->
    expand_node_pass2(Node, Registry).

% First pass: expand a single AST node
expand_node_pass1({macro_def, Line, Name, Params, Body}, Registry) ->
    % Extract parameter names from Params
    ParamNames = extract_param_names_from_params(Params),

    % Register the macro with parameter names
    Metadata = {length(Params), 1, any, [{args, Params}]},
    NewRegistry = register_macro_with_params(Name, Metadata, Body, ParamNames, Registry),

    % Return the macro definition as is for now
    {{macro_def, Line, Name, Params, Body}, NewRegistry};
expand_node_pass1({macro_def_infix, Line, Name, Params, Body}, Registry) ->
    % Extract parameter names from Params
    ParamNames = extract_param_names_from_params(Params),

    % Register the infix macro with parameter names
    Metadata = {length(Params), 1, any, [infix]},
    NewRegistry = register_macro_with_params(Name, Metadata, Body, ParamNames, Registry),

    % Return the macro definition as is for now
    {{macro_def_infix, Line, Name, Params, Body}, NewRegistry};
expand_node_pass1({do_block, Line, Expressions}, Registry) ->
    % Expand expressions in do block
    {ExpandedExpressions, NewRegistry} = expand_macros_pass1(Expressions, Registry),
    {{do_block, Line, ExpandedExpressions}, NewRegistry};
expand_node_pass1({list, Line, Elements}, Registry) ->
    % Check if this list is a macro call (first element is macro name)
    case Elements of
        [{atom, _, MacroName} | Args] ->
            case get_macro(MacroName, Registry) of
                not_found ->
                    % Not a macro call, expand elements normally
                    {ExpandedElements, NewRegistry} = expand_macros_pass1(Elements, Registry),
                    {{list, Line, ExpandedElements}, NewRegistry};
                {_Metadata, Body, ParamNames} ->
                    % This is a macro call with arguments
                    ExpandedMacro = expand_macro_call(MacroName, Args, Body, Line, Registry),
                    {ExpandedMacro, Registry}
            end;
        _ ->
            % Not a macro call, expand elements normally
            {ExpandedElements, NewRegistry} = expand_macros_pass1(Elements, Registry),
            {{list, Line, ExpandedElements}, NewRegistry}
    end;
expand_node_pass1(Node, Registry) ->
    % For other nodes, return as is
    {Node, Registry}.

% Second pass: expand a single AST node
expand_node_pass2({possible_macro_call, Line, Op, Args}, Registry) ->
    % Check if the operator is a macro
    case get_macro(Op, Registry) of
        not_found ->
            % Not a macro, convert to binary_op for regular operators
            case Args of
                [Left, Right] ->
                    ExpandedLeft = expand_node_pass2(Left, Registry),
                    ExpandedRight = expand_node_pass2(Right, Registry),
                    {binary_op, Line, ExpandedLeft, Op, ExpandedRight};
                _ ->
                    {atom, Line, {invalid_macro_call, Op, Args}}
            end;
        {_Metadata, Body, ParamNames} ->
            % This is a macro call with arguments
            ExpandedMacro = expand_macro_call(Op, Args, Body, Line, Registry),
            ExpandedMacro
    end;
expand_node_pass2({binary_op, Line, Left, Op, Right}, Registry) ->
    % Check if the operator is a macro
    case get_macro(Op, Registry) of
        not_found ->
            % Not a macro, expand children
            ExpandedLeft = expand_node_pass2(Left, Registry),
            ExpandedRight = expand_node_pass2(Right, Registry),
            {binary_op, Line, ExpandedLeft, Op, ExpandedRight};
        {_Metadata, Body, ParamNames} ->
            % This is a macro call with left and right as arguments
            ExpandedMacro = expand_macro_call(Op, [Left, Right], Body, Line, Registry),
            ExpandedMacro
    end;
expand_node_pass2({list, Line, Elements}, Registry) ->
    ExpandedElements = [expand_node_pass2(Elem, Registry) || Elem <- Elements],
    {list, Line, ExpandedElements};
expand_node_pass2({tuple, Line, Elements}, Registry) ->
    ExpandedElements = [expand_node_pass2(Elem, Registry) || Elem <- Elements],
    {tuple, Line, ExpandedElements};
expand_node_pass2({map, Line, Entries}, Registry) ->
    ExpandedEntries = [expand_map_entry(Entry, Registry) || Entry <- Entries],
    {map, Line, ExpandedEntries};
expand_node_pass2(Node, _Registry) ->
    % For other nodes, return as is
    Node.

% Expand a macro call with arguments
expand_macro_call(MacroName, Args, Body, Line, Registry) ->
    % Get macro metadata and parameter names
    {_Metadata, MacroBody, ParamNames} = get_macro(MacroName, Registry),
    % Expand each argument recursivamente
    ExpandedArgs = [expand_macros_pass2(Arg, Registry) || Arg <- Args],
    % Create parameter substitution map using actual parameter names
    Substitution = create_substitution_with_names(ParamNames, ExpandedArgs),
    % Substitute parameters in the macro body
    ExpandedBody = substitute_parameters(MacroBody, Substitution),
    % Check if this is a __erl__ macro (direct Erlang AST)
    case ExpandedBody of
        {tuple, _, [{atom, _, '__erl__'}, ErlAST]} ->
            % This is a __erl__ macro, return the Erlang AST directly
            ErlAST;
        _ ->
            % Regular macro expansion
            ExpandedBody
    end.





% Extract parameter names from Params AST
extract_param_names_from_params(Params) ->
    [extract_param_name(Param) || Param <- Params].

% Extract parameter name from a parameter AST node
extract_param_name({atom, _Line, Name}) ->
    Name;
extract_param_name({ident, _Line, Name}) ->
    list_to_atom(Name);
extract_param_name(Other) ->
    % Fallback for unknown parameter types
    unknown_param.

% Create parameter substitution map using actual parameter names
create_substitution_with_names([], []) ->
    #{};
create_substitution_with_names([ParamName | ParamNames], [Arg | Args]) ->
    Substitution = create_substitution_with_names(ParamNames, Args),
    maps:put(ParamName, Arg, Substitution);
create_substitution_with_names(_, _) ->
    % Fallback for mismatched parameters
    #{}.

% Create parameter substitution map - mais genérico (deprecated)
create_substitution(MacroName, Args) ->
    % Para macros com parâmetros nomeados
    case Args of
        [] -> #{};
        [SingleArg] -> #{body => SingleArg};
        [Left, Right] -> #{left => Left, right => Right};
        MultipleArgs -> #{body => {list, 1, MultipleArgs}}
    end.

% Substitute parameters in macro body - mais genérico
substitute_parameters(Body) ->
    substitute_parameters_rec(Body, #{}).

substitute_parameters(Body, Substitution) ->
    substitute_parameters_rec(Body, Substitution).

substitute_parameters_rec({atom, Line, Name}, Substitution) ->
    case maps:get(Name, Substitution, not_found) of
        not_found ->
            {atom, Line, Name};
        Value ->
            Value
    end;
substitute_parameters_rec({ident, Line, Name}, Substitution) ->
    case maps:get(list_to_atom(Name), Substitution, not_found) of
        not_found ->
            {ident, Line, Name};
        Value ->
            Value
    end;
substitute_parameters_rec({tuple, Line, Elements}, Substitution) ->
    ExpandedElements = [substitute_parameters_rec(Elem, Substitution) || Elem <- Elements],
    {tuple, Line, ExpandedElements};
substitute_parameters_rec({list, Line, Elements}, Substitution) ->
    ExpandedElements = [substitute_parameters_rec(Elem, Substitution) || Elem <- Elements],
    {list, Line, ExpandedElements};
substitute_parameters_rec({map, Line, Entries}, Substitution) ->
    ExpandedEntries = [substitute_map_entry(Entry, Substitution) || Entry <- Entries],
    {map, Line, ExpandedEntries};
substitute_parameters_rec({binary_op, Line, Left, Op, Right}, Substitution) ->
    ExpandedLeft = substitute_parameters_rec(Left, Substitution),
    ExpandedRight = substitute_parameters_rec(Right, Substitution),
    {binary_op, Line, ExpandedLeft, Op, ExpandedRight};
substitute_parameters_rec({do_block, Line, Expressions}, Substitution) ->
    ExpandedExpressions = [substitute_parameters_rec(Expr, Substitution) || Expr <- Expressions],
    {do_block, Line, ExpandedExpressions};
substitute_parameters_rec(Node, _Substitution) ->
    % For other node types, return as is
    Node.

% Expand map entry
expand_map_entry({map_entry, Line, Key, Value}, Registry) ->
    ExpandedKey = expand_node_pass2(Key, Registry),
    ExpandedValue = expand_node_pass2(Value, Registry),
    {map_entry, Line, ExpandedKey, ExpandedValue};
expand_map_entry({Key, Value}, Registry) ->
    ExpandedKey = expand_node_pass2(Key, Registry),
    ExpandedValue = expand_node_pass2(Value, Registry),
    {ExpandedKey, ExpandedValue}.

% Substitute map entry
substitute_map_entry({Key, Value}, Substitution) ->
    SubstitutedKey = substitute_parameters_rec(Key, Substitution),
    SubstitutedValue = substitute_parameters_rec(Value, Substitution),
    {SubstitutedKey, SubstitutedValue}.

unwrap_list([{list, _, SubElements}]) ->
    unwrap_list(SubElements);
unwrap_list(Els) -> Els.