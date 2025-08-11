-module(lx_generator).
-export([generate/2, generate_module/2]).

%% Generate Erlang code from Lx AST
generate(AST, Options) ->
    ModuleName = maps:get(module_name, Options, test),
    ErlangCode = generate_module(AST, ModuleName),
    {ok, ErlangCode}.

%% Generate a complete Erlang module
generate_module(AST, ModuleName) ->
    ModuleHeader = generate_module_header(ModuleName),
    Functions = generate_functions(AST),
    ModuleHeader ++ "\n" ++ Functions ++ "\n".

%% Generate module header
generate_module_header(ModuleName) ->
    "-module(" ++ atom_to_list(ModuleName) ++ ").\n" ++
    "-export([main/0]).\n\n" ++
    "main() ->\n".

%% Generate functions from AST
generate_functions(AST) ->
    case AST of
        [] ->
            "    ok.\n";
        [SingleExpr] ->
            "    " ++ generate_expression(SingleExpr) ++ ".\n";
        MultipleExprs ->
            "    " ++ generate_expression_list(MultipleExprs) ++ ".\n"
    end.

%% Generate expression list
generate_expression_list(Exprs) ->
    case Exprs of
        [] -> "ok";
        [Expr] -> generate_expression(Expr);
        [Expr | Rest] ->
            generate_expression(Expr) ++ ",\n    " ++ generate_expression_list(Rest)
    end.

%% Generate single expression
generate_expression(Expr) ->
    io:format("DEBUG: generate_expression called with: ~p~n", [Expr]),
    case Expr of
        {integer, _Line, Value} ->
            integer_to_list(Value);
        {float, _Line, Value} ->
            float_to_list(Value);
        {atom, _Line, Value} ->
            atom_to_list(Value);
        {string, _Line, Value} ->
            "\"" ++ Value ++ "\"";
        {var, _Line, Name} ->
            atom_to_list(Name);
        {cons, _Line, Head, Tail} ->
            "[" ++ generate_expression(Head) ++ " | " ++ generate_expression(Tail) ++ "]";
        {nil, _Line} ->
            "[]";
        {tuple, _Line, Elements} ->
            "{" ++ generate_tuple_elements(Elements) ++ "}";
        {map, _Line, Map, Entries} ->
            "#{" ++ generate_map_entries(Entries) ++ "}";
        {map_field_exact, _Line, Key, Value} ->
            generate_expression(Key) ++ " => " ++ generate_expression(Value);
        {call, _Line, {remote, _RemoteLine, Module, Function}, Args} ->
            ModuleStr = generate_expression(Module),
            FunctionStr = generate_expression(Function),
            ArgsStr = generate_function_args(Args),
            % Handle operators specially
            case FunctionStr of
                "+" -> "(" ++ generate_expression(hd(Args)) ++ " + " ++ generate_expression(hd(tl(Args))) ++ ")";
                "-" -> "(" ++ generate_expression(hd(Args)) ++ " - " ++ generate_expression(hd(tl(Args))) ++ ")";
                "*" -> "(" ++ generate_expression(hd(Args)) ++ " * " ++ generate_expression(hd(tl(Args))) ++ ")";
                "/" -> "(" ++ generate_expression(hd(Args)) ++ " / " ++ generate_expression(hd(tl(Args))) ++ ")";
                _ -> ModuleStr ++ ":" ++ FunctionStr ++ "(" ++ ArgsStr ++ ")"
            end;
        {call, _Line, Function, Args} ->
            FunctionStr = generate_expression(Function),
            ArgsStr = generate_function_args(Args),
            FunctionStr ++ "(" ++ ArgsStr ++ ")";
        {case_, _Line, Expr, Clauses} ->
            "case " ++ generate_expression(Expr) ++ " of\n    " ++
            generate_case_clauses(Clauses) ++ "\nend";
        {'case', _Line, Expr, Clauses} ->
            io:format("DEBUG: Generating case expression~n"),
            "case " ++ generate_expression(Expr) ++ " of\n    " ++
            generate_case_clauses(Clauses) ++ "\nend";
        {clause, _Line, Patterns, Guards, Body} ->
            PatternStr = generate_patterns(Patterns),
            GuardStr = generate_guards(Guards),
            BodyStr = generate_body(Body),
            PatternStr ++ GuardStr ++ " ->\n    " ++ BodyStr;
        {try_, _Line, Expr, Cases, Catch, After} ->
            "try\n    " ++ generate_expression(Expr) ++ "\n" ++
            "catch\n    " ++ generate_catch_clauses(Cases) ++ "\n" ++
            "after\n    " ++ generate_after_expressions(After) ++ "\n" ++
            "end";
        {term, _Line, Term} ->
            io_lib:format("~p", [Term]);
        _ ->
            io_lib:format("%% Unsupported Core Erlang expression: ~p", [Expr])
    end.

%% Generate patterns
generate_patterns([]) -> "";
generate_patterns([Pattern]) -> generate_expression(Pattern);
generate_patterns([Pattern | Rest]) ->
    generate_expression(Pattern) ++ ", " ++ generate_patterns(Rest).

%% Generate guards
generate_guards([]) -> "";
generate_guards(Guards) ->
    " when " ++ generate_guard_expressions(Guards).

%% Generate guard expressions
generate_guard_expressions([]) -> "";
generate_guard_expressions([Guard]) -> generate_expression(Guard);
generate_guard_expressions([Guard | Rest]) ->
    generate_expression(Guard) ++ ", " ++ generate_guard_expressions(Rest).

%% Generate body
generate_body([]) -> "ok";
generate_body([Expr]) -> generate_expression(Expr);
generate_body([Expr | Rest]) ->
    generate_expression(Expr) ++ ",\n    " ++ generate_body(Rest).

%% Generate catch clauses
generate_catch_clauses([]) -> "";
generate_catch_clauses([Clause]) -> generate_expression(Clause);
generate_catch_clauses([Clause | Rest]) ->
    generate_expression(Clause) ++ ";\n    " ++ generate_catch_clauses(Rest).

%% Generate after expressions
generate_after_expressions([]) -> "ok";
generate_after_expressions([Expr]) -> generate_expression(Expr);
generate_after_expressions([Expr | Rest]) ->
    generate_expression(Expr) ++ ",\n    " ++ generate_after_expressions(Rest).

%% Generate tuple elements
generate_tuple_elements([]) -> "";
generate_tuple_elements([Element]) -> generate_expression(Element);
generate_tuple_elements([Element | Rest]) ->
    generate_expression(Element) ++ ", " ++ generate_tuple_elements(Rest).

%% Generate map entries
generate_map_entries([]) -> "";
generate_map_entries([Entry]) -> generate_expression(Entry);
generate_map_entries([Entry | Rest]) ->
    generate_expression(Entry) ++ ", " ++ generate_map_entries(Rest).

%% Generate function arguments
generate_function_args([]) -> "";
generate_function_args([Arg]) -> generate_expression(Arg);
generate_function_args([Arg | Rest]) ->
    generate_expression(Arg) ++ ", " ++ generate_function_args(Rest).

%% Generate case clauses
generate_case_clauses([]) -> "";
generate_case_clauses([Clause]) -> generate_expression(Clause);
generate_case_clauses([Clause | Rest]) ->
    generate_expression(Clause) ++ ";\n    " ++ generate_case_clauses(Rest).