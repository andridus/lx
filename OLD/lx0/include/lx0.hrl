%% LX0 Compiler Header File
%% Common definitions and macros

-define(LX_VERSION, "0.1.0").

%% Token definitions
-define(TOKEN_ATOM, atom).
-define(TOKEN_INTEGER, integer).
-define(TOKEN_FLOAT, float).
-define(TOKEN_STRING, string).
-define(TOKEN_VAR, var).
-define(TOKEN_OP, op).
-define(TOKEN_PUNCT, punct).
-define(TOKEN_KEYWORD, keyword).

%% AST node types
-define(AST_LITERAL, literal).
-define(AST_VARIABLE, variable).
-define(AST_FUNCTION_CALL, function_call).
-define(AST_FUNCTION_DEF, function_def).
-define(AST_RECORD_DEF, record_def).
-define(AST_RECORD_CREATE, record_create).
-define(AST_RECORD_ACCESS, record_access).
-define(AST_BINARY, binary).
-define(AST_LIST, list).
-define(AST_TUPLE, tuple).
-define(AST_MAP, map).
-define(AST_CASE, case).
-define(AST_IF, if).
-define(AST_BLOCK, block).
-define(AST_PATTERN_MATCH, pattern_match).

%% Error types
-define(ERROR_SYNTAX, syntax_error).
-define(ERROR_TYPE, type_error).
-define(ERROR_COMPILE, compile_error).