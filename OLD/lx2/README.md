# LX2 - Compilador LX em Erlang

## Visão Geral

LX2 é uma reescrita completa do compilador LX usando Erlang/OTP e ferramentas nativas como Yacc (yecc) e Lex (leex). Esta implementação aproveita as capacidades nativas do Erlang para criar um compilador robusto, escalável e manutenível.

## Status do Projeto

**Fase Atual**: Fase 1 - Fundação ✅ **CONCLUÍDA**
**Próxima Fase**: Fase 2 - Task 1 (Functions with Literals)

## Fase 1: Fundação - Concluída ✅

### Funcionalidades Implementadas

1. **Configuração do Ambiente**
   - ✅ Setup do projeto Erlang/OTP
   - ✅ Configuração do Rebar3
   - ✅ Estrutura de diretórios
   - ✅ Makefile e scripts de build
   - ✅ Configuração de testes (EUnit)

2. **Lexer Básico (leex)**
   - ✅ Definição de tokens básicos em `leex/lx2_lexer.xrl`
   - ✅ Suporte a literais: integers, floats, strings, atoms, booleans, nil
   - ✅ Reconhecimento de identificadores
   - ✅ Tratamento de comentários (`#`)
   - ✅ Tratamento de whitespace e newlines
   - ✅ Tokens para operadores e pontuação básica

3. **Parser Básico (yecc)**
   - ✅ Gramática básica em `yecc/lx2_parser.yrl`
   - ✅ Gramática para literais
   - ✅ Gramática para funções simples: `def name() do ... end`
   - ✅ Estrutura AST básica

4. **Gerador de Código Básico**
   - ✅ Geração de código Erlang básico
   - ✅ Compilação direta para BEAM
   - ✅ Geração opcional de arquivos `.erl` para debugging

### Exemplo de Uso

```bash
# Compilar projeto
make compile

# Executar testes
make test

# Exemplo de compilação
erl -pa _build/default/lib/lx2/ebin -eval "
Source = \"def answer() do 42 end\",
{ok, ModuleName, BeamCode, _} = lx2:compile(Source),
{module, ModuleName} = code:load_binary(ModuleName, \"\", BeamCode),
Result = ModuleName:answer(),
io:format(\"Result: ~p~n\", [Result]),
halt().
"
```

### Sintaxe Suportada

```lx
# Funções simples
def answer() do
    42
end

def greeting() do
    "Hello, World!"
end

def status() do
    :ok
end

def pi() do
    3.14159
end

def truth() do
    true
end

def nothing() do
    nil
end
```

### Modos de Compilação

1. **Modo Direto (Padrão)**: Compilação direta para BEAM
   ```erlang
   {ok, ModuleName, BeamCode, DebugInfo} = lx2:compile(Source).
   ```

2. **Modo .erl**: Geração de arquivo .erl para debugging
   ```erlang
   ErlCode = lx2:compile(Source, #{mode => erl}).
   ```

3. **Modo Híbrido**: Ambos BEAM + .erl
   ```erlang
   {ok, ModuleName, BeamCode, ErlCode, DebugInfo} =
       lx2:compile(Source, #{mode => both}).
   ```

## Estrutura do Projeto

```
lx2/
├── src/
│   ├── lx2.erl                 # Módulo principal
│   ├── lx2_lexer.erl           # Analisador léxico (gerado)
│   ├── lx2_parser.erl          # Analisador sintático (gerado)
│   ├── lx2_codegen.erl         # Gerador de código
│   └── lx2.app.src             # Configuração da aplicação
├── leex/
│   └── lx2_lexer.xrl           # Definição do lexer
├── yecc/
│   └── lx2_parser.yrl          # Definição da gramática
├── include/
│   └── lx2.hrl                 # Definições comuns
├── test/
│   └── lx2_basic_tests.erl     # Testes básicos
├── examples/
│   └── task_01/                # Exemplos Task 1
├── rebar.config                # Configuração Rebar3
├── Makefile                    # Makefile para build
└── README.md                   # Documentação principal
```

## Comandos de Uso

### Compilador LX2

O LX2 inclui um executável para compilar e executar arquivos .lx diretamente:

```bash
# Compilar arquivo .lx para .erl
lx examples/task_01/simple.lx

# Executar arquivo .lx diretamente
lx run examples/task_01/simple.lx

# Mostrar AST (Abstract Syntax Tree)
lx ast examples/task_01/simple.lx

# Ver ajuda
lx help
```

### Desenvolvimento

```bash
# Compilar projeto
make compile

# Executar testes
make test

# Limpar build
make clean

# Debug com observer
make debug

# Executar Dialyzer
make dialyzer
```

## Próximos Passos

A **Fase 1** está concluída com sucesso! O compilador LX2 agora possui:

- ✅ Lexer funcional com suporte a literais básicos
- ✅ Parser funcional para funções simples
- ✅ Gerador de código que compila para BEAM
- ✅ Testes básicos passando
- ✅ Estrutura de projeto organizada

**Próxima fase**: Implementar **Task 1 - Functions with Literals** com suporte completo a todos os tipos de literais e funções mais complexas.

## Contribuição

Este projeto segue as melhores práticas de desenvolvimento Erlang/OTP:

- Uso de leex/yecc para análise léxica e sintática
- Compilação direta para BEAM para máxima performance
- Testes abrangentes com EUnit
- Documentação clara e exemplos práticos