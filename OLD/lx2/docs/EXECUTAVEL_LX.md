# Executável LX2 - Compilador de Linha de Comando

## Visão Geral

O LX2 inclui um executável de linha de comando que permite compilar e executar arquivos .lx diretamente, sem necessidade de usar o Erlang shell.

## Instalação

### Opção 1: Link Simbólico (Recomendado)

```bash
# Criar link simbólico no diretório do usuário
ln -sf $(pwd)/bin/lx ~/.local/bin/lx

# Adicionar ~/.local/bin ao PATH (se necessário)
export PATH="$HOME/.local/bin:$PATH"
```

### Opção 2: Uso Direto

```bash
# Usar o executável diretamente
./bin/lx <comando>
```

## Comandos Disponíveis

### 1. Compilar Arquivo (.lx → .erl)

```bash
# Sintaxe básica
lx <arquivo.lx>

# Exemplos
lx examples/task_01/simple.lx
lx examples/task_01/main.lx
```

**Resultado**: Gera um arquivo .erl na mesma pasta do arquivo .lx

### 2. Executar Arquivo (.lx → Execução Direta)

```bash
# Sintaxe básica
lx run <arquivo.lx>

# Exemplos
lx run examples/task_01/simple.lx
lx run examples/task_01/main.lx
```

**Resultado**: Compila e executa o arquivo, mostrando o resultado

### 3. Mostrar AST (.lx → Abstract Syntax Tree)

```bash
# Sintaxe básica
lx ast <arquivo.lx>

# Exemplos
lx ast examples/task_01/simple.lx
lx ast examples/task_01/complex.lx
```

**Resultado**: Mostra a estrutura AST do arquivo para debugging

### 4. Ajuda

```bash
# Ver ajuda
lx help
lx --help
lx -h
```

## Exemplos de Uso

### Exemplo 1: Arquivo Simples

```lx
# examples/task_01/single.lx
def answer() do
    42
end
```

```bash
# Compilar
lx examples/task_01/single.lx
# Resultado: Compiled examples/task_01/single.lx to examples/task_01/single.erl

# Executar
lx run examples/task_01/single.lx
# Resultado: 42
```

### Exemplo 2: Múltiplas Funções

```lx
# examples/task_01/simple.lx
def answer() do
    42
end

def greeting() do
    "Hello, World!"
end

def status() do
    :ok
end
```

```bash
# Compilar
lx examples/task_01/simple.lx
# Gera: examples/task_01/simple.erl

# Executar (executa a primeira função encontrada)
lx run examples/task_01/simple.lx
# Resultado: 42
```

### Exemplo 3: Função Main

```lx
# examples/task_01/main.lx
def main() do
    "Hello from LX2!"
end

def answer() do
    42
end
```

```bash
# Executar (prioriza função 'main')
lx run examples/task_01/main.lx
# Resultado: <<"Hello from LX2!">>
```

### Exemplo 4: Visualizar AST

```lx
# examples/task_01/complex.lx
def main() do
    "Main function"
end

def answer() do
    42
end

def greeting() do
    "Hello, World!"
end
```

```bash
# Mostrar AST
lx ast examples/task_01/complex.lx
# Resultado:
# AST for examples/task_01/complex.lx:
# {function_def, main, [],
#   {literal, string, "Main function"}
# }
# {function_def, answer, [],
#   {literal, integer, 42}
# }
# {function_def, greeting, [],
#   {literal, string, "Hello, World!"}
# }
```

## Arquivos Gerados

### Arquivo .erl

Quando você compila um arquivo .lx, o LX2 gera um arquivo .erl equivalente:

```erlang
% examples/task_01/simple.erl (gerado automaticamente)
-module(answer).
-export([
    answer/0,
    greeting/0,
    status/0
]).

answer() ->
    42.

greeting() ->
    "Hello, World!".

status() ->
    ok.
```

## Lógica de Execução

### Prioridade de Funções

Quando você usa `lx run`, o compilador procura por funções na seguinte ordem:

1. **Função `main/0`** (prioridade máxima)
2. **Funções comuns**: `answer/0`, `test/0`, `start/0`, `init/0`
3. **Primeira função** encontrada no arquivo

### Exemplo de Prioridade

```lx
def test() do
    "test function"
end

def main() do
    "main function"
end

def answer() do
    42
end
```

```bash
lx run example.lx
# Resultado: <<"main function">> (executa main/0)
```

## Tratamento de Erros

O LX2 agora inclui um sistema avançado de mensagens de erro que mostra:

- **Tipo de erro** (Lexical, Syntax, Type, etc.)
- **Linha exata** onde o erro ocorreu
- **Contexto do código** (2 linhas antes e depois)
- **Indicador visual** (^) apontando para a posição exata

### Erros de Lexer (Caracteres Inválidos)

```bash
lx error_syntax.lx
# Resultado:
# Error in error_syntax.lx:
#
# Lexical Error at line 6:
# lexical error: {illegal,"@"}
#
# Context:
# 0004 |
# 0005 | def invalid() do
# 0006 |     @invalid_token
#      |
#      |                   ^
# 0007 | end
# 0008 |
```

### Erros de Parser (Sintaxe Inválida)

```bash
lx error_parser.lx
# Resultado:
# Error in error_parser.lx:
#
# Syntax Error at line 6:
# syntax error before: def
#
# Context:
# 0004 |
# 0005 | def invalid() do
# 0006 |     def missing_do
#      |
#      |                   ^
# 0007 | end
# 0008 |
```

### Erros de Execução

```bash
lx run file_with_error.lx
# Resultado: Runtime error: {badmatch, ...}
```

### Arquivo Não Encontrado

```bash
lx nonexistent.lx
# Resultado: Error reading file nonexistent.lx: enoent
```

## Integração com Desenvolvimento

### Workflow Típico

1. **Desenvolver**: Escrever código .lx
2. **Testar**: `lx run arquivo.lx`
3. **Compilar**: `lx arquivo.lx` (gera .erl para debugging)
4. **Iterar**: Fazer mudanças e testar novamente

### Debugging

```bash
# Compilar para .erl para inspeção
lx arquivo.lx

# Verificar código gerado
cat arquivo.erl

# Executar para testar
lx run arquivo.lx

# Visualizar AST para debugging
lx ast arquivo.lx

# Corrigir erros com mensagens detalhadas
lx ast arquivo_com_erro.lx
```

## Configuração Avançada

### Variáveis de Ambiente

```bash
# Definir caminho para o executável
export LX2_PATH="/path/to/lx2/bin/lx"

# Usar com caminho completo
$LX2_PATH run arquivo.lx
```

### Alias Úteis

```bash
# Adicionar ao .bashrc ou .zshrc
alias lxc='lx compile'
alias lxr='lx run'
alias lxh='lx help'
```

## Limitações Atuais

### Funcionalidades Suportadas
- ✅ Funções simples sem argumentos
- ✅ Todos os tipos de literais
- ✅ Múltiplas funções por arquivo
- ✅ Compilação para .erl
- ✅ Execução direta
- ✅ Visualização de AST

### Funcionalidades Não Suportadas (Futuras Fases)
- ❌ Funções com argumentos
- ❌ Variáveis e bindings
- ❌ Operadores binários
- ❌ Estruturas de dados complexas

## Troubleshooting

### Problema: "command not found"
```bash
# Verificar se o link simbólico existe
ls -la ~/.local/bin/lx

# Verificar se ~/.local/bin está no PATH
echo $PATH | grep local/bin
```

### Problema: "permission denied"
```bash
# Tornar o script executável
chmod +x bin/lx
```

### Problema: Erro de compilação
```bash
# Verificar se o projeto foi compilado
make compile

# Verificar se os módulos estão disponíveis
erl -pa _build/default/lib/lx2/ebin -eval "lx2:compile(\"def test() do 42 end\"), halt()."
```

## Próximos Passos

O executável será expandido nas próximas fases para suportar:

- **Fase 2**: Funções com argumentos
- **Fase 3**: Variáveis e bindings
- **Fase 4**: Operadores binários
- **Fase 5**: Estruturas de dados complexas

---

**Executável**: `bin/lx`
**Documentação**: Este arquivo
**Exemplos**: `examples/task_01/`