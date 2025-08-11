# Sistema de Erros Melhorado - LX2

## Visão Geral

O LX2 agora inclui um sistema avançado de mensagens de erro que fornece informações detalhadas e contextuais para facilitar o debugging e desenvolvimento.

## Características do Sistema de Erros

### ✅ **Informações Detalhadas**
- **Tipo de erro**: Lexical, Syntax, Type, etc.
- **Linha exata**: Número da linha onde o erro ocorreu
- **Arquivo**: Nome do arquivo com erro
- **Mensagem descritiva**: Explicação clara do problema

### ✅ **Contexto Visual**
- **2 linhas antes**: Código anterior ao erro
- **Linha do erro**: Código com o problema
- **2 linhas depois**: Código posterior ao erro
- **Indicador visual**: Seta (^) apontando para a posição exata

### ✅ **Formatação Profissional**
- **Numeração de linhas**: Para fácil referência
- **Indentação**: Preserva a estrutura do código
- **Separadores visuais**: Facilita a leitura

## Tipos de Erros Suportados

### 1. Erros de Lexer (Análise Léxica)

**Causa**: Caracteres ou tokens inválidos
**Exemplo**: `@`, `$`, caracteres especiais não reconhecidos

```bash
lx error_syntax.lx

Error in error_syntax.lx:

Lexical Error at line 6:
lexical error: {illegal,"@"}

Context:
0004 |
0005 | def invalid() do
0006 |     @invalid_token
     |
     |                   ^
0007 | end
0008 |
```

### 2. Erros de Parser (Análise Sintática)

**Causa**: Estrutura sintática inválida
**Exemplo**: Palavras-chave faltando, parênteses não balanceados

```bash
lx error_parser.lx

Error in error_parser.lx:

Syntax Error at line 6:
syntax error before: def

Context:
0004 |
0005 | def invalid() do
0006 |     def missing_do
     |
     |                   ^
0007 | end
0008 |
```

### 3. Erros de Tipo (Futuras Fases)

**Causa**: Incompatibilidade de tipos
**Exemplo**: Operações entre tipos incompatíveis

```bash
# Será implementado nas próximas fases
Type Error at line 5:
cannot add integer and string
```

### 4. Erros de Geração de Código

**Causa**: Problemas na geração de código Erlang
**Exemplo**: AST inválida, problemas de compilação

```bash
Code Generation Error: invalid AST structure
```

## Exemplos Práticos

### Exemplo 1: Erro de Caractere Inválido

**Arquivo**: `error_syntax.lx`
```lx
def answer() do
    42
end

def invalid() do
    @invalid_token
end
```

**Saída**:
```
Error in error_syntax.lx:

Lexical Error at line 6:
lexical error: {illegal,"@"}

Context:
0004 |
0005 | def invalid() do
0006 |     @invalid_token
     |
     |                   ^
0007 | end
0008 |
```

**Solução**: Remover o caractere `@` ou usar sintaxe válida

### Exemplo 2: Erro de Sintaxe

**Arquivo**: `error_parser.lx`
```lx
def answer() do
    42
end

def invalid() do
    def missing_do
end
```

**Saída**:
```
Error in error_parser.lx:

Syntax Error at line 6:
syntax error before: def

Context:
0004 |
0005 | def invalid() do
0006 |     def missing_do
     |
     |                   ^
0007 | end
0008 |
```

**Solução**: Adicionar `do` após `def missing_do`

### Exemplo 3: Erro com Contexto Amplo

**Arquivo**: `error_context.lx`
```lx
def answer() do
    42
end

def greeting() do
    "Hello, World!"
end

def status() do
    :ok
end

def invalid() do
    # This line has an error
    @invalid_token_here
end

def pi() do
    3.14159
end
```

**Saída**:
```
Error in error_context.lx:

Lexical Error at line 15:
lexical error: {illegal,"@"}

Context:
0013 | def invalid() do
0014 |     # This line has an error
0015 |     @invalid_token_here
     |
     |                        ^
0016 | end
0017 |
```

## Integração com Comandos

### Comando `ast`
```bash
lx ast arquivo_com_erro.lx
# Mostra erro detalhado e para a execução
```

### Comando `run`
```bash
lx run arquivo_com_erro.lx
# Mostra erro detalhado e para a execução
```

### Comando `compile`
```bash
lx compile arquivo_com_erro.lx
# Mostra erro detalhado e para a compilação
```

## Workflow de Debugging

### 1. Identificar o Erro
```bash
lx ast arquivo.lx
```

### 2. Analisar o Contexto
- Ver a linha exata do erro
- Entender o contexto (2 linhas antes/depois)
- Identificar o tipo de erro

### 3. Corrigir o Problema
- Baseado no tipo de erro
- Usando o contexto fornecido
- Testando a correção

### 4. Verificar a Correção
```bash
lx ast arquivo_corrigido.lx
# Deve mostrar o AST sem erros
```

## Vantagens do Sistema

### 🎯 **Precisão**
- Localização exata do erro
- Contexto relevante
- Mensagens claras

### 🚀 **Produtividade**
- Debugging mais rápido
- Menos tentativa e erro
- Correções mais precisas

### 📚 **Educacional**
- Ajuda a entender a sintaxe LX
- Mostra padrões corretos
- Facilita o aprendizado

### 🔧 **Profissional**
- Interface similar a compiladores modernos
- Experiência de desenvolvimento fluida
- Feedback imediato e útil

## Próximas Melhorias

### Fase 2: Sistema de Tipos
- Erros de tipo com contexto
- Sugestões de correção
- Verificação de tipos em tempo real

### Fase 3: Variáveis e Bindings
- Erros de variável não definida
- Conflitos de escopo
- Sugestões de nomes

### Fase 4: Operadores
- Erros de operador inválido
- Verificação de tipos de operandos
- Sugestões de operadores corretos

## Conclusão

O sistema de erros melhorado do LX2 transforma a experiência de desenvolvimento, fornecendo:

- **Feedback imediato** e preciso
- **Contexto visual** claro
- **Mensagens informativas** e úteis
- **Workflow de debugging** eficiente

Isso torna o LX2 não apenas um compilador funcional, mas uma ferramenta de desenvolvimento profissional e educacional! 🚀

---

**Comandos de teste**:
- `lx ast examples/task_01/error_syntax.lx`
- `lx ast examples/task_01/error_parser.lx`
- `lx ast examples/task_01/error_context.lx`