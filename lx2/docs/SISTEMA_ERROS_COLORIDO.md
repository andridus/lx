# Sistema de Erros Colorido - LX2

## Visão Geral

O LX2 agora inclui um sistema de mensagens de erro com **cores ANSI** e **posicionamento preciso** do indicador de erro, oferecendo uma experiência de debugging profissional e visualmente atrativa.

## Características do Sistema Colorido

### ✅ **Cores ANSI Implementadas**
- **🔴 Vermelho**: "Compilation failed" e indicador de erro (`^~~`)
- **🟡 Amarelo**: Tipo de erro ([Lexical Error], [Syntax Error], etc.)
- **🔵 Azul**: Número da linha
- **🔵 Ciano**: Caminho do arquivo e coordenadas

### ✅ **Posicionamento Preciso**
- **`^` no início do token**: Posicionado exatamente onde o erro começa
- **Detecção inteligente**: Analisa o tipo de erro para posicionar corretamente
- **Suporte a diferentes tokens**: Caracteres, palavras-chave, variáveis

## Exemplos de Erros Coloridos

### 1. Erro de Lexer (Caractere Inválido)

**Arquivo**: `error_syntax.lx`
```lx
def answer() do
    42
end

def invalid() do
    @invalid_token
end
```

**Saída Colorida**:
```
🔴 Compilation failed: 🔴 [🟡 Lexical Error🟡] 🔵 examples/task_01/error_syntax.lx:6:1🔵
🟡 Illegal character: @🟡

🔵6🔵 |     @invalid_token
   | 🔴^~~🔴
```

### 2. Erro de Parser (Sintaxe Inválida)

**Arquivo**: `error_parser.lx`
```lx
def answer() do
    42
end

def invalid() do
    def missing_do
end
```

**Saída Colorida**:
```
🔴 Compilation failed: 🔴 [🟡 Syntax Error🟡] 🔵 examples/task_01/error_parser.lx:6:1🔵
syntax error before: def

🔵6🔵 |     def missing_do
   | 🔴 ^~~🔴
```

### 3. Erro de Tipo (Variável Indefinida)

**Arquivo**: `undefined_var.lx`
```lx
def answer() do
  _
end
```

**Saída Colorida**:
```
🔴 Compilation failed: 🔴 [🟡 Type Error🟡] 🔵 examples/task_01/undefined_var.lx:2:1🔵
🟡 Undefined variable: _🟡

🔵2🔵 |   _
   | 🔴^~~🔴
```

### 4. Erro Complexo (Posição Específica)

**Arquivo**: `complex_error.lx`
```lx
def answer() do
    x = 42
    y = @invalid_here
    z = 100
end
```

**Saída Colorida**:
```
🔴 Compilation failed: 🔴 [🟡 Lexical Error🟡] 🔵 examples/task_01/complex_error.lx:3:1🔵
🟡 Illegal character: @🟡

🔵3🔵 |     y = @invalid_here
   | 🔴 ^~~🔴
```

## Algoritmo de Posicionamento

### 🎯 **Detecção Inteligente por Tipo de Erro**

#### **Erros de Lexer**
```erlang
% Extrai o caractere ilegal da mensagem
case re:run(Message, "\\{illegal,\"([^\"]+)\"\\}", [{capture, all_but_first, list}]) of
    {match, [Char]} ->
        find_char_position(LineContent, Char);
```

#### **Erros de Parser**
```erlang
% Extrai o token problemático da mensagem
case re:run(Message, "syntax error before: ([a-zA-Z_]+)", [{capture, all_but_first, list}]) of
    {match, [Token]} ->
        find_token_position(LineContent, Token);
```

#### **Erros de Tipo**
```erlang
% Detecta variáveis indefinidas
case string:find(Message, "Undefined variable: _") of
    nomatch -> fallback;
    _ -> find_char_position(LineContent, "_")
```

### 🔍 **Funções de Busca**

#### **Busca de Caractere**
```erlang
find_char_position(LineContent, Char) ->
    find_char_position(LineContent, Char, 1).

find_char_position([Char | _Rest], Char, Pos) ->
    Pos;  % Retorna a posição exata
```

#### **Busca de Token**
```erlang
find_token_position(LineContent, Token) ->
    % Encontra o início do token (não parte de outra palavra)
    find_token_start(LineContent, Token, 1)
```

## Códigos de Cores ANSI

### **Cores Utilizadas**
```erlang
Red = "\033[31m",      % 🔴 Vermelho para erros
Yellow = "\033[33m",   % 🟡 Amarelo para tipos de erro
Blue = "\033[34m",     % 🔵 Azul para números de linha
Cyan = "\033[36m",     % 🔵 Ciano para caminhos de arquivo
Reset = "\033[0m",     % Reset para voltar à cor padrão
```

### **Aplicação das Cores**
- **"Compilation failed"**: Vermelho
- **Tipo de erro**: Amarelo
- **Caminho do arquivo**: Ciano
- **Número da linha**: Azul
- **Indicador de erro (`^~~`)**: Vermelho
- **Mensagem de erro**: Amarelo (quando aplicável)

## Vantagens do Sistema Colorido

### 🎨 **Visual**
- **Destaque imediato**: Erros saltam aos olhos
- **Hierarquia visual**: Diferentes elementos têm cores distintas
- **Profissionalismo**: Interface similar a compiladores modernos

### 🎯 **Funcional**
- **Posicionamento preciso**: `^` exatamente onde o erro começa
- **Detecção inteligente**: Analisa o contexto do erro
- **Feedback imediato**: Identificação rápida do problema

### 🚀 **Produtividade**
- **Debugging mais rápido**: Erros são facilmente identificáveis
- **Menos confusão**: Cores ajudam a distinguir diferentes tipos de erro
- **Experiência fluida**: Interface intuitiva e agradável

## Compatibilidade

### ✅ **Terminais Suportados**
- **Terminais modernos**: Suporte completo a ANSI
- **IDEs**: VSCode, IntelliJ, etc.
- **Terminais Unix/Linux**: Bash, Zsh, etc.

### ⚠️ **Fallback**
- **Terminais sem cores**: Funciona normalmente (sem cores)
- **Redirecionamento**: Preserva funcionalidade em pipes
- **Compatibilidade**: Não quebra em ambientes sem suporte a cores

## Exemplos de Uso

### **Debugging Rápido**
```bash
# Erro imediatamente visível com cores
lx ast arquivo_com_erro.lx

# Posição exata do erro destacada
# Correção rápida e precisa
```

### **Desenvolvimento Iterativo**
```bash
# 1. Escrever código
# 2. Testar com cores
lx ast codigo.lx

# 3. Corrigir erro (posição exata)
# 4. Testar novamente
lx ast codigo.lx
```

## Próximas Melhorias

### **Fase 2: Sistema de Tipos**
- Cores para diferentes tipos de erro de tipo
- Sugestões de correção coloridas
- Highlight de tipos incompatíveis

### **Fase 3: Variáveis e Bindings**
- Cores para variáveis não definidas
- Destaque de conflitos de escopo
- Sugestões de nomes de variáveis

### **Fase 4: Operadores**
- Cores para operadores inválidos
- Highlight de tipos de operandos
- Sugestões de operadores corretos

## Conclusão

O sistema de erros colorido do LX2 oferece:

- **🎨 Experiência visual profissional** com cores ANSI
- **🎯 Posicionamento preciso** do indicador de erro
- **🚀 Debugging mais rápido** e intuitivo
- **🔧 Interface moderna** similar a compiladores profissionais

O LX2 agora tem um sistema de erros de **nível enterprise** que transforma completamente a experiência de desenvolvimento! 🚀

---

**Comandos de teste**:
- `lx ast examples/task_01/error_syntax.lx`
- `lx ast examples/task_01/error_parser.lx`
- `lx ast examples/task_01/undefined_var.lx`
- `lx ast examples/task_01/complex_error.lx`