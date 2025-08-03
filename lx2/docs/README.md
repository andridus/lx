# Documentação LX2

## Visão Geral

LX2 é uma reescrita completa do compilador da linguagem LX usando Erlang e Yacc para definição de sintaxe. Esta implementação aproveita os pontos fortes do Erlang em programação concorrente, pattern matching e programação funcional para criar um compilador mais robusto e manutenível.

## Índice

1. [Visão Geral da Arquitetura](ARCHITECTURE.md)
2. [Comparação Erlang vs V](ERLANG_VS_V.md)
3. [Roadmap de Implementação](IMPLEMENTATION_ROADMAP.md)
4. [Sistema de Tipos Hindley-Milner](HINDLEY_MILNER.md)
5. [Definição da Gramática Yacc](YACC_GRAMMAR.md)
6. [Compilação Direta para BEAM](BEAM_COMPILATION.md)
7. [Task 1: Functions with Literals](TASK_01_IMPLEMENTATION.md)
8. [Task 2: Variables and Local Bindings](TASK_02_IMPLEMENTATION.md)
9. [Task 3: Binary Operators](TASK_03_IMPLEMENTATION.md)
10. [Estratégia de Testes](TESTING_STRATEGY.md)
11. [Considerações de Performance](PERFORMANCE.md)

## Início Rápido

```bash
# Compilar o compilador
make

# Compilar um arquivo LX (padrão: direto para BEAM)
./lx2 compile examples/simple.lx

# Gerar arquivo .erl para debugging
./lx2 compile --mode=erl examples/simple.lx

# Compilar para BEAM + gerar .erl
./lx2 compile --mode=both examples/simple.lx

# Executar testes
make test

# Gerar documentação
make docs
```

## Estrutura do Projeto

```
lx2/
├── src/                    # Código fonte Erlang
│   ├── lx2.erl            # Módulo principal do compilador
│   ├── lexer.erl          # Analisador léxico
│   ├── parser.yrl         # Arquivo de gramática Yacc
│   ├── ast.erl            # Árvore Sintática Abstrata
│   ├── types.erl          # Sistema de tipos (Hindley-Milner)
│   ├── codegen.erl        # Geração de código
│   └── utils.erl          # Funções utilitárias
├── include/               # Arquivos de cabeçalho
│   └── lx2.hrl           # Definições comuns
├── examples/              # Arquivos LX de exemplo
├── tests/                 # Suite de testes
├── docs/                  # Documentação
└── Makefile              # Configuração de build
```

## Funcionalidades

### Implementadas (Baseado no LX1)
- ✅ Literais (integers, floats, strings, atoms, booleans, nil)
- ✅ Variáveis e bindings
- ✅ Operadores binários (aritméticos, comparação, lógicos, bitwise)
- ✅ Diretivas ($print, $type)
- ✅ Listas (literais, cons, concatenação, length, membership)
- ✅ Tuplas (literais, size, element access, setelement)
- ✅ Maps (literais, access, put, remove, size)
- ✅ Records (definições, literais, access, update)
- ✅ Funções com argumentos e múltiplos heads
- ✅ Sistema de tipos Hindley-Milner

### Funcionalidades Específicas do LX2
- ✅ **Compilação direta para BEAM** (padrão)
- ✅ **Geração opcional de arquivos .erl** (debugging)
- ✅ **Modo híbrido** (BEAM + .erl)
- ✅ **Integração nativa com VM Erlang**
- ✅ **Compilação em memória** (sem I/O desnecessário)

### Planejadas para LX2
- 🔄 Sistema de módulos
- 🔄 Pattern matching
- 🔄 Guards
- 🔄 Expressões receive
- 🔄 Operador send
- 🔄 List comprehensions
- 🔄 Expressões fun
- 🔄 Suporte a binary/bitstring
- 🔄 Expressões try/catch

## Princípios de Design

1. **Erlang Nativo**: Aproveitar os pontos fortes do Erlang em pattern matching e concorrência
2. **Integração Yacc**: Usar Yacc para definição robusta de gramática
3. **Segurança de Tipos**: Implementar sistema de tipos Hindley-Milner
4. **Performance**: Otimizar para velocidade de compilação e qualidade do código gerado
5. **Manutenibilidade**: Código limpo, bem documentado com testes abrangentes
6. **Compatibilidade**: Manter compatibilidade com sintaxe LX1 onde possível

## Contribuindo

Veja [CONTRIBUTING.md](CONTRIBUTING.md) para diretrizes de desenvolvimento.

## Licença

Este projeto está licenciado sob a Licença MIT - veja [LICENSE](../LICENSE) para detalhes.