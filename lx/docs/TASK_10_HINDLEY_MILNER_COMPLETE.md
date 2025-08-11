# Task 10: Sistema de Tipos Hindley-Milner Completo - PENDING ⏳

## Status: PENDENTE DE IMPLEMENTAÇÃO

A Task 10 implementará o sistema de tipos Hindley-Milner completo no lx1, expandindo o sistema atual para incluir generalização, instanciação, substituições completas e tipos polimórficos avançados.

## O que será implementado

### 1. **Algoritmo HM Completo** 🔄
- **Generalização** de tipos (generalize)
- **Instanciação** de tipos (instantiate)
- **Substituições** completas com composição
- **Occurs check** robusto para prevenir tipos infinitos
- **Constraint solving** avançado

### 2. **Tipos Polimórficos Avançados** 🔄
- **Tipos genéricos** completos: `list(T)`, `map(K, V)`, `tuple(A, B)`
- **Tipos de função** polimórficos: `(A, B) -> C`
- **Tipos recursivos** complexos
- **Tipos de ordem superior**: `((A -> B) -> C)`

### 3. **Sistema de Constraints Robusto** 🔄
- **Constraint collection** durante inferência
- **Constraint solving** com backtracking
- **Unificação** de tipos complexos
- **Type classes** básicas (opcional)

### 4. **Melhorias no Type Environment** 🔄
- **Escopo léxico** completo para tipos
- **Shadowing** de tipos
- **Type aliases** com resolução
- **Module-level** type definitions

## Estrutura de Arquivos

```
lx1/
├── analysis/                    # Sistema HM completo
│   ├── hm_inferencer.v         # NOVO: Inferenciador HM completo
│   ├── hm_generalization.v     # NOVO: Generalização de tipos
│   ├── hm_instantiation.v      # NOVO: Instanciação de tipos
│   ├── hm_substitution.v       # NOVO: Substituições completas
│   ├── hm_constraints.v        # NOVO: Sistema de constraints
│   ├── hm_unification.v        # MELHORADO: Unificação robusta
│   ├── type_env.v              # MELHORADO: Escopo léxico completo
│   ├── type_table.v            # MELHORADO: Cache de tipos
│   └── analyzer.v              # MELHORADO: Integração HM
├── examples/task_10/           # NEW: Examples for Task 10
│   ├── polymorphic_functions.lx
│   ├── generic_types.lx
│   ├── higher_order.lx
│   ├── recursive_types.lx
│   ├── type_aliases.lx
│   └── complex_inference.lx
└── tests/
    └── hm_complete_test.v      # NEW: Tests for Task 10
```

## Exemplos Funcionais

### Exemplo 1: Funções Polimórficas
**Entrada LX:**
```lx
def identity(x) do
    x
end

def map(f, list) do
    case list do
        [] -> []
        [head | tail] -> [f(head) | map(f, tail)]
    end
end
```

**Inferência HM:**
- `identity`: `(A) -> A` (polimórfico)
- `map`: `((A -> B), [A]) -> [B]` (polimórfico)

### Exemplo 2: Tipos Genéricos
**Entrada LX:**
```lx
def head(list) do
    [first | _] = list
    first
end

def pair(a, b) do
    {a, b}
end
```

**Inferência HM:**
- `head`: `([A]) -> A`
- `pair`: `(A, B) -> {A, B}`

### Exemplo 3: Tipos de Ordem Superior
**Entrada LX:**
```lx
def compose(f, g) do
    fn(x) do
        f(g(x))
    end
end
```

**Inferência HM:**
- `compose`: `((B -> C), (A -> B)) -> (A -> C)`

### Exemplo 4: Tipos Recursivos
**Entrada LX:**
```lx
def length(list) do
    case list do
        [] -> 0
        [_ | tail] -> 1 + length(tail)
    end
end
```

**Inferência HM:**
- `length`: `([A]) -> integer`

## Implementação Técnica

### 1. **HM Inferencer Completo**
```v
// lx1/analysis/hm_inferencer.v
pub struct HMInferencer {
mut:
    type_env     TypeEnv
    type_table   TypeTable
    constraints  []Constraint
    var_counter  int
}

pub fn (mut hmi HMInferencer) infer_expression(expr ast.Expr) !TypeInfo {
    // Implementação completa do algoritmo HM
    // - Inferência de tipos
    // - Coleta de constraints
    // - Generalização
    // - Instanciação
}
```

### 2. **Generalização de Tipos**
```v
// lx1/analysis/hm_generalization.v
pub fn generalize(type_info TypeInfo, env TypeEnv) TypeScheme {
    // Generalizar tipos livres em type_info
    // - Identificar variáveis livres
    // - Criar type scheme
    // - Quantificar variáveis
}
```

### 3. **Instanciação de Tipos**
```v
// lx1/analysis/hm_instantiation.v
pub fn instantiate(type_scheme TypeScheme) TypeInfo {
    // Instanciar type scheme
    // - Substituir variáveis quantificadas
    // - Criar novas variáveis de tipo
    // - Retornar tipo concreto
}
```

### 4. **Sistema de Constraints**
```v
// lx1/analysis/hm_constraints.v
pub struct Constraint {
    left  TypeInfo
    right TypeInfo
    position ast.Position
}

pub fn (mut hmi HMInferencer) solve_constraints() !Substitution {
    // Resolver constraints coletadas
    // - Unificação
    // - Occurs check
    // - Substituições
}
```

## Testes Completos

### 1. **Testes de Inferência Básica**
```v
// lx1/tests/hm_complete_test.v
fn test_polymorphic_identity() {
    lx_code := '
def identity(x) do
    x
end'

    result := compile_and_infer_types(lx_code)
    expected_type := '(A) -> A'

    assert result.success
    assert result.function_types['identity'] == expected_type
}
```

### 2. **Testes de Generalização**
```v
fn test_type_generalization() {
    lx_code := '
def make_list(x) do
    [x]
end'

    result := compile_and_infer_types(lx_code)
    expected_type := '(A) -> [A]'

    assert result.success
    assert result.function_types['make_list'] == expected_type
}
```

### 3. **Testes de Constraints**
```v
fn test_constraint_solving() {
    lx_code := '
def apply(f, x) do
    f(x)
end'

    result := compile_and_infer_types(lx_code)
    expected_type := '((A -> B), A) -> B'

    assert result.success
    assert result.function_types['apply'] == expected_type
}
```

### 4. **Testes de Erro**
```v
fn test_type_error_detection() {
    lx_code := '
def invalid_add(a, b) do
    a + b
end

def test() do
    invalid_add(1, "string")
end'

    result := compile_and_infer_types(lx_code)

    assert !result.success
    assert result.errors.len > 0
    assert result.errors[0].contains('type mismatch')
}
```

### 5. **Testes de Performance**
```v
fn test_large_type_inference() {
    lx_code := '
def complex_function(a, b, c, d, e) do
    temp1 = a + b
    temp2 = c * d
    temp3 = temp1 - temp2
    temp4 = temp3 / e
    temp4
end'

    result := compile_and_infer_types(lx_code)

    assert result.success
    assert result.inference_time < 100 // milliseconds
}
```

### 6. **Testes de Integração**
```v
fn test_hm_with_existing_features() {
    lx_code := '
def polymorphic_map(f, list) do
    case list do
        [] -> []
        [head | tail] -> [f(head) | polymorphic_map(f, tail)]
    end
end

def test_usage() do
    numbers = [1, 2, 3, 4, 5]
    doubled = polymorphic_map(fn(x) -> x * 2 end, numbers)
    doubled
end'

    result := compile_and_test(lx_code)

    assert result.success
    assert result.output == [2, 4, 6, 8, 10]
}
```

## Critérios de Aceitação

### ✅ Funcionalidades Obrigatórias
- [ ] Algoritmo HM completo implementado
- [ ] Generalização de tipos funcionando
- [ ] Instanciação de tipos funcionando
- [ ] Sistema de constraints robusto
- [ ] Unificação de tipos complexos
- [ ] Occurs check implementado
- [ ] Tipos polimórficos básicos
- [ ] Tipos genéricos (list, map, tuple)
- [ ] Tipos de função polimórficos
- [ ] Integração com sistema existente

### ✅ Testes Obrigatórios
- [ ] Testes de inferência polimórfica
- [ ] Testes de generalização
- [ ] Testes de instanciação
- [ ] Testes de constraint solving
- [ ] Testes de unificação complexa
- [ ] Testes de detecção de erros
- [ ] Testes de performance
- [ ] Testes de integração

### ✅ Documentação Obrigatória
- [ ] Documentação do algoritmo HM
- [ ] Exemplos de uso
- [ ] Guia de troubleshooting
- [ ] Referência de tipos
- [ ] Casos de teste documentados

## Estimativa de Tempo
- **Desenvolvimento**: 2-3 semanas
- **Testes**: 1 semana
- **Documentação**: 3-4 dias
- **Total**: 3-4 semanas

## Dependências
- Task 1-9 completas
- Sistema de tipos básico funcionando
- Parser e AST estáveis
- Gerador de código Erlang funcionando

## Próximos Passos
1. Implementar HM Inferencer completo
2. Implementar Generalização/Instanciação
3. Implementar Sistema de Constraints
4. Integrar com sistema existente
5. Implementar testes completos
6. Documentar funcionalidades