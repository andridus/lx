# Task: Implementação das Sintaxes de Worker e Supervisor em LX

## Objetivo
Implementar suporte completo às novas sintaxes de `worker` e `supervisor` na linguagem LX, incluindo o caso especial de supervisor sem nome para projetos com bloco `application`.

---

## 1. Parsing: Suporte à Nova Sintaxe

### 1.1. Worker

- Sintaxe:
  ```lx
  worker nome_modulo do
    # corpo: funções, records, tipos, etc.
  end
  ```
- O parser deve reconhecer o bloco `worker nome_modulo do ... end` como um novo tipo de declaração de módulo.
- O corpo do bloco pode conter:
  - Funções (`def`, `defp`)
  - Records
  - Tipos

### 1.2. Supervisor

- Sintaxe com nome:
  ```lx
  supervisor nome_modulo do
    children [nome_worker, nome_1_worker]
    strategy one_for_one
    # corpo: funções, records, tipos, etc.
  end
  ```
- Sintaxe alternativa:
  ```lx
  supervisor nome_modulo do
    children %{
      worker: [nome_worker, nome_1_worker],
      supervisor: [nome_supervisor]
    }
    strategy one_for_one
    # corpo: funções, records, tipos, etc.
  end
  ```
- Sintaxe **sem nome** (válida apenas em projetos com bloco `application`):
  ```lx
  supervisor do
    children [meu_worker, outro_worker]
    strategy one_for_one
    # corpo: funções, records, tipos, etc.
  end
  ```
- O parser deve reconhecer todos os formatos acima.
- O corpo do bloco pode conter:
  - Funções (`def`, `defp`)
  - Records
  - Tipos

---

## 2. Análise Semântica

### 2.1. Worker

- Exigir a implementação da função `init`.
- Exigir pelo menos uma das funções: `handle_call`, `handle_cast`, `handle_continue`, `handle_info`.
- Validar que o nome do worker (alias) não conflita com outros módulos, workers ou supervisors.

### 2.2. Supervisor

- Validar que os nomes em `children` referenciam workers ou supervisors válidos.
- Validar que o nome do supervisor (alias) não conflita com outros módulos, workers ou supervisors.
- Validar que a estratégia (`strategy`) é um dos valores permitidos (`one_for_one`, `one_for_all`, etc).

### 2.3. Supervisor sem nome

- Só é permitido se houver um bloco `application` no projeto.
- Só pode haver **um** supervisor sem nome por projeto.
- Este supervisor será tratado como o supervisor principal do application.
- Se não houver `application`, um erro deve ser emitido.
- Se houver mais de um supervisor sem nome, erro.

### 2.4. Geral

- O analisador deve impedir que haja dois módulos (incluindo workers e supervisors) com o mesmo nome (alias).

---

## 3. Geração de Código Erlang

### 3.1. Worker

- Gerar um arquivo `{nome_modulo}.erl` para cada worker.
- O arquivo deve conter:
  - Um módulo Erlang com o nome correspondente (convertendo o alias para nome de arquivo/módulo).
  - Implementação das funções `init`, `handle_call`, etc., conforme definidas no LX.
  - Exports apropriados para as funções exigidas pelo OTP.

### 3.2. Supervisor

- Gerar um arquivo `{nome_modulo}.erl` para cada supervisor.
- O arquivo deve conter:
  - Um módulo Erlang com o nome correspondente (convertendo o alias para nome de arquivo/módulo).
  - Implementação da função `init/1` que define os filhos e a estratégia.
  - Exports apropriados para supervisor.

### 3.3. Supervisor sem nome

- O supervisor sem nome deve ser gerado como o supervisor principal do projeto.
- O nome do arquivo e do módulo pode ser derivado do nome do projeto (ex: `main_supervisor.erl` ou `{nome_do_projeto}_sup.erl`).
- O bloco `application` deve referenciar este supervisor como seu root supervisor.

### 3.4. Corpo do Bloco

- Funções, records e tipos definidos dentro do bloco devem ser incluídos no módulo gerado.

---

## 4. Testes

- Adicionar exemplos de uso de `worker` e `supervisor` (com e sem nome) nos testes de sintaxe e geração de código.
- Testar casos de erro: nomes duplicados, ausência de funções obrigatórias, children inválidos, uso de supervisor sem nome em arquivos isolados, múltiplos supervisores sem nome, etc.

---

## 5. Documentação

- Atualizar a referência da linguagem (`docs/pt_br/REFERENCIA_LINGUAGEM.md` e `docs/LX_SYNTAX_REFERENCE.md`) com exemplos e explicações das novas sintaxes.
- Incluir exemplos de supervisor sem nome e sua relação com o bloco `application`.

---

## Exemplos

### Worker
```lx
worker meu_worker do
  record State { count :: integer }

  def init(args) do
    {:ok, State{count: 0}}
  end

  def handle_call({:inc, n}, _from, state) do
    {:reply, :ok, State{state | count: state.count + n}}
  end
end
```

### Supervisor com nome
```lx
supervisor meu_supervisor do
  children [meu_worker, outro_worker]
  strategy one_for_one

  defp helper_fun() do
    # ...
  end
end
```

### Supervisor sem nome (root supervisor)
```lx
application {
  description: "Meu app LX",
  vsn: "1.0.0",
  applications: [:kernel, :stdlib],
  registered: [],
  env: %{},
  deps: []
}

supervisor do
  children [meu_worker, outro_worker]
  strategy one_for_one
end
```

---

## Observações
- O nome do worker/supervisor é um alias (identificador), sem `:` e sem PascalCase.
- Os nomes em `children` também são aliases.
- O supervisor sem nome só é permitido em projetos com bloco `application` e será o root supervisor do projeto.