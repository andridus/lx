# Referência da Linguagem LX

## Visão Geral

LX é uma linguagem funcional que compila para Erlang. Ela oferece:
- Funções com múltiplas cláusulas e pattern matching com guards
- Um sistema de tipos forte com anotações de tipo, tipos customizados e geração automática de specs
- Records, listas, tuplas, mapas, binários/bitstrings
- Funções anônimas (lambdas), expressões de controle de fluxo (if, case, with, match)
- Primitivas de concorrência (spawn, send, receive)
- List comprehensions

Tudo compila para Erlang legível com anotações `-spec`.


## Criação de Projetos e CLI

Use o CLI `lx`.

```bash
# Criar um novo projeto estilo umbrella (apps/ + <project>.yml)
lx new meu_projeto

# Dentro do projeto, adicionar um novo app
cd meu_projeto
lx add app meu_app

# Compilar o projeto inteiro (gera umbrella _build/ com arquivos rebar3)
lx .

# Compilar um arquivo ou diretório (modo não-projeto)
lx compile caminho/para/arquivo.lx
lx compile caminho/para/dir

# Executar um arquivo único (leve: erlc + erl)
lx run caminho/para/arquivo.lx

# Abrir shell rebar3 para o projeto (pré-compila primeiro)
lx shell [dir]

# Criar um symlink global para o binário lx atual
sudo lx symlink [--force]
```

Layout do projeto criado por `lx new <nome>`:

```
<projeto>/
  apps/
  <projeto>.yml           # configuração do projeto para build (erl_opts, deps)
  _build/                 # umbrella gerado (rebar3, apps/*)
```

Notas:
- `<projeto>.yml` é usado para produzir `_build/rebar.config` e `config/sys.config`.
- Para cada app sob `apps/<app>/`, as fontes são compiladas em `_build/apps/<app>/src`.
- O descritor da aplicação `<app>.app.src` é gerado/garantido em `_build`.


## Sintaxe Básica

### Comentários
```lx
# comentário de linha única
```

### Identificadores
- Variáveis e funções: snake_case
- Nomes de records: snake_case (mapeia para Erlang `-record(name, ...)`)
- Nome do módulo é implicitamente o nome do arquivo

### Literais
```lx
# Números
42
3.14

# Strings (compilam para binários UTF-8)
"olá"

# Booleanos
true
false

# Nil
nil

# Átomos
:ok
:error
:timeout
```


## Coleções e Dados

### Listas
```lx
[]
[1, 2, 3]

# Cons e concatenação
[0 | [1, 2]]     # [0, 1, 2]
[1, 2] ++ [3, 4] # [1, 2, 3, 4]

# Pattern matching
[head | tail] = [1, 2, 3]
```

### Tuplas
```lx
{1, 2}
{:ok, "ok"}

{status, msg} = {:error, "ops"}
```

### Mapas
Implementado com criação, acesso, atualização e pattern matching.
```lx
usuario = %{ nome: "João", idade: 30 }
nome = usuario.nome       # acesso por ponto
idade = usuario[:idade]   # acesso por índice

usuario2 = %{ usuario | idade: 31 }             # atualização
usuario3 = %{ usuario | email: "joao@x.com" }   # adicionar chave

# pattern matching em mapas
auth = case usuario do
  %{nome: n} -> {:ok, n}
  _ -> {:error}
end
```


## Binários e Bitstrings

Tanto expressões quanto pattern matching suportam tamanhos e qualificadores (endianness, signedness, tipos).

### Expressões
```lx
<<>>
<<1, 2, 3>>

valor = 0x1234
big    = <<valor:16/big>>
little = <<valor:16/little>>

int_val = 42
float_val = 3.14
bin = <<int_val:32/integer, float_val:64/float>>

dados = "olá"
pedaco = <<dados/binary>>
```

### Pattern Matching
```lx
def parse_header(pacote) do
  <<versao:8, tamanho:16, resto/binary>> = pacote
  {versao, tamanho, resto}
end

# Opções mistas e segmento de tamanho variável
def decode(pacote) do
  <<tipo:4, _rsv:4, id:16/big, sz:32/big, payload:sz/binary, _/binary>> = pacote
  {tipo, id, payload}
end
```


## Records

Defina records com campos tipados:
```lx
record usuario { nome :: string, idade :: integer }

u = usuario{nome: "João", idade: 30}
u.nome           # acesso ao campo

# Pattern matching
def quem(u) do
  case u do
    usuario{nome: n, idade: a} when a > 18 -> n
    _ -> "menor"
  end
end
```

Erlang gerado usa `-record(usuario, ...)` e `#usuario{...}`.


## Funções

### Definições
```lx
# função pública
def adicionar(a :: integer, b :: integer) do
  a + b
end

# função privada
defp helper(x :: integer) do
  x + 1
end
```

### Múltiplas Cláusulas e Guards
```lx
def classificar do
  (n) when n > 0 -> :positivo
  (n) when n < 0 -> :negativo
  (_) -> :zero
end

# dispatch baseado em padrão
def processar_lista do
  ([]) -> "vazia"
  ([h | _]) -> "não_vazia"
end
```

### Anotação de Tipo de Retorno (em blocos multi-cláusula)
```lx
def contagem_regressiva :: string do
  (0) -> "pronto"
  (n :: integer) -> contagem_regressiva(n - 1)
end
```

### Type Specs
- Tipos são inferidos e emitidos como `-spec` em Erlang.
- Anotações de parâmetro com `::` influenciam o spec gerado.
- Funções multi-cláusula podem produzir tipos de retorno união.


## Funções Anônimas (Lambdas)

Formas single-line e multi-line, incluindo lambdas multi-head. Invocação usa `.(...)`.
```lx
# single-line
def demo() do
  f = fn(x :: integer, y :: integer) -> x + y
  f.(3, 4)
end

# multi-line do/end
def demo2() do
  g = fn(x :: integer) do
    y = x * 2
    y + 1
  end
  g.(10)
end

# multi-head
def demo3() do
  h = fn do
    (:ok) -> "sucesso"
    (:error) -> "falha"
    (_) -> "desconhecido"
  end
  h.(:ok)
end
```

Limitações:
- Funções anônimas não são recursivas (auto-chamadas são rejeitadas na implementação atual).


## Controle de Fluxo

### If
```lx
def testar_if(x) do
  if x > 0 do
    "positivo"
  else
    "não positivo"
  end
end

# If sem else retorna nil no ramo falso
```

### Case (com padrões e guards)
```lx
def lidar(resultado) do
  case resultado do
    {:ok, dados} -> dados
    {:error, motivo} -> motivo
    _ -> "desconhecido"
  end
end
```

### Expressão with
```lx
def testar_with() do
  resultado = {:sucesso, 10}
  with {:sucesso, x} <- resultado do
    x
  else
    {:error, _} -> 0
  end
end
```

### Match e Match..Rescue
```lx
# match propaga valores não correspondentes
def testar_match_simples() do
  dados = {:ok, "sucesso"}
  match {:ok, valor} <- dados
  valor
end

# rescue em caso de não correspondência
def testar_match_rescue() do
  dados = {:error, "falhou"}
  match {:ok, res} <- dados rescue err do
    {:falhou, err}
  end
  :pronto
end
```


## Concorrência

Primitivas suportadas:
- `spawn(fn() -> ... end)` – cria um processo
- Operador de envio `!`
- `receive do ... end`

```lx
def loop_servidor() do
  :ok
end

def iniciar() do
  pid = spawn(fn() -> loop_servidor() end)
  pid ! {:mensagem, "olá"}
end


def aguardar() do
  receive do
    {:mensagem, dados} -> dados
    :parar -> :ok
  end
end
```


## List Comprehensions

Compreensões de gerador único com filtro opcional e transformação. Aninhamento é suportado aninhando blocos `for`.
```lx
def quadrados() do
  numeros = [1, 2, 3, 4]
  for x in numeros do
    x * x
  end
end

# com filtro
def filtrados() do
  numeros = [1, 2, 3, 4, 5]
  for x in numeros when x > 2 do
    x
  end
end

# aninhado
def aninhados() do
  matriz = [[1, 2], [3, 4]]
  for linha in matriz do
    for x in linha do
      x + 1
    end
  end
end

# membership
def membership() do
  numeros = [1, 2, 3, 4, 5]
  permitidos = [2, 4]
  for x in numeros when x in permitidos do
    x * 10
  end
end
```


## Tipos

### Tipos Built-in
- integer, float, boolean, binary (string), atom, list(T), tuple(...), map(K, V), function

### Anotações de Tipo
```lx
def adicionar(a :: integer, b :: integer) :: integer do
  a + b
end
```

### Tipos Customizados
```lx
# Alias simples
type user_id :: integer

def id(x :: user_id) do
  x
end

# Tipo opaco
type opaque user_id :: integer

# Tipo nominal
type nominal email :: string
```

Estes geram declarações de tipo Erlang correspondentes (incluindo `-opaque` e uma tag nominal).


## Diretivas

- `@doc "Texto"` – emite um atributo `-doc` de nível de módulo para a próxima função pública
- `$print(expr)` – inspeção em tempo de compilação de uma expressão (removida da saída)
- `$type(expr)` – garante e registra o tipo inferido no spec gerado

Exemplos:
```lx
@doc "Adiciona dois números"
def adicionar(a :: integer, b :: integer) do
  $print(a)
  $type(a + b)
  a + b
end
```

Diretivas desconhecidas ou aridade incorreta produzem erros.


## Operadores e Semântica

- Aritméticos: `+ - * /`
- Comparação: `== != < <= > >=` (`!=` compila para Erlang `/=`)
- Booleanos: `and` / `or` (compilam para Erlang `andalso` / `orelse`)
- Bitwise: `&&& ||| ^^^ <<< >>>` (compilam para `band bor bxor bsl bsr`)

Precedência de operadores segue a semântica Erlang.


## Bloco Application, Imports e Deps

A linguagem suporta um bloco `application { ... }` no topo do arquivo. Seu conteúdo é atualmente emitido como comentários Erlang para documentação e ferramentas.
```lx
application {
  description: "Meu App",
  vsn: "0.1.0",
  deps: [:cowboy, :jsx],
  registered: [:servidor_principal],
  env: %{debug: true, port: 8080}
}
```

`import :modulo` é aceito e atualmente emitido como comentário (`%% Import: modulo`).

Resolução de dependências e linking em runtime são gerenciados pelo build do projeto (rebar3) gerado pelo CLI a partir de `<projeto>.yml`. O `deps` e `import` no código-fonte atuam como metadados hoje e não linkam código por si mesmos.


## Limitações e Notas

- Funções anônimas não podem ser recursivas.
- `import` e `application.deps` são metadados no código-fonte; gerenciamento real de dependências é tratado pelo umbrella rebar3 gerado pelo CLI sob `_build/`.
- If sem `else` retorna `nil` no ramo falso.
- Strings são binários UTF-8.


## Estilo

- Variáveis e funções: `snake_case`
- Records: `snake_case`
- Átomos: `:lowercase` ou `:snake_case`
- Mantenha funções pequenas e prefira múltiplas cláusulas com pattern matching e guards.


## Exemplos

### Multi-cláusula com tipos
```lx
def fatorial :: integer do
  (0) -> 1
  (n :: integer) -> n * fatorial(n - 1)
end
```

### Encode/decode binário
```lx
def encode(tipo, id, payload) do
  tamanho = byte_size(payload)
  <<tipo:4, 0:4, id:16/big, tamanho:32/big, payload/binary>>
end

def decode(pacote) do
  <<tipo:4, _rsv:4, id:16/big, sz:32/big, dados:sz/binary, _/binary>> = pacote
  {tipo, id, dados}
end
```

### With + match
```lx
def exemplo(dados_maybe) do
  with {:sucesso, dados} <- dados_maybe do
    dados
  else
    _ -> "falhou"
  end
end
```

Este documento reflete as funcionalidades validadas pela suíte de testes atual e comportamento da CLI. À medida que a linguagem evolui, as seções serão estendidas adequadamente.
