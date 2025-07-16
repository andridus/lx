# Referência da Linguagem LX

## Visão Geral

LX é uma linguagem de programação funcional que compila para Erlang, combinando a sintaxe limpa do Elixir com recursos modernos de programação funcional e um sistema de tipos robusto.

## Criação de Projetos

Para criar um novo projeto LX, utilize o comando:

```bash
lx new nome_do_projeto
```

Isso irá gerar a seguinte estrutura:

```
nome_do_projeto/
├── application.lx         # Configuração principal do projeto
├── src/                  # Código-fonte LX
│   └── nome_do_projeto.lx # Módulo principal
```

Exemplo de `application.lx`:
```lx
application {
  description: "example - A Lx Application",
  vsn: "1.0.0",
  applications: [:kernel, :stdlib],
  registered: [],
  env: %{
    debug: true,
    timeout: 5000,
    port: "8080"
  },
  deps: []
}
```

Exemplo de arquivo principal em `src/`:
```lx
# Main application module
record User{
    name :: string,
    age :: integer
}

def new_user(name :: string) do
    User{name: name, age: 10}
end
```

## Sintaxe Fundamental

### Comentários

```lx
# Comentário de linha simples
# Comentários começam com # e vão até o fim da linha
```

### Identificadores

```lx
# Identificadores de variáveis e funções (snake_case)
variavel
nome_da_funcao
contador_1

# Identificadores records (PascalCase)
MinhaEstrutura
ModuloExemplo
```

### Literais

#### Números

```lx
# Inteiros
42
0
1_000_000  # Separadores para legibilidade

# Ponto flutuante
3.14
2.71e8     # Notação científica
```

#### Strings

```lx
# Strings com aspas duplas
"Olá, mundo!"
"String com \"aspas\" escape"
"String com \n quebra de linha"

# Interpolação de strings
nome = "João"
idade = 25
"Olá, #{nome}! Você tem #{idade} anos."
```

#### Booleanos

```lx
true
false
```

#### Nil

```lx
nil  # Representa ausência de valor
```

#### Átomos

```lx
:ok
:error
:timeout
:atom_with_underscores
```

### Variáveis

```lx
# Declaração e atribuição
nome = "Alice"
idade = 30
ativo = true

# Variáveis são imutáveis por padrão
x = 10
# x = 20  # Erro: não pode reatribuir

# Pattern matching em atribuições
{:ok, resultado} = operacao()
[primeiro | resto] = lista
```

#### Escopo de Variáveis

```lx
# Variáveis têm escopo léxico (limitado ao bloco onde são definidas)
# Bloco com escopo local
def fun do
  soma = do
    y = 10  # Variável local do bloco
    z = 20  # Variável local do bloco
    y + z   # Resultado do bloco
  end
  .
  .
  .
  soma
end

# Escopo em funções
def calcular() do
  a = 15  # Variável local da função
  b = 25  # Variável local da função

  # Bloco aninhado
  resultado = do
    temp = a * 2  # Pode acessar 'a' do escopo externo
    temp + b      # Pode acessar 'b' do escopo externo
  end

  resultado
end

# Escopo em case/if
valor = case numero do
  n when n > 0 ->
    positivo = true    # Variável local do case
    "positivo"
  _ ->
    negativo = true    # Variável local do case
    "não positivo"
end
# positivo e negativo não são acessíveis aqui
```

## Tipos de Dados

### Listas

```lx
# Lista vazia
[]

# Lista com elementos
[1, 2, 3, 4, 5]
["a", "b", "c"]
[1, "dois", :tres]  # Tipos mistos

# Operações com listas
lista = [1, 2, 3]
nova_lista = [0 | lista]  # [0, 1, 2, 3] (cons)
concatenada = [1, 2] ++ [3, 4]  # [1, 2, 3, 4]

# Decomposição
[head | tail] = [1, 2, 3]
# head = 1, tail = [2, 3]
```

### Tuplas

```lx
# Tupla vazia
{}

# Tuplas com elementos
{1, 2}
{:ok, "sucesso"}
{:error, "falha", 404}
{"nome", "idade", true}

# Acesso por pattern matching
{status, mensagem} = {:ok, "processado"}
```

### Mapas

```lx
# Mapa vazio
%{}

# Mapa com chaves e valores
usuario = %{
  nome: "João",
  idade: 25,
  ativo: true
}

# Acesso a valores
nome = usuario.nome
idade = usuario[:idade]

# Atualização (retorna novo mapa)
usuario_atualizado = %{usuario | idade: 26}

# Adição de chaves
usuario_com_email = %{usuario | email: "joao@email.com"}
```

### Records

```lx
# Definição de record
record Pessoa {
  nome :: string,
  idade :: integer,
  email :: string
}

# Criação de instância
pessoa = Pessoa{
  nome: "Maria",
  idade: 28,
  email: "maria@email.com"
}

# Acesso a campos
nome = pessoa.nome
idade = pessoa.idade

# Atualização (retorna novo record)
pessoa_atualizada = Pessoa{pessoa | idade: 29}

# Pattern matching
case pessoa do
  Pessoa{nome: nome, idade: idade} when idade >= 18 ->
    "#{nome} é maior de idade"
  Pessoa{nome: nome} ->
    "#{nome} é menor de idade"
end
```

## Funções

### Definição de Funções

```lx
# Função simples
def somar(a, b) do
  a + b
end

# Função privada
defp multiplicar(a, b) do
  a * b
end

# Função com múltiplas cláusulas
def fatorial do
  (0) ->   1
  (n) when n > 0 -> n * fatorial(n - 1)
end

# Função multiplas clausulas com guards
def categorizar_idade do
    (idade) when idade < 18 -> :menor
    (idade) when idade >= 18 and idade < 65 -> :adulto
    (_) -> :idoso
end
```

### Funções Anônimas

```lx
# Função anônima simples
quadrado = fn(x) -> x * x end

# Uso
resultado = quadrado(5)  # 25

# Função anônima com múltiplas cláusulas
processar = fn
  {:ok, valor} -> valor * 2
  {:error, _} -> 0
end
```

### Especificações de Tipo

```lx
# Especificação de tipo para função
def somar(a :: integer, b :: integer) do
  a + b
end

# Especificação com tipos genéricos
def mapear(lista :: list(any), funcao :: (any -> integer)) :: integer do
  case lista do
    [] -> []
    [h | t] -> [funcao(h) | mapear(t, funcao)]
  end
end
```

## Controle de Fluxo

### Expressões Condicionais

```lx
# If-else
resultado = if temperatura > 30 do
  "quente"
else
  "normal"
end

# Case
status = case resposta do
  {:ok, dados} -> {:sucesso, dados}
  {:error, motivo} -> {:falha, motivo}
  _ -> {:desconhecido}
end
```

### Pattern Matching

```lx
# Pattern Match com listas
def processar_lista(lista) do
  case lista do
    [] -> :vazia
    [item] -> {:um_item, item}
    [primeiro, segundo | resto] ->
      {:multiplos, primeiro, segundo, resto}
  end
end

# Pattern Match com tuplas
def processar_resultado(resultado) do
  case resultado do
    {:ok, valor} -> valor
    {:error, :not_found} -> "não encontrado"
    {:error, motivo} -> "erro: #{motivo}"
  end
end

# Pattern Match com records
def processar_pessoa(pessoa) do
  case pessoa do
    Pessoa{nome: nome, idade: idade} when idade >= 18 ->
      "#{nome} pode votar"
    Pessoa{nome: nome} ->
      "#{nome} não pode votar ainda"
  end
end
```

### Guards

```lx
# Guards em funções
def classificar_numero do
  (n) when n > 0 ->  :positivo
  (n) when n < 0 ->  :negativo
  (n) when n == 0 -> :zero
end

# Guards compostos
def validar_usuario do
  (nome, idade) when is_string(nome) and idade >= 0 -> :valido
  (_, _) -> :invalido
end
```

## Concorrência

### Processos

```lx
# Spawn de processo
pid = spawn(fn ->
  # Código do processo
  loop()
end)

# Envio de mensagem
pid ! {:mensagem, "dados"}

# Recebimento de mensagem
receive do
  {:mensagem, dados} ->
    # Processar dados
    processar(dados)
  {:stop} ->
    :ok
after
  5000 ->
    :timeout
end
```

### Supervisores

```lx
# Configuração de supervisor
supervisor locate_supervisor do
  strategy :one_for_one
  children [
    worker: [MeuWorker]
    supervisor: [MeuSupervisor]
  ]
  def start_link(_) do
    # Lógica de inicialização
  end
end
```

### Workers

```lx
# Configuração de worker
worker locate_worker do
  def start_link(_) do
    # Lógica de inicialização
  end
end
```

## Tipos Customizados

### Definições de Tipo

A linguagem LX possui um sistema de tipos robusto, incluindo tipos nominais, opacos, genéricos e recursivos.

#### Tipo união
```lx
type status :: :ok | :error | :pending
```

#### Tipo genérico
```lx
type result(T) :: {:some, T} | :none
```

#### Tipo recursivo
```lx
type list(T) :: [] | {T, list(T)}
```

#### Alias simples
```lx
type nome :: string
type idade :: integer
```

#### Alias opaco
```lx
type opaque user_id :: integer
```

#### Alias nominal
```lx
type nominal email :: string
```

#### Uso de tipos em funções
```lx
def soma(a :: integer, b :: integer) :: integer do
  a + b
end

def processa_resultado(res :: result(integer)) :: integer do
  case res do
    {:some, v} -> v
    :none -> 0
  end
end
```

#### Tipos em records
```lx
record Pessoa {
  nome :: string,
  idade :: integer,
  email :: email
}
```

## Módulos

### Estrutura de Módulo
No lx não se declara módulo, o nome do módulo é o nome do arquivo.
```lx

# Deps
deps [:cowboy, :outro_modulo]

# Exportações são automáticas para funções públicas (def)
# Funções privadas usam defp

# Definições de tipos
record Usuario {
  id :: integer,
  nome :: string
}

# Funções públicas
def criar_usuario(nome) do
  Usuario{id: gerar_id(), nome: nome}
end

# Funções privadas
defp gerar_id() do
  :random.uniform(1000000)
end
```

## Tratamento de Erros

### With Expression

```lx
def processar_dados(dados) do
  with {:ok, validados} <- validar(dados),
       {:ok, processados} <- processar(validados),
       {:ok, salvos} <- salvar(processados) do
    {:ok, salvos}
  else
    {:error, motivo} -> {:error, motivo}
  end
end
```

### Match expression

```lx
def processar_dados(dados) do
  match {:ok, validados} <- validar(dados)
  match {:ok, processados} <- processar(validados)
  match {:ok, salvos} <- salvar(processados)
  {:ok, salvos}
end
```

### Match..Rescue expression

```lx
def processar_dados(dados) do
  match {:ok, validados} <- validar(dados) rescue error do {:error, error} end
  match {:ok, processados} <- processar(validados) rescue error do {:error, error} end
  match {:ok, salvos} <- salvar(processados) rescue error do {:error, error} end
  {:ok, salvos}
end
```

## Diretivas

```lx
# Documentação
@doc "Calcula a soma de dois números"
def calcular(a, b) do
  a + b
end

# Reflexão
@reflection
def info_funcao() do
  # Informações sobre a função em tempo de execução
end
```

## Testes

### Testes Unitários

```lx
# Definição de teste
describe "Módulo de matemática" do
  test "soma de números positivos" do
    assert somar(2, 3) == 5
  end

  test "soma com zero" do
    assert somar(5, 0) == 5
    assert somar(0, 5) == 5
  end
end
```

## Exemplos Práticos

### Servidor de Estado

```lx
record Estado {
  contador :: integer,
  nome :: string
}

def iniciar_servidor() do
  spawn(fn -> loop(Estado{contador: 0, nome: "servidor"}) end)
end

defp loop(estado) do
  receive do
    {:incrementar, pid} ->
      novo_estado = Estado{estado | contador: estado.contador + 1}
      pid ! {:ok, novo_estado.contador}
      loop(novo_estado)

    {:obter_estado, pid} ->
      pid ! {:ok, estado}
      loop(estado)

    {:parar} ->
      :ok
  end
end
```

### Processamento de Lista

```lx
def processar_numeros(numeros) do
  numeros
  |> filtrar_positivos()
  |> mapear_quadrados()
  |> somar_todos()
end

defp filtrar_positivos(lista) do
  case lista do
    [] -> []
    [h | t] when h > 0 -> [h | filtrar_positivos(t)]
    [_ | t] -> filtrar_positivos(t)
  end
end

defp mapear_quadrados(lista) do
  case lista do
    [] -> []
    [h | t] -> [h * h | mapear_quadrados(t)]
  end
end

defp somar_todos(lista) do
  case lista do
    [] -> 0
    [h | t] -> h + somar_todos(t)
  end
end
```

### Sistema de Usuários

```lx
record Usuario {
  id :: integer,
  nome :: string,
  email :: string,
  ativo :: boolean
}

def criar_usuario(nome :: string, email :: string) :: {:ok, Usuario} | {:error, string} do
  match :ok <- validar_nome(nome)
  match :ok <- validar_email(email)
  {:ok, Usuario{
    id: gerar_id(),
    nome: nome,
    email: email,
    ativo: true
  }}
end

defp validar_nome(nome) when is_string(nome) and nome != "" do
  :ok
end

defp validar_nome(_) do
  {:error, "nome inválido"}
end

defp validar_email(email) when is_string(email) do
  # Validação simplificada
  case String.contains?(email, "@") do
    true -> :ok
    false -> {:error, "email inválido"}
  end
end
```

## Convenções de Estilo

### Nomenclatura

- **Variáveis e funções**: `snake_case`
- **Records e módulos**: `PascalCase`
- **Átomos**: `:lowercase` ou `:snake_case`

### Indentação

- Use 2 espaços para indentação
- Alinhe elementos de estruturas de dados
- Quebre linhas longas de forma legível

### Comentários

- Use comentários para explicar *por que*, não *o que*
- Mantenha comentários atualizados com o código
- Use docstrings para documentar APIs públicas

## Ferramentas de Desenvolvimento

### Compilação

```bash
# Compilar arquivo único
lx compile arquivo.lx

# Compilar projeto (diretório)
lx compile caminho/do/projeto

# Compilar com debug dos tokens
gx compile arquivo.lx --debug-tokens

# Compilar com debug do sistema de tipos
lx compile arquivo.lx --debug-types

# Compilar sem rodar o rebar3 (apenas gera código Erlang)
lx compile caminho/do/projeto --no-rebar-compile
```

### Shell interativo do projeto

```bash
# Iniciar shell Erlang no contexto do projeto (compila antes de abrir o shell)
lx shell caminho/do/projeto
```

### Criação de link simbólico global

```bash
# Cria um symlink /usr/local/bin/lx para o binário atual
sudo lx symlink
```

## Opções do Compilador LX

- `lx new <nome>`: Cria um novo projeto LX
- `lx compile <arquivo.lx|diretorio> [flags]`: Compila um arquivo ou projeto LX
  - `--debug-tokens`: Mostra tokens gerados pelo lexer
  - `--debug-types`: Mostra inferência e checagem de tipos
  - `--no-rebar-compile`: Não executa o rebar3 após gerar o código Erlang
- `lx shell [diretorio]`: Compila e abre shell Erlang no contexto do projeto
- `lx symlink`: Cria um link simbólico global para o comando `lx`

Esta referência cobre os principais aspectos da linguagem LX. Para exemplos mais detalhados e casos de uso específicos, consulte a documentação completa e os exemplos no diretório `ex/`.
`
