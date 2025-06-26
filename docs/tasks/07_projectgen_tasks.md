# Fase 7: Project Generation - Tarefas e Testes

## Objetivos
Gerar artefatos de projeto (app.src, rebar.config, estrutura de diretórios) e CLI funcional.

## Tarefas

### T7.1: Estrutura Base de Geração
**Descrição**: Implementar estrutura base para geração de projetos
**Entregáveis**:
- [ ] `generator/generator.v` - Estrutura principal do gerador
- [ ] `generator/templates.v` - Sistema de templates
- [ ] `generator/config.v` - Configuração de projetos
- [ ] `generator/validator.v` - Validação de configurações

**Testes**:
- Estrutura base do gerador funcionando
- Sistema de templates configurável
- Validação de configurações de projeto
- Geração de estrutura de diretórios

### T7.2: Geração de .app.src
**Descrição**: Implementar geração de arquivos .app.src
**Entregáveis**:
- [ ] Geração de .app.src para aplicações simples
- [ ] Geração de .app.src para aplicações multi-app
- [ ] Configuração de aplicações e dependências
- [ ] Configuração de módulos e funções registradas

**Testes**:
- Aplicação simples: nome, versão, aplicações, módulos
- Aplicação multi-app: múltiplas aplicações
- Dependências: aplicações OTP e Hex
- Módulos: lista de módulos da aplicação

### T7.3: Geração de rebar.config
**Descrição**: Implementar geração de arquivos rebar.config
**Entregáveis**:
- [ ] Configuração de dependências (Hex, GitHub, path)
- [ ] Configuração de plugins e tools
- [ ] Configuração de profiles (dev, prod)
- [ ] Configuração de compilação e build

**Testes**:
- Dependências Hex: `{:cowboy, "~> 2.8"}`
- Dependências GitHub: `{:my_lib, {:git, "https://github.com/user/repo"}}`
- Dependências locais: `{:local_lib, {:path, "path/to/lib"}}`
- Profiles: configurações diferentes para dev/prod

### T7.4: Estrutura de Diretórios
**Descrição**: Implementar geração de estrutura de diretórios OTP
**Entregáveis**:
- [ ] Estrutura padrão OTP (src/, include/, test/, etc.)
- [ ] Estrutura para projetos umbrella
- [ ] Estrutura para aplicações multi-app
- [ ] Arquivos de exemplo e documentação

**Testes**:
- Estrutura básica: src/, include/, test/, _build/, config/
- Projetos umbrella: apps/app1/, apps/app2/
- Multi-app: lib/app1/, lib/app2/
- Arquivos: README.md, lx.config, exemplos

### T7.5: CLI para Geração
**Descrição**: Implementar comandos CLI para geração de projetos
**Entregáveis**:
- [ ] Comando `lx new` para criar projetos
- [ ] Comando `lx init` para inicializar projetos existentes
- [ ] Comando `lx add` para adicionar aplicações
- [ ] Comando `lx config` para configurar projetos

**Testes**:
- `lx new my_app` - cria projeto básico
- `lx new my_app --umbrella` - cria projeto umbrella
- `lx init` - inicializa projeto em diretório atual
- `lx add app_name` - adiciona aplicação ao projeto

### T7.6: Configuração via lx.config
**Descrição**: Implementar sistema de configuração via arquivo
**Entregáveis**:
- [ ] Parser de arquivo lx.config
- [ ] Configuração de dependências globais
- [ ] Configuração de aplicações
- [ ] Configuração de ambientes

**Testes**:
- Dependências globais: aplicadas a todos os arquivos
- Dependências locais: sobrescrevem globais
- Configuração de aplicações: nome, versão, módulos
- Ambientes: dev, prod, test

### T7.7: Integração com rebar3
**Descrição**: Validar integração com rebar3 e Hex
**Entregáveis**:
- [ ] Validação de projetos gerados com rebar3
- [ ] Integração com Hex para dependências
- [ ] Validação de projetos umbrella
- [ ] Testes de build e run

**Testes**:
- `rebar3 compile` - compila sem erros
- `rebar3 deps` - resolve dependências
- `rebar3 test` - executa testes
- `rebar3 release` - gera release

### T7.8: Templates e Exemplos
**Descrição**: Implementar templates e exemplos de projetos
**Entregáveis**:
- [ ] Template de projeto básico
- [ ] Template de projeto com OTP
- [ ] Template de projeto umbrella
- [ ] Exemplos de código LX

**Testes**:
- Template básico: aplicação simples
- Template OTP: workers e supervisors
- Template umbrella: múltiplas aplicações
- Exemplos: código LX executável

## Critérios de Aceitação

- Todos os artefatos gerados corretamente
- CLI cobre todos os fluxos principais
- Integração com rebar3 e Hex funcionando
- Cobertura de testes > 90% para geração de projeto
- Documentação de exemplos de projetos gerados

## Atualização de Changelog

Ao final de **cada tarefa concluída**, deve ser registrada uma entrada no arquivo `LX_CHANGELOG_V.md`:

**Formato:**
```
## [Data] [Fase] Descrição resumida
- Resumo do que foi implementado/testado/validado
- Principais funcionalidades adicionadas
- Impacto no projeto (performance, funcionalidade, etc.)
- Dependências resolvidas ou criadas
```

**Exemplos:**
```
## [2024-09-03] [ProjectGen] Estrutura base e sistema de templates
- Implementada estrutura principal do gerador de projetos
- Sistema de templates configurável para diferentes tipos de projeto
- Validação de configurações de projeto e estrutura de diretórios
- Base estabelecida para geração de artefatos OTP

## [2024-09-05] [ProjectGen] Geração de .app.src e configurações
- Geração de arquivos .app.src para aplicações simples e multi-app
- Configuração automática de aplicações, dependências e módulos
- Suporte a projetos umbrella e aplicações múltiplas
- Validação de configurações de aplicação

## [2024-09-07] [ProjectGen] Geração de rebar.config e dependências
- Implementada geração de rebar.config com dependências Hex, GitHub e locais
- Configuração de plugins, tools e profiles (dev, prod)
- Suporte a configurações de compilação e build
- Integração com sistema de dependências do rebar3

## [2024-09-09] [ProjectGen] Estrutura de diretórios e CLI
- Geração de estrutura de diretórios OTP padrão
- Comandos CLI: new, init, add, config para gerenciar projetos
- Suporte a projetos umbrella e multi-app
- Arquivos de exemplo e documentação automática

## [2024-09-11] [ProjectGen] Configuração via lx.config e integração
- Sistema de configuração via arquivo lx.config
- Dependências globais e locais com prioridade
- Integração completa com rebar3 e Hex
- Validação de projetos gerados com testes de build

## [2024-09-13] [ProjectGen] Templates e exemplos finais
- Templates para projetos básicos, OTP e umbrella
- Exemplos de código LX executável em projetos gerados
- Documentação completa de uso e configuração
- Validação final com projetos reais
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais

**Duração Estimada**: 1-2 semanas
**Dependências**: Fase 6 (Linter & OTP)
**Riscos**: Baixos - geração de projetos bem definida