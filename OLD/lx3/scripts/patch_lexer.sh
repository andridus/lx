#!/bin/sh
# Script para incluir funções customizadas no lexer gerado

echo "Incluindo funções customizadas no lexer..."

# Adicionar tokenize ao export existente (apenas se não existir)
if ! grep -q "tokenize/1" src/lx_lexer.erl; then
    sed -i 's/-export(\[string\/1,string\/2,token\/2,token\/3,tokens\/2,tokens\/3\])./-export([string\/1,string\/2,token\/2,token\/3,tokens\/2,tokens\/3]).\n-export([tokenize\/1,tokenize\/2])./' src/lx_lexer.erl
fi

# Adicionar funções customizadas no final
cat leex/lx_lexer_custom.erl.inc >> src/lx_lexer.erl

echo "Lexer atualizado com funções customizadas."