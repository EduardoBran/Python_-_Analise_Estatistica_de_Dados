####  Big Data Real-Time Analytics com Python e Spark  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/2.Big-Data-Real-Time-Analytics-com-Python-e-Spark/7.Analise_Estatistica")
getwd()



## Importando Pacotes
library(readxl)         # carregar arquivos
library(dplyr)          # manipula dados
library(tidyr)          # manipula dados (funcao pivot_longer)
library(ggplot2)        # gera gráficos
library(patchwork)      # unir gráficos
library(corrplot)       # mapa de Correlação
library(caret)          # pacote preProcess para normalização

library(randomForest)




#############################             Estudo de Caso             #############################


####  Pré Processamento de Dados Para E-Commerce Analytics



### Contexto:

# - Um provedor de internet oferece diversos tipos de planos de acesso com diferentes larguras de banda. A empresa então fez um upgrade da sua infraestrutura
#   e gostaria de validar algumas hipóteses sobre o consumo de largura de banda dos clientes.

# - A empresa possuía dados do consumo médio de largura de banda antes do upgrade e registrou o consumo no primeiro mês após o upgrade, no segundo mês e no 
#   mês anterior ao upgrade. Os dados foram registrados junto com algumas características dos clientes.

# - Algumas perguntas de negócio foram formuladas e agora iremos respondê-las através de **4 testes de hipótese**.

# - Os dados usados são fictícios.



### Objetivo

## O objetivo deste projeto é demonstrar na pŕatica como utilizamos um Teste de Hipóteste do início ao fim.

# - Começando com a formulação do problema.
# - Escolha do teste ideal para o problema.
# - Formulação das hipóteses.
# - Preparação dos Dados (se necessário).
# - Execução do Teste.
# - Interpretação do Resultado do Teste.



### Testes

## Serão Realizados 4 Testes Diferentes

# - 3 Testes para Variáveis Numéricas
# - 1 Teste para Variável Categórica



### Perguntas

## 1) O consumo médio de largura de banda do mês anterior ao upgrade foi maior que 50?
#     (Neste caso usaremos um Teste t de Uma Amostra)

## 2) O consumo médio de largura de banda do primeiro mês após ao upgrade foi maior que 54?
#     (Neste caso usaremos um Teste t de Uma Amostra)

## 3) Houve diferença de consumo de largura de banda antes e depois do upgrade, considerando o primeiro mês após o upgrade?
#     (Neste caso usaremos: Teste t de Duas Amostras (Pareado))

## 4) O gênero do cliente influenciou o consumo de largura de banda no primeiro mês após o upgrade?
#     (Neste caso usaremos: Teste t de Duas Amostras Independentes)

## 5) Há alguma relação entre região e segmento do cliente?
#     (Neste caso usaremos: Teste do Qui-Quadrado)




#### Carregando os Dados
df <- data.frame(read.csv2("dados/dataset.csv", sep = ","))



#### Realizando Análise Inicial (Sumário Estatístico, Veriricação de Valores NA, '' e especiais)

analise_inicial <- function(dataframe_recebido) {  # para encotrar linhas com caracter especial, vá para o fim do script
  # Sumário
  cat("\n\n####  DIMENSÕES  ####\n\n")
  print(dim(dataframe_recebido))
  cat("\n\n\n####  INFO  ####\n\n")
  print(str(dataframe_recebido))
  cat("\n\n\n####  SUMÁRIO  ####\n\n")
  print(summary(dataframe_recebido))
  cat("\n\n\n####  VERIFICANDO QTD DE LINHAS DUPLICADAS  ####\n\n")
  print(sum(duplicated(dataframe_recebido)))
  cat("\n\n\n####  VERIFICANDO VALORES NA  ####\n\n")
  valores_na <- colSums(is.na(dataframe_recebido))
  if(any(valores_na > 0)) {
    cat("\n-> Colunas com valores NA:\n\n")
    print(valores_na[valores_na > 0])
  } else {
    cat("\n-> Não foram encontrados valores NA.\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES VAZIOS ''  ####\n\n")
  valores_vazios <- sapply(dataframe_recebido, function(x) sum(x == ""))
  if(any(valores_vazios > 0)) {
    cat("\n-> Colunas com valores vazios \"\":\n\n")
    print(valores_vazios[valores_vazios > 0])
  } else {
    cat("\n-> Não foram encontrados valores vazios \"\".\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES COM CARACTERES ESPECIAIS  ####\n\n")
  caracteres_especiais <- sapply(dataframe_recebido, function(x) {
    sum(sapply(x, function(y) {
      if(is.character(y) && length(y) == 1) {
        any(charToRaw(y) > 0x7E | charToRaw(y) < 0x20)
      } else {
        FALSE
      }
    }))
  })
  if(any(caracteres_especiais > 0)) {
    cat("\n-> Colunas com caracteres especiais:\n\n")
    print(caracteres_especiais[caracteres_especiais > 0])
  } else {
    cat("\n-> Não foram encontrados caracteres especiais.\n")
  }
}

analise_inicial(df)


## Modificando todas as variáveis do tipo chr para numérico usando mutate e across
df <- df %>%
  mutate(across(where(is.character), as.numeric))
str(df)



## Pergunta 1:

# - O consumo médio de largura de banda do mês anterior ao upgrade foi maior que 50?
#   Neste caso usaremos um Teste t de Uma Amostra.

# -> Quando usar : O Teste t de Uma Amostra é usado quando queremos comparar a média de uma amostra única com uma média populacional conhecida ou suposta,
#                 na ausência de informações sobre o desvio padrão da população.

# -> Por quê usar: Este teste é útil quando temos uma pequena amostra (tipicamente menos que 30) e não conhecemos o desvio padrão da população.
#                  Ele nos ajuda a entender se a média da amostra é significativamente diferente da média populacional proposta.

# -> Motivo da : O Teste t de Uma Amostra foi escolhido para essa situação específica porque ele é adequado para avaliar a média de uma única amostra
#    escolha     de dados contra uma média populacional conhecida ou uma média de referência específica. Neste caso, você quer testar se o consumo
#                médio de largura de banda do mês anterior ao upgrade é significativamente diferente de 50 (um valor especificado).

# Hipóteses:
   
#   H0: O consumo médio de largura de banda antes do upgrade foi igual a 50.
#   H1: O consumo médio de largura de banda antes do upgrade foi diferente de 50.
 
# - Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.
# - Se um valor-p relatado a partir de um teste t for menor que 0,05, esse resultado é considerado estatisticamente significativo.
#   Se um valor-p for maior que 0,05, o resultado é insignificante.

# -> Um valor-p de um teste t é a probabilidade de que os resultados de seus dados de amostra tenham ocorrido por acaso.







## Pergunta 2:

# - O consumo médio de largura de banda do primeiro mês após ao upgrade foi maior que 54?
#   Neste caso usaremos um Teste t de Uma Amostra.

# Motivo da  : O Teste t de Uma Amostra foi escolhido para esta situação específica porque ele permite avaliar se a média do consumo de largura de banda
# escolha      após o upgrade difere de um valor específico, neste caso, 54. A pergunta específica busca determinar se houve um aumento significativo no 
#              consumo médio de largura de banda após o upgrade em comparação a um limiar de 54.

# Hipóteses:

#   H0: O consumo médio de largura de banda no primeiro mês após o upgrade é igual ou menor que 54. Matematicamente, isso pode ser expresso como H0: μ ≤ 54.
#   H1: O consumo médio de largura de banda no primeiro mês após o upgrade é maior que 54. Em termos matemáticos, isso seria H1: μ > 54.

# - Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.
# - Se um valor-p relatado a partir de um teste t for menor que 0,05, esse resultado é considerado estatisticamente significativo.
#   Se um valor-p for maior que 0,05, o resultado é insignificante.

# -> Um valor-p de um teste t é a probabilidade de que os resultados de seus dados de amostra tenham ocorrido por acaso.




## Pergunta 3: