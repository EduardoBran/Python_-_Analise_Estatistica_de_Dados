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


#### Novas Perguntas:

## 6) O uso do cartão melhorou significativamente em relação ao ano passado, que era 50?
#     (Neste caso usaremos: )

## 7) A última campanha foi bem-sucedida em termos de uso do cartão de crédito?
#     (Neste caso usaremos: )

## 8) Existe diferença de uso do cartão de crédito entre homens e mulheres?
#     (Neste caso usaremos: )

## 9) Existe diferença entre os segmentos de clientes em termos de uso do cartão de crédito?
#     (Neste caso usaremos: )

## 10) Existe uma relação entre o uso do cartão no último mês e o uso pré-campanha?
#      (Neste caso usaremos: )



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






###############################################     PERGUNTAS     ###############################################    






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

# Executar o Teste t de Uma Amostra
resultado_ttest <- t.test(df$consumo_medio_mes_anterior_ao_upgrade, mu = 50, alternative = "two.sided")
resultado_ttest

# Interpretação do valor-p
if(resultado_ttest$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n
      Portanto, o consumo médio de largura de banda antes do upgrade foi significativamente diferente de 50.\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).\n
      Portanto, não há evidência suficiente para afirmar que o consumo médio de largura de banda antes do upgrade foi diferente de 50.\n")
}

## Interpretando

# - Rejeitamos a hipótese nula (H0).
#   Portanto, o consumo médio de largura de banda antes do upgrade foi significativamente diferente de 50.






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

# Executar o Teste t de Uma Amostra
resultado_ttest <- t.test(df$consumo_medio_primeiro_mes_apos_upgrade, mu = 54, alternative = "two.sided")
resultado_ttest

# Interpretação do valor-p
# Interpretação do valor-p
if(resultado_ttest$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n",
      "Portanto, o consumo médio de largura de banda no primeiro mês após o upgrade foi significativamente maior que 54.\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).\n",
      "Portanto, não há evidência suficiente para afirmar que o consumo médio de largura de banda no primeiro mês após o upgrade foi maior que 54.\n")
}


## Interpretando

# - Falhamos em rejeitar a hipótese nula (H0). 
#   Portanto, não há evidência suficiente para afirmar que o consumo médio de largura de banda no primeiro mês após o upgrade foi maior que 54.






## Pergunta 3:

# - Houve diferença de consumo de largura de banda antes e depois do upgrade, considerando o primeiro mês após o upgrade?
#   Neste caso usaremos: Teste t de Duas Amostras (Pareado).

# Este teste é usado quando temos duas amostras que são relacionadas ou dependentes. Este é um teste para a hipótese nula de que duas amostras relacionadas 
# têm valores médios (esperados) idênticos.

# -> Quando usar: Este teste é utilizado quando as medições são feitas em pares correlacionados, ou seja, quando duas amostras estão relacionadas de alguma
#                 maneira (antes e depois, por exemplo).

# -> Por quê usar: O Teste t Pareado é útil para reduzir a variabilidade entre as amostras que poderia ser causada por variáveis não controladas, 
#                  focando na diferença dentro dos pares. Isso é especialmente útil em estudos longitudinais ou estudos que envolvem medições repetidas.

# -> Motivo da escolha: O Teste t Pareado foi escolhido porque é especificamente projetado para comparar duas amostras relacionadas, que neste caso são
#                       as medidas de consumo de largura de banda antes e depois do upgrade para os mesmos clientes. Esse teste é ideal para avaliar a 
#                       diferença significativa causada pelo upgrade, controlando as variáveis inerentes aos indivíduos que não mudam com o tempo.
#                       Ele fornece uma análise direta e eficaz da mudança induzida pelo upgrade ao focar nas diferenças dentro dos mesmos sujeitos,
#                       aumentando assim a precisão e a relevância dos resultados estatísticos para a pergunta de pesquisa.

# Hipóteses:
 
#  H0: O consumo antes do upgrade foi igual ao consumo após o upgrade (as médias são iguais).
#  H1: O consumo antes do upgrade foi diferente do consumo após o upgrade (as médias não são iguais).

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 4:

# - O gênero do cliente influenciou o consumo de largura de banda no primeiro mês após o upgrade?
#   Neste caso usaremos: Teste t de Duas Amostras Independentes.

# Calculamos o Teste t para as médias de duas amostras independentes. Este é um teste para a hipótese nula de que 2 amostras independentes têm valores
# médios (esperados) idênticos. Este teste assume que as populações têm variâncias idênticas por padrão.

# -> Quando usar: Este teste é aplicado quando queremos comparar as médias de duas amostras independentes de duas populações.

# -> Por quê usar: Ele é útil quando duas amostras são coletadas de forma independente uma da outra e queremos avaliar se existe uma diferença
#                  estatisticamente significativa entre suas médias.

# -> Motivo da escolha: O Teste t de Duas Amostras Independentes foi escolhido para analisar se há diferenças no consumo de largura de banda entre gêneros
#                       após um upgrade, porque permite comparar diretamente as médias de dois grupos independentes formados com base no gênero dos clientes. 
#                       Este teste é particularmente adequado aqui, pois cada grupo (masculino e feminino) é tratado como uma amostra separada, sem 
#                       dependência ou relação direta entre as observações dos dois grupos. Assim, o teste oferece um meio eficaz de verificar se as 
#                       diferenças observadas no consumo médio de largura de banda são estatisticamente significativas entre homens e mulheres, ajudando a
#                       determinar se o upgrade teve impactos diferenciados baseados no gênero.

# Hipóteses:

#  H0: Consumo de clientes do sexo masculino foi igual ao consumo de clientes do sexo feminino no primeiro mês após o upgrade
#      (consumo médio foi igual entre os gêneros).

#  H1: Consumo de clientes do sexo masculino não foi igual ao consumo de clientes do sexo feminino no primeiro mês após o upgrade
#      (consumo médio não foi igual entre os gêneros).

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 5:

# - Há alguma relação entre região e segmento do cliente?
#   Neste caso usaremos: Teste do Qui-Quadrado.

# Teste qui-quadrado de independência de variáveis é usado em uma tabela de contingência.
# A função calcula a estatística qui-quadrado e o valor-p para o teste de hipótese de independência das frequências observadas na tabela de contingência.
# As frequências esperadas são calculadas com base nas somas marginais sob o pressuposto de independência.

# -> Quando usar: O teste do Qui-Quadrado é usado para avaliar se há uma associação significativa entre duas variáveis categóricas.

# -> Por quê usar: É útil para determinar se as diferenças nas contagens ou frequências observadas em categorias são devidas ao acaso ou a uma associação
#                  real entre as variáveis.

# Motivo da escolha: O Teste do Qui-Quadrado foi escolhido porque é o método padrão para investigar a independência entre duas variáveis categóricas.
#                    Neste caso, as variáveis são 'região' e 'segmento' do cliente. Este teste é ideal para determinar se as distribuições dos segmentos 
#                    variam significativamente entre diferentes regiões, ou se elas são independentes uma da outra. Utilizando este teste, podemos validar 
#                    estatisticamente se há ou não uma associação entre a localização geográfica dos clientes e seus segmentos de mercado, o que é crucial
#                    para entender a dinâmica do mercado e para o planejamento estratégico de marketing e vendas.

# Hipóteses:
  
#   H0: Não há relacionamento entre região e segmento.
#   H1: Há relacionamento entre região e segmento.

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.





## Interpretando

# - 






## Pergunta 5:

# - Há alguma relação entre região e segmento do cliente?
#   Neste caso usaremos: Teste do Qui-Quadrado.

# Teste qui-quadrado de independência de variáveis é usado em uma tabela de contingência.
# A função calcula a estatística qui-quadrado e o valor-p para o teste de hipótese de independência das frequências observadas na tabela de contingência.
# As frequências esperadas são calculadas com base nas somas marginais sob o pressuposto de independência.

# -> Quando usar: O teste do Qui-Quadrado é usado para avaliar se há uma associação significativa entre duas variáveis categóricas.

# -> Por quê usar: É útil para determinar se as diferenças nas contagens ou frequências observadas em categorias são devidas ao acaso ou a uma associação
#                  real entre as variáveis.

# Motivo da escolha: O Teste do Qui-Quadrado foi escolhido porque é o método padrão para investigar a independência entre duas variáveis categóricas.
#                    Neste caso, as variáveis são 'região' e 'segmento' do cliente. Este teste é ideal para determinar se as distribuições dos segmentos 
#                    variam significativamente entre diferentes regiões, ou se elas são independentes uma da outra. Utilizando este teste, podemos validar 
#                    estatisticamente se há ou não uma associação entre a localização geográfica dos clientes e seus segmentos de mercado, o que é crucial
#                    para entender a dinâmica do mercado e para o planejamento estratégico de marketing e vendas.

# Hipóteses:

#   H0: Não há relacionamento entre região e segmento.
#   H1: Há relacionamento entre região e segmento.

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.





## Interpretando

# - 






## Pergunta 6:

# - O uso do cartão melhorou significativamente em relação ao ano passado, que era 50?
#   Neste caso usaremos: .


# -> Quando usar: 

# -> Por quê usar: 

# Motivo da escolha:

# Hipóteses:

#   H0: 
#   H1: 

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 7:

# - A última campanha foi bem-sucedida em termos de uso do cartão de crédito?
#   Neste caso usaremos: .


# -> Quando usar: 

# -> Por quê usar: 

# Motivo da escolha:

# Hipóteses:

#   H0: 
#   H1: 

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 8:

# - Existe diferença de uso do cartão de crédito entre homens e mulheres?
#   Neste caso usaremos: .


# -> Quando usar: 

# -> Por quê usar: 

# Motivo da escolha:

# Hipóteses:

#   H0: 
#   H1: 

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 9:

# - Existe diferença entre os segmentos de clientes em termos de uso do cartão de crédito?
#   Neste caso usaremos: .


# -> Quando usar: 

# -> Por quê usar: 

# Motivo da escolha:

# Hipóteses:

#   H0: 
#   H1: 

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 






## Pergunta 10:

# - Existe uma relação entre o uso do cartão no último mês e o uso pré-campanha?
#   Neste caso usaremos: .


# -> Quando usar: 

# -> Por quê usar: 

# Motivo da escolha:

# Hipóteses:

#   H0: 
#   H1: 

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.




## Interpretando

# - 




























# 1) Teste t de Uma Amostra
# Exemplo: Um pesquisador deseja verificar se a temperatura média em uma cidade é diferente de 25°C. Ele coleta a temperatura média diária durante um mês
#          e aplica um Teste t de Uma Amostra.

# Motivo da escolha: O Teste t de Uma Amostra é ideal aqui porque permite comparar a média de uma amostra (temperaturas médias diárias) com um valor
#                    específico (25°C). Este teste é usado para determinar se a média da amostra é estatisticamente diferente do valor conhecido ou
#                    hipotético.


# 2) Teste t de Duas Amostras (Pareado)
# Quando usar: Este teste é utilizado quando as medições são feitas em pares correlacionados, ou seja, quando duas amostras estão relacionadas de alguma maneira (antes e depois, por exemplo).
# 
# Por quê usar: O Teste t Pareado é útil para reduzir a variabilidade entre as amostras que poderia ser causada por variáveis não controladas, focando na diferença dentro dos pares. Isso é especialmente útil em estudos longitudinais ou estudos que envolvem medições repetidas.
# 
# Exemplo: Um exemplo clássico é medir o peso de um grupo de indivíduos antes e depois de um programa de dieta de um mês. O teste pode ajudar a determinar se houve uma mudança significativa no peso médio.
# 
# 3) Teste t de Duas Amostras Independentes
# Quando usar: Este teste é aplicado quando queremos comparar as médias de duas amostras independentes de duas populações.
# 
# Por quê usar: Ele é útil quando duas amostras são coletadas de forma independente uma da outra e queremos avaliar se existe uma diferença estatisticamente significativa entre suas médias.
# 
# Exemplo: Por exemplo, comparar o desempenho médio de alunos de duas escolas diferentes em um teste padrão ou a eficácia de dois diferentes tipos de medicamentos em dois grupos de pacientes distintos.
# 
# 4) Teste do Qui-Quadrado
# Quando usar: O teste do Qui-Quadrado é usado para avaliar se há uma associação significativa entre duas variáveis categóricas.
# 
# Por quê usar: É útil para determinar se as diferenças nas contagens ou frequências observadas em categorias são devidas ao acaso ou a uma associação real entre as variáveis.
# 
# Exemplo: Avaliar se o gênero (masculino, feminino) está associado à preferência por um novo produto. Outro exemplo seria testar se a escolha de curso universitário é independente do país de origem do estudante.
# 
# 5) Comparação de Proporção vs. Comparação de Média
# Necessidade de Distinção: Sim, é crucial saber se estamos comparando proporções ou médias:
#   
#   Comparação de proporções: Usada quando estamos interessados em comparar as proporções (percentagens) entre grupos. Testes típicos incluem o teste z para proporções e o teste do Qui-Quadrado.
# Comparação de médias: Usada quando queremos comparar médias numéricas entre grupos. Testes típicos incluem os testes t (uma amostra, duas amostras pareadas, duas amostras independentes).