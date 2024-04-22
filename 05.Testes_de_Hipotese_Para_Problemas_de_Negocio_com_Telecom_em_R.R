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

## 6) Houve melhoria significativa no consumo médio do segundo mês após o upgrade em comparação ao consumo medio mensal antes do upgrade?
#     (Neste caso usaremos: Teste t de Duas Amostras (Pareado))

## 7) Existe diferença de uso do cartão de crédito entre homens e mulheres?
#     (Neste caso usaremos: Teste t de Duas Amostras Independentes)

## 8) Existe diferença entre os segmentos de clientes em termos de uso do cartão de crédito?
#     (Neste caso usaremos: )

## 9) Existe uma relação entre o uso do cartão no último mês e o uso pré-campanha?
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

# Visualizando Interpretação do valor-p
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

# Visualizando Interpretação do valor-p
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

names(df)

# Executando o Teste t Pareado
resultado <- t.test(df$consumo_medio_mensal_antes_upgrade, df$consumo_medio_primeiro_mes_apos_upgrade, paired = TRUE)
resultado

# Visualizando Interpretação do valor-p
if(resultado$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).")
}


## Interpretando

# - Como o valor-p 0.3868 é maior que 0,05, falhamos em rejeitar a hipótese nula.
#   Logo, o consumo médio no primeiro mês após o upgrade foi similar ao consumo médio antes do upgrade.






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

names(df)
table(df$genero)

# Separando as amostras em grupos
consumo_cliente_masculino <- df$consumo_medio_primeiro_mes_apos_upgrade[df$genero == 0]
consumo_cliente_feminino <- df$consumo_medio_primeiro_mes_apos_upgrade[df$genero == 1]

# Visualizando os primeiros resultados de cada grupo
print(head(consumo_cliente_masculino))
print(head(consumo_cliente_feminino))

# Visualizando a média de cada grupo
mean(consumo_cliente_masculino)
mean(consumo_cliente_feminino)

# Visualizando a variância de cada grupo
var(consumo_cliente_masculino)
var(consumo_cliente_feminino)

# Executando o Teste t de Duas Amostras Independentes (Teste de Welch)
resultado <- t.test(consumo_cliente_masculino, consumo_cliente_feminino, var.equal = FALSE)
print(resultado)

# Interpretando o valor de p
if (resultado$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n")
  cat("Portanto, houve diferença entre o consumo masculino e o consumo feminino de largura de banda no primeiro mês após o upgrade.\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).\n")
}


## Interpretando

# - Como o valor-p 0.0003409 é menor que 0,05, rejeitamos a H0.
#   Assim, concluímos que houve diferença entre o consumo masculino e o consumo feminino de largura de banda no primeiro mês após o upgrade.






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

str(df)

# Verifica tabela de contigência
table(df$segmento, df$regiao)

# Executa o teste
resultado = chisq.test(table(df$segmento, df$regiao))

# Interpretando o valor de p
if (resultado$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).\n")
}


## Interpretando

# - Interpretando o valor de p (tabela sem totais)
#   Como o valor-p 0.002307 menor que 0.05, rejeitamos a H0 e podemos dizer que há relação entre região e segmento do cliente.






## Pergunta 6:

# - Houve melhoria significativa no consumo médio do segundo mês após o upgrade em comparação ao consumo medio mensal antes do upgrade?
#   Neste caso usaremos: .


# -> Por quê usar: É apropriado para comparar medidas de consumo em dois momentos diferentes (antes e após o upgrade) nos mesmos indivíduos ou unidades.

# Motivo da escolha: Esta análise pode ajudar a determinar se os efeitos do upgrade foram sustentados ao longo do tempo, ao invés de apenas uma melhoria 
#                    imediata no primeiro mês.

# Hipóteses:

#  H0: O consumo antes do upgrade foi igual ao consumo após o segundo mês após o upgrade (as médias são iguais).
#  H1: O consumo antes do upgrade foi diferente do consumo após o segundo mês após o upgrade (as médias não são iguais).

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.

names(df)

# Executando o Teste t Pareado
resultado <- t.test(df$consumo_medio_mensal_antes_upgrade, df$consumo_medio_segundo_mes_apos_upgrade, paired = TRUE)
resultado

# Visualizando Interpretação do valor-p
if(resultado$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula (H0).\n")
} else {
  cat("Falhamos em rejeitar a hipótese nula (H0).")
}


## Interpretando

# - Como o valor-p 4.296e-16 é menor que 0,05, rejeitamos a hipótese nula.
#   Logo, o consumo médio no segundo mês após o upgrade foi diferente do consumo médio antes do upgrade.






## Pergunta 7:

# - Existe diferença de uso do cartão de crédito entre homens e mulheres?
#   Neste caso usaremos: Teste t de Duas Amostras Independentes.


# -> Quando usar: Este teste é apropriado quando queremos comparar as médias de duas amostras independentes.

# -> Por quê usar: O teste é útil para avaliar se existe uma diferença estatisticamente significativa entre os dois grupos que não estão relacionados entre
#                  si, como é o caso de grupos separados por gênero.

# Motivo da escolha: Este teste foi escolhido porque permite uma comparação direta entre os dois grupos independentes - homens e mulheres - em termos de uso
#                    do cartão de crédito medido pelo consumo médio. Ele é ideal para identificar diferenças baseadas em características demográficas nos
#                    dados.

# Hipóteses:

#   H0: Não existe diferença no consumo médio do cartão de crédito entre homens e mulheres (as médias de consumo são iguais entre os gêneros).
#   H1: Existe uma diferença no consumo médio do cartão de crédito entre homens e mulheres (as médias de consumo não são iguais entre os gêneros).

# Se o valor-p for menor que 0,05 rejeitamos a H0. Caso contrário, falhamos em rejeitar a H0.

names(df)


## Verificando Diferença No Consumo Médio Mensal Antes do Upgrade

# Separando as amostras em grupos
consumo_cliente_masculino_au <- df$consumo_medio_mensal_antes_upgrade[df$genero == 0]
consumo_cliente_feminino_au <- df$consumo_medio_mensal_antes_upgrade[df$genero == 1]

# Visualizando a média de cada grupo
mean(consumo_cliente_masculino_au)
mean(consumo_cliente_feminino_au)

# Executando o Teste t de Duas Amostras Independentes (Teste de Welch)
resultado <- t.test(consumo_cliente_masculino_au, consumo_cliente_feminino_au, var.equal = FALSE)
print(resultado)


## Interpretando

# - Como o valor-p 0.4572 é maior que 0.05, falhamos em rejeitar a H0 e podemos dizer que estatisticamente não houve diferença entre o consumo masculino
#   e o consumo feminino mensal antes do upgrade.


## Verificando Diferença No Consumo Médio do Segundo Mês Após o Upgrade

# Separando as amostras em grupos
consumo_cliente_masculino_du <- df$consumo_medio_segundo_mes_apos_upgrade[df$genero == 0]
consumo_cliente_feminino_du <- df$consumo_medio_segundo_mes_apos_upgrade[df$genero == 1]

# Visualizando a média de cada grupo
mean(consumo_cliente_masculino_du)
mean(consumo_cliente_feminino_du)

# Executando o Teste t de Duas Amostras Independentes (Teste de Welch)
resultado <- t.test(consumo_cliente_masculino_du, consumo_cliente_feminino_du, var.equal = FALSE)
print(resultado)



rm(consumo_cliente_masculino_au, consumo_cliente_feminino_au, consumo_cliente_masculino_du, consumo_cliente_feminino_du)



## Interpretando

# - Como o valor-p 0.0003409 é menor que 0,05, rejeitamos a H0. Assim, concluímos que estatisticamente houve diferença entre o consumo masculino e o
#   consumo feminino mensal depois do segundo mês do upgrade


## CONCLUSÃO

# - Com base nos resultados dos testes, podemos concluir que estatisticamente não houve diferença significativa no uso do cartão de crédito entre homens e
#   mulheres antes do upgrade, mas uma diferença significativa foi observada após o upgrade no segundo mês, indicando um possível impacto diferenciado do
#   upgrade entre os gêneros.






## Pergunta 8:

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






## Pergunta 9:

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





















### 1. Teste t de Uma Amostra

## Quando usar:
# - Este teste é utilizado quando queremos comparar a média de uma amostra com um valor conhecido ou hipotético.

## Exemplo:
# - Um fabricante de bolas de tênis afirma que suas bolas têm uma pressão interna de 2 psi. Para verificar essa afirmação, um técnico mede a pressão de
#   uma amostra de 30 bolas. O Teste t de Uma Amostra pode ser usado para determinar se a média da pressão nas bolas da amostra é significativamente
#   diferente de 2 psi.

### 2. Teste t de Duas Amostras (Pareado)

## Quando usar:
# - Usado quando as medições são feitas em pares correlacionados, geralmente em situações de "antes e depois" ou onde os sujeitos são os mesmos sob duas
#   condições diferentes.

## Exemplo:
# - Um grupo de pacientes recebe um medicamento para reduzir a pressão arterial e suas pressões são medidas antes e depois da medicação.
#   O Teste t Pareado pode ser usado para determinar se houve uma mudança significativa na pressão arterial média antes e depois da administração do
#   medicamento.

### 3. Teste t de Duas Amostras Independentes

## Quando usar:
# - Este teste é apropriado quando duas amostras são independentes uma da outra e queremos comparar suas médias.

## Exemplo:
# - Dois grupos de estudantes, um usando um novo método de ensino e outro usando o método tradicional, são testados para desempenho acadêmico.
#   O Teste t de Duas Amostras Independentes pode ser usado para ver se há uma diferença significativa nas pontuações médias entre os dois grupos.

### 4. Teste do Qui-Quadrado

## Quando usar: 
# - Este teste é usado para determinar se há uma associação significativa entre duas variáveis categóricas.

## Exemplo:
# - Um pesquisador quer saber se há uma relação entre gênero (masculino e feminino) e preferência por três diferentes marcas de refrigerante.
#   A análise Qui-Quadrado pode ser usada para ver se a preferência por marca é independente do gênero dos consumidores.

### 5. Teste t de Welch

## Quando usar:
# - Similar ao teste t de duas amostras, mas não assume igualdade de variâncias entre as duas amostras. É útil quando as amostras podem ter variâncias
#   diferentes.

## Exemplo:
# - Dois grupos de estudantes, um de uma escola rural e outro de uma escola urbana, fazem um teste padronizado. Devido à possibilidade de variâncias
#   diferentes nas pontuações devido a contextos educacionais distintos, o Teste t de Welch pode ser usado para comparar as médias das pontuações de ambos
#   os grupos.

### 6. Teste ANOVA (Análise de Variância)

## Quando usar:
# - O teste ANOVA é usado para comparar as médias de três ou mais grupos independentes para determinar se pelo menos um dos grupos difere significativamente
#   dos outros. É especialmente útil para testar a igualdade das médias em um design onde existem múltiplas categorias ou níveis de uma variável independente.

## Exemplo:
# - Suponha que um nutricionista queira avaliar a eficácia de três diferentes dietas na perda de peso. Um grupo de participantes é dividido aleatoriamente em 
#   três grupos, cada um seguindo uma das dietas. Depois de três meses, o peso perdido por cada participante é registrado. O teste ANOVA pode ser usado para
#   analisar se há diferenças significativas na média de peso perdido entre os três grupos de dietas. Se o teste ANOVA mostrar uma diferença estatisticamente
#   significativa, testes post-hoc (como o teste de Tukey) podem ser realizados para determinar quais grupos específicos diferem entre si.


