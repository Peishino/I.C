# Pacotes que vamos utilizar
library(bnlearn)
library(tidyr)
library(dplyr)
library(Rgraphviz)
library(ggplot2)
# lendo os dados
health_activity_data <- read.csv("health_activity_data.csv")

str(health_activity_data)

# Esses dados trazem informações sobre a saúde e atividade física de 1000 pessoas.
# Para criarmos nossa rede bayesiana vamos categorizar algumas variáveis:

# contar a quantidade de cada idade

# Vamos categorizar a idade em 3 grupos: Jovem, Adulto e Idoso

health_activity_data$faixa_etaria <- cut(health_activity_data$Age,
                                          breaks = c(18, 30, 55, Inf),
                                          labels = c("Jovem", "Adulto", "Idoso"),
                                          right = FALSE)

# Vamos categorizar a atividade fisíca em 3 grupos: Sedentário, Moderado e Ativo

health_activity_data$atividade_fisica <- cut(health_activity_data$Exercise_Hours_per_Week,
                                              breaks = c(0, 1, 3, 6, 9, Inf),
                                              labels = c("Sedentário", "Levemente Ativo", "Moderado", "Ativo", "Muito Ativo"),
                                              right = FALSE )

# Vamos categorizar o IMC em 3 grupos: Abaixo do peso, Peso normal e Acima do peso
health_activity_data$imc <- cut(health_activity_data$BMI,
                                  breaks = c(18.5, 24.9, Inf),
                                  labels = c("Peso normal", "Acima do peso"),
                                  right = FALSE)
# Vamos categorizar o tempo de sono em 3 grupos: Abaixo do ideal, Ideal e Acima do ideal
health_activity_data$sono <- cut(health_activity_data$Hours_of_Sleep,
                                  breaks = c(0, 5, 6, 7, 8, 9, Inf),
                                  labels = c("Privação Severa", "Privação Leve", "Abaixo do Ideal", "Ideal", "Acima do Ideal", "Excessivo"),
                                  right = FALSE)
# Vamos categorizar o consumo de alcool apenas em 2 grupos: Sim e Não
health_activity_data$alcool <- cut(health_activity_data$Alcohol_Consumption,
                                   breaks = c(-1, 0, 2, 4, Inf),
                                   labels = c("Não bebe", "Ocasional (1-2x/semana)", 
                                              "Moderado (3-4x/semana)", 
                                              "Frequente (5+ vezes/semana)"),
                                   right = TRUE)

# Vamos categorizar o número de passos em 2 grupos: Abaixo do ideal e Ideal 
health_activity_data$passos <- cut(
  health_activity_data$Daily_Steps,
  breaks = c(0, 3000, 5000, 7500, 10000, Inf),
  labels = c("Sedentário", "Pouco Ativo", "Moderado", "Ativo", "Muito Ativo"),
  right = FALSE
)
# Vamos categorizar o ritmo cardíaco em 3 grupos: Abaixo do ideal, Ideal e Acima do ideal
health_activity_data$ritmo_cardiaco <- cut(health_activity_data$Heart_Rate,
                                             breaks = c(0, 60, 100, Inf),
                                             labels = c("Abaixo do ideal", "Ideal", "Acima do ideal"),
                                             right = FALSE)

# Vamos categorizar a pressão sanguínea, mas ela está no estilo xxx/xx precisamos fazer de uma forma diferente:
# Primerio separar em colunas
health_activity_data <- separate(health_activity_data, Blood_Pressure,
                                 into = c("Sistolic", "Diastolic"),
                                 sep = "/", convert = TRUE)

# Criar categorias baseadas na classificação da hipertensão
health_activity_data$pressao_cat <- with(health_activity_data, case_when(
  Sistolic <= 90 | Diastolic <= 60 ~ "Hipotensão",
  Sistolic < 120 & Diastolic < 80 ~ "Normal",
  Sistolic >= 120 & Sistolic < 130 & Diastolic < 80 ~ "Elevada",
  (Sistolic >= 130 & Sistolic < 140) | (Diastolic >= 80 & Diastolic < 90) ~ "Hipertensão estágio 1",
  Sistolic >= 140 | Diastolic >= 90 ~ "Hipertensão estágio 2",
  TRUE ~ "Indefinido"
))

# Traduzir o consumo de cigarro para sim e não
health_activity_data$fumante <- ifelse(health_activity_data$Smoker == "Yes", "Sim", "Não")
# Traduzir o genero para masculino e feminino
health_activity_data$genero <- ifelse(health_activity_data$Gender == "Male", "Masculino", "Feminino")
# Traduzir a diabete para sim e não
health_activity_data$diabete <- ifelse(health_activity_data$Diabetic == "Yes", "Sim_D", "Não_D")
# Traduzir doença cardiaca para sim e não
health_activity_data$doenca_cardiaca <- ifelse(health_activity_data$Heart_Disease == "Yes", "Sim", "Não")


dados_categoricos <- health_activity_data %>%
  select(faixa_etaria, atividade_fisica, imc, sono, alcool, passos, ritmo_cardiaco,
         pressao_cat, fumante, genero, diabete, doenca_cardiaca) %>%
  mutate(across(everything(), as.factor))
table(dados_categoricos$sono)
# estrutura da rede Hill-Climbing
modelo_bn <- hc(dados_categoricos, score = "bde", iss = 10)
modelstring(modelo_bn)
score(modelo_bn, dados_categoricos, type = "bic")
graphviz.plot(modelo_bn, shape = "ellipse", main = "Rede Bayesiana de Saúde")
arc.strength(modelo_bn, data = dados_categoricos, criterion = "mi")

# (Por algum motivo ele não reconheceu as dependências entre as variáveis)
# Vamos fazer manualmente a estrutura da rede

#Definindo a estrutura da rede manualmente com todas as relações especificadas
model_string <- paste0(
  "[faixa_etaria]",                          
  "[atividade_fisica]",                     
  "[alcool]",                               
  "[genero]",                               
  "[fumante]",                              
  "[sono|atividade_fisica:faixa_etaria]",   
  "[passos|fumante]",                       
  "[imc|faixa_etaria:atividade_fisica]",    
  "[ritmo_cardiaco|atividade_fisica:faixa_etaria:passos:pressao_cat]",  
  "[diabete|genero:imc]",                   
  "[pressao_cat|fumante:alcool:atividade_fisica]",  
  "[doenca_cardiaca|ritmo_cardiaco:fumante:pressao_cat:diabete]"
)

# Criando o objeto da rede bayesiana
modelo_manual <- model2network(model_string)
?ci.test
# Visualizando a estrutura
graphviz.plot(modelo_manual, shape = "ellipse", main = "Rede Bayesiana de Saúde")

# Testando as dependências que criei manualmente:

arc.strength(modelo_manual, data = dados_categoricos, criterion = "mi")
# Apenas as relações entre fumante e pressao_cat, alcool e pressao_cat foram significativas

score(modelo_manual, dados_categoricos, type = "bde", iss = 10)


# modelo hibrido
# vamos especificar uma whitelist e uma blacklist e ficar validando se o modelo faz sentido
whitelist <- data.frame(
  from = c("diabete","fumante","passos","alcool","atividade_fisica","atividade_fisica","ritmo_cardiaco","alcool","diabete","genero","atividade_fisica","pressao_cat","fumante","alcool","alcool"),
  to = c("doenca_cardiaca","pressao_cat","diabete","diabete","imc","pressao_cat","doenca_cardiaca","imc","pressao_cat","alcool","passos","ritmo_cardiaco","ritmo_cardiaco","atividade_fisica","passos")
)
blacklist <- data.frame(
  from = c("doenca_cardiaca","passos","fumante","atividade_fisica","pressao_cat","fumante","alcool","pressao_cat","fumante","diabete","doenca_cardiaca","diabete","faixa_etaria","fumante","fumante","passos","alcool","alcool","diabete","atividade_fisica","imc"),
  to = c("fumante","fumante","doenca_cardiaca","fumante","faixa_etaria","atividade_fisica","fumante","doenca_cardiaca","alcool","fumante","faixa_etaria","faixa_etaria","fumante","faixa_etaria","diabete","doenca_cardiaca","faixa_etaria","doenca_cardiaca","ritmo_cardiaco","doenca_cardiaca","faixa_etaria")
)
modelo_bn_hibrido <- hc(dados_categoricos, score = "bde",whitelist = whitelist,blacklist = blacklist, iss = 10)
graphviz.plot(modelo_bn_hibrido, shape = "ellipse", main = "Rede Bayesiana de Saúde")
# Vamos verificar as dependências entre as variáveis
arc.strength(modelo_bn_hibrido, data = dados_categoricos, criterion = "x2") # estudar mais sobre isso
score(modelo_escore, dados_categoricos, type = "bde")
modelstring(modelo_bn_hibrido)
score(modelo_bn_hibrido, dados_categoricos, type = "bic")
# "[faixa_etaria][sono][fumante][genero][alcool|genero][atividade_fisica|alcool][imc|atividade_fisica:alcool][passos|atividade_fisica:alcool][diabete|alcool:passos][pressao_cat|atividade_fisica:fumante:diabete][ritmo_cardiaco|pressao_cat:fumante][doenca_cardiaca|ritmo_cardiaco:diabete]"
# todas significativas!

# Ajustando o modelo com os dados
modelo_fit <- bn.fit(modelo_bn_hibrido, data = dados_categoricos, method = "bayes")

# Probabilidade global de doença cardíaca
prob_global <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = TRUE)
prob_global
# Probabilidade de doença cardíaca dado que a pessoa é fumante
prob_fumante <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = (fumante == "Sim"))
prob_fumante
# Probabilidade de doença cardíaca dado que a pessoa não é fumante
prob_nao_fumante <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = (fumante == "Não"))
prob_nao_fumante
# Probabilidade de doença cardíaca dado que a pessoa é diabética
prob_diabetico <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = (diabete == "Sim_D"))
prob_diabetico
# Probabilidade de doença cardíaca dado que a pessoa não é diabética
prob_nao_diabetico <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = (diabete == "Não_D"))
prob_nao_diabetico
# validaçãoc cruzada
cv <- bn.cv(modelo_bn_hibrido, data = dados_categoricos, method = "k-fold", k = 10)
cv

# vamos visualizar com o Rgraphviz
hlight <- list(nodes = nodes(modelo_bn_hibrido), arcs =  arcs(modelo_bn_hibrido), col = "grey", textCol = "grey")
pp <- graphviz.plot(modelo_bn_hibrido, highlight = hlight, render = F)

edgeRenderInfo(pp) <- list( col = "black", lwd = 3)
nodeRenderInfo(pp) <- list( col = "black", textCol = "black",fill = "grey")

renderGraph(pp)

# Tabelas de probabilidade condicional
# Vamos criar uma tabela de probabilidade condicional para a variável "doenca_cardiaca"
bn.fit.barchart(modelo_fit$doenca_cardiaca, main = "Probabilidade de doença cardiaca", xlab = "Pr(DC | RC, D)", ylab = "")

bn.fit.barchart(modelo_fit$ritmo_cardiaco, main = "Probabilidade de Ritmo Cardiaco", xlab = "Pr(RC | Pr)", ylab = "")
table(dados_categoricos$doenca_cardiaca)
bn.fit.barchart(modelo_fit$imc, main = "Probabilidade de IMC", xlab = "Pr(IMC | AF, A)", ylab = "")

bn.fit.barchart(modelo_fit$pressao_cat, main = "Probabilidade de Pressão Sanguínea", xlab = "Pr(PS | AF, F, D)", ylab = "")

# probabilidade para doença cardiaca para uma pessoa com hipotensão


prob_hipotensao <- cpquery(modelo_fit, event = (doenca_cardiaca == "Sim"), evidence = (pressao_cat == "Hipotensão"))
prob_hipotensao


# oq é o mi?
# oq blacklist e whitelist influenciam? calculo de score (bic bde)
# propensity score

# função bn.cv? (dps)
