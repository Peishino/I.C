library(bnlearn)
library(brms)
library(readxl)
library(tidyverse)
dados <- read_xlsx(choose.files())
dados$`Sua altura`
dados <- dados[dados$`Idade da mãe` != "43 Anos",]
dados$`Idade da mãe` <- as.numeric(dados$`Idade da mãe`)
dados$`Idade do Pai` <- as.numeric(dados$`Idade do Pai`)
dados2 <- dados[,c(4,5,8,6)]
colnames(dados2) <- c("genero","altura_filho", "altura_pai", "altura_mae")
dados2$genero <- as.factor(dados2$genero)
dados2$genero <- as.numeric(dados2$genero)

estrutura_string <- "[altura_pai][altura_mae][genero][altura_filho|altura_pai:altura_mae:genero]"

rede <- model2network(estrutura_string)

plot(rede)

str(dados2)

modelo_bn <- bn.fit(rede, dados2)

pred <- predict(modelo_bn, node = "altura_filho", data = dados2)

modelo_bn$altura_filho

