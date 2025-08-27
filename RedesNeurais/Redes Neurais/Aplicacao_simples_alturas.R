install.packages("neuralnet")
library(neuralnet)
library(readxl)

# utilizando um exemplo que fiz no terceiro semestre sobre previsão da altura das pessoas baseado na altura dos pais
# na época utilizei um modelo de regressão linear, agora vamos tentar aplicar o modelo de Redes neurais

dados <- read_xlsx(choose.files())
dados$`Sua altura`
dados <- dados[dados$`Idade da mãe` != "43 Anos",]
dados$`Idade da mãe` <- as.numeric(dados$`Idade da mãe`)
dados$`Idade do Pai` <- as.numeric(dados$`Idade do Pai`)
dados2 <- dados[,c(5,8,6)]
colnames(dados2) <- c("altura_filho", "altura_pai", "altura_mae")

#modelo de Redes neurais
modelo <- neuralnet(altura_filho ~ altura_pai + altura_mae, data = dados2, hidden = 3, linear.output = TRUE)

# grafico da rede
plot(modelo)

#testando o modelo
novos_dados <- data.frame(altura_pai = c(1.75, 1.80), altura_mae = c(1.65, 1.70))
pred <- compute(modelo, novos_dados)
print(pred$net.result)  


# adicionando mais variáveis

dados2 <- dados[,c(5,8,6,4,7,9)]
colnames(dados2) <- c("altura_filho", "altura_pai", "altura_mae","sexo","idade_mae","idade_pai")
dados2$sexo <- ifelse(dados2$sexo == "Feminino",0,1)
str(dados2)
modelo_genero <- neuralnet(altura_filho ~ altura_pai + altura_mae + sexo +idade_mae + idade_pai, data = dados2, hidden = 3, linear.output = T)

plot(modelo_genero)

novos_dados <- data.frame(
  altura_pai = c(1.74, 1.80,1.71),
  altura_mae = c(1.60, 1.69,1.55),
  sexo = c(0, 1,1),
  idade_mae = c(54,40,46),
  idade_pai = c(58,42,52)
)
pred <- compute(modelo_genero, novos_dados)
pred$net.result

# os resultados foram muito bons!!!
# mas uma coisa que tive dificuldade foi conseguir interpretar oq esses neuronios estão fazendo 