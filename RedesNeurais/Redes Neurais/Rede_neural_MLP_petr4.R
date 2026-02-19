if(!require(quantmod)) install.packages("quantmod")
if(!require(neuralnet)) install.packages("neuralnet")
if(!require(tidyverse)) install.packages("tidyverse")

library(quantmod)
library(neuralnet)
library(tidyverse)

getSymbols("PETR4.SA", src = "yahoo", from = "2020-01-01")
dados <- Ad(PETR4.SA)
names(dados) <- "Preco"

#log diff preco
dados <- diff(log(dados$Preco))
dados <- na.omit(dados)

# normalização dos dados:
# para as redes neurais é obrigatório normalizar os dados

temp_scale <- scale(dados)
medias <- attr(temp_scale, "scaled:center")
desvios <- attr(temp_scale, "scaled:scale")

dados_norm <- as.numeric(temp_scale)

# em geral no mercado é utilizado 5 para semanas, 21 para mês e 252 para ano
lags <- 30
n <- length(dados_norm)

# diferentemente do arima, a rede neural não entende os dados em função do tempo
# então precisamos transformar os dados em uma matriz onde cada linha é um ponto no tempo e cada
# coluna é um "lag" (dia anterior)
# X1 = t-1, X2 = t-2, ...
base_ml <- data.frame(Alvo = dados_norm[(lags+1):n]) # pegar a partir da posição lags+1 pq n tenho dados dos dias 0-1, 0-2...
# o "Alvo" vai ser a variável explicada e as lags vão ser as variáveis explicativas
for(i in 1:lags){
  base_ml[paste0("Lag_", i)] <- dados_norm[(lags+1-i):(n-i)]
}

tam_teste <- 30
tam_treino <- nrow(base_ml) - tam_teste

treino <- base_ml[1:tam_treino, ]
teste  <- base_ml[(tam_treino+1):nrow(base_ml), ]

# para criação do modelo de rede neural, precisamos criar uma fórmula
# no formato Alvo ~ Lag_1 + Lag_2 + ... + Lag_n
formula_nn <- as.formula(paste("Alvo ~", paste(names(base_ml)[-1], collapse = " + ")))
set.seed(123)

# No código MVP_MLP_petr4.R há uma função para testar várias arquiteturas

modelo_nn <- neuralnet(formula_nn, 
                       data = treino,
                       act.fct = "tanh",
                       hidden = c(16,8),
                       stepmax = 1e7,
                       linear.output = TRUE,
                       threshold = 0.1,
                       lifesign = "full",
                       lifesign.step = 1000)
?neuralnet
# hidden = c(4,2) significa que a rede tem 2 camadas ocultas, a primeira com 4 neurônios e a segunda com 2 neurônios
#plot(modelo_nn)
previsoes_nn <- neuralnet::compute(modelo_nn, teste[, -1]) # tirar a coluna alvo

#plotar previsões vs real
ggplot() +
  geom_line(data = teste, aes(x = 1:nrow(teste), y = Alvo), color = "blue", size = 1) +
  geom_line(data = as.data.frame(previsoes_nn$net.result), aes(x = 1:nrow(teste), y = V1), color = "red", size = 1) +
  labs(title = "Previsões vs Real", x = "Tempo", y = "Valor Normalizado") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "blue", "Previsto" = "red")) +
  theme(legend.title = element_blank())


previsoes_nn_valores <- previsoes_nn$net.result * (maxs - mins) + mins
real_valores <- teste$Alvo * (maxs - mins) + mins
resultados <- data.frame(
  Real = as.numeric(real_valores),
  Previsto = as.numeric(previsoes_nn_valores)
)
print(resultados)