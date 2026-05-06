if(!require(quantmod)) install.packages("quantmod")
if(!require(tseries)) install.packages("tseries")
if(!require(randtests)) install.packages("randtests")
if(!require(GeneCycle)) install.packages("GeneCycle")
if(!require(forecast)) install.packages("forecast")
if(!require(lmtest)) install.packages("lmtest")

library(quantmod)
library(tseries)
library(randtests)
library(GeneCycle)
library(forecast)
library(lmtest)

ativo <- "PETR4.SA" 
inicio <- "2007-01-01"
fim <- "2026-05-06" 

# dados do Yahoo Finance
getSymbols(ativo, src = "yahoo", from = inicio, to = fim, auto.assign = TRUE)
dados <- as.numeric(Ad(get(ativo)))
dados_petr <- as.numeric(Ad(get(ativo))) # Pegamos apenas a coluna "Adjusted" (Preço Fechamento Ajustado por dividendos/desdobramentos)
dados_petr <- dados_petr[1:(length(dados_petr)-10)]
petr_ts <- ts(dados_petr, start = 1, frequency = 1) 
ts.plot(petr_ts, main = "Fechamento Ajustado PETR4", ylab = "Preço", line.width = 2)

library(ggplot2)

ggplot(data = data.frame(Tempo = 1:length(petr_ts), Preco = as.numeric(petr_ts)), aes(x = Tempo, y = Preco)) +
  geom_line(color = "#0f0f0f", size = 1) +
  labs(title = "", x = "", y = "") +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    axis.text = element_text(size = 30)
  )


acf(petr_ts, lag.max = 40, main="ACF Preço") # q = 0
pacf(petr_ts, lag.max = 40, main="PACF Preço") # p = 1
adf.test(petr_ts) #  p valor > 0.05 não estacionário, provavelmente d = 1

media <- vector()
desvio <- vector()
j <- 1
z <- 1

if(length(dados_petr) %% 2 != 0) {
  vet <- dados_petr[1:(length(dados_petr)-1)]
} else {
  vet <- dados_petr
}
for(i in 1:(length(vet)/2)){
  media[i] <- (vet[j]+ vet[j+1])/2
  j <- j + 2
  
  desvio[i] <- sd(c(vet[z], vet[z+1]))
  z <- z + 2
}
plot(media, desvio, main = "Média vs Desvio Padrão (Teste Visual)") # aleatório então é aditivo
# em dados financeiros é comum ver que os dados tem tendência e que são multiplicativos, mas nesse caso está como aditivo

cox.stuart.test(petr_ts) # tendência presente

diff_petr <- diff(petr_ts) # vamos fazer uma diff para tirar a tendência
ts.plot(diff_petr)

acf(diff_petr, lag.max = 40, main="ACF Retornos")
pacf(diff_petr, lag.max = 40, main="PACF Retornos")
adf.test(diff_petr) # p valor < 0.05, agora é estacionário
cox.stuart.test(diff_petr) # p valor > 0.05, sem tendência
fisher.g.test(diff_petr)# p valor > 0.05, não tem sazonalidade periódica forte

# Modelo ARIMA
modelo <- auto.arima(petr_ts,trace=TRUE)
modelo
?auto.arima

m1 <- auto.arima(petr_ts)
coeftest(m1) # todos os coeficientes são significativos (p-valor < 0.05)

# Análise de Resíduos M1
ts.plot(m1$residuals)
hist(m1$residuals, breaks=30, main="Histograma Resíduos M1") # Lembra a normal
acf(m1$residuals, lag.max = 40) 
pacf(m1$residuals, lag.max = 40)

# Teste Box-Pierce (Independência dos resíduos)
Box.test(m1$residuals, type = c("Box-Pierce")) # p-valor > 0.05, resíduos são independentes 

# Envelope (Cpgram)
cpgram(m1$residuals, main="Cpgram M1") # dentro da faixa azul, está ok

# segundo modelo
m2 <- Arima(petr_ts, order = c(1,1,0))
coeftest(m2) # nenhum dos coeficientes são significativos (p-valor > 0.05)

# Diagnóstico M2
ts.plot(m2$residuals)
Box.test(m2$residuals, type = c("Box-Pierce"))# p-valor > 0.05, resíduos são independentes 
cpgram(m2$residuals, main="Cpgram M2")

# Comparação dos Modelos
BIC(m1)
BIC(m2)
AIC(m1)
AIC(m2)
# m2 foi melhor que m1

melhor_modelo <- m2

accuracy(melhor_modelo)
'accuracy(melhor_modelo)
ME      RMSE       MAE         MPE     MAPE     MASE        ACF1
Training set 0.005044264 0.2450356 0.1514653 -0.00126123 1.907175 1.001329 -0.00532868'

previsao <- forecast(melhor_modelo, h = 10, level=c(95))

plot(previsao, lwd = 2, col="black", xlab= "Tempo", main="Previsão PETR4 (Log-Price)")

#zoom
plot(previsao, lwd = 2, col="black", xlab= "Tempo", main="Previsão PETR4", xlim=c(4780,4795),ylim=c(34,55))
lines(dados, col="black", lwd=2)#legenda
#colocar legenda
legend("topleft", legend=c("Dados Reais", "Previsão"),
       col=c("black", "blue"), lty=1, cex=0.8)


dados_ggplot <- dados %>%
  tail(30) %>%
  data.frame(preco = .)

ggplot(dados_ggplot, aes(x = 1:nrow(dados_ggplot), y = preco)) +
  geom_line(aes(color = "Real"),size = 1) +
  geom_line(data = data.frame(x = (20+1):30, y = previsao$mean), aes(x = x, y = y,color = "Previsão"), size = 1, linetype = "dashed")+
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Previsão" = "blue")) +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    axis.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.text = element_text(size = 30)
  )
# comparar valores reais com a média do modelo
previsao$mean
dados[(length(dados)-9):length(dados)]

diferencas <- dados[(length(dados)-4):length(dados)] - previsao$mean
diferencas
resultado <- data.frame(
  Dia = 1:10,
  Previsao = round(previsao$mean, 2),
  Real = round(dados[(length(dados)-4):length(dados)], 2),
  Diferenca = round(diferencas, 2)
)
resultado
# não é um bom resultado, o modelo não lida bem com a não linearidade (esperado)
# eu não consigo avaliar corretamente o modelo de forma consistente devido a previsão de muitos dias a frente

rsme <- sqrt(mean((resultado$Real - resultado$Previsao)^2))
rsme
mape <- mean(abs(resultado$Real - resultado$Previsao) / resultado$Real) * 100
mape


# uma outra forma de validar seria utilizar o rolling forecast ou sliding window
# onde consigo prever somente um dia a frente
# mas consigo atualizar o modelo com o último dia e prever o próximo dia, e assim por diante, até chegar no final da série

# Quantidade de dias que queremos prever no total
n_janela <- 10 
n_total <- length(dados)

# Vetores vazios para guardar os resultados do loop
previsoes_1_passo <- numeric(n_janela)
valores_reais <- numeric(n_janela)
limite_inf <- numeric(n_janela)
limite_sup <- numeric(n_janela)

# Loop da janela deslizante
for (i in 1:n_janela) {

  fim_treino <- n_total - n_janela + (i - 1)
  

  dados_treino <- ts(dados[1:fim_treino], start = 1, frequency = 1)
  modelo_atual <- Arima(dados_treino, order = c(1,1,0))
  prev <- forecast(modelo_atual, h = 1)
  previsoes_1_passo[i] <- as.numeric(prev$mean)
  limite_inf[i] <- as.numeric(prev$lower)
  limite_sup[i] <- as.numeric(prev$upper)
  valores_reais[i] <- dados[fim_treino + 1]
}

# Criando o DataFrame com todos os dados
resultado_sliding <- data.frame(
  Dia = 1:n_janela,
  Real = valores_reais,
  Previsao = previsoes_1_passo,
  Limite_Inf = limite_inf,
  Limite_Sup = limite_sup
)

print(round(resultado_sliding, 2))

ggplot(resultado_sliding, aes(x = Dia)) +
  geom_ribbon(aes(ymin = Limite_Inf, ymax = Limite_Sup, fill = "IC 95%"), alpha = 0.2) +
  geom_line(aes(y = Previsao, color = "Previsão"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Previsao, color = "Previsão"), size = 3) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  geom_point(aes(y = Real, color = "Real"), size = 3) +
  scale_color_manual(values = c("Real" = "#0f0f0f", "Previsão" = "blue")) +
  scale_fill_manual(values = c("IC 95%" = "blue")) +
  labs(title = "Validação Sliding Window: Real vs Previsão (h=1)",
       x = "Dias de Teste (Últimos 10 dias)", 
       y = "Preço (R$)",
       color = "Série:",
       fill = "Intervalo:") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )


# já está melhor, mas ainda é péssimo. O modelo não entende a alta volatilidade do mercado.
# ele simplesmente faz uma "linha reta", ou seja, prevê que o valor de amanhã seja praticamente o mesmo de hoje
# o que não é uma boa previsão para o mercado financeiro.