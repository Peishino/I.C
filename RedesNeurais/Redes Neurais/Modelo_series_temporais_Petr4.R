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
fim <- Sys.Date() # Data de hoje

# dados do Yahoo Finance
getSymbols(ativo, src = "yahoo", from = inicio, to = fim, auto.assign = TRUE)
dados <- as.numeric(Ad(get(ativo)))
dados_petr <- as.numeric(Ad(get(ativo))) # Pegamos apenas a coluna "Adjusted" (Preço Fechamento Ajustado por dividendos/desdobramentos)
dados_petr <- dados_petr[1:(length(dados_petr)-5)]
petr_ts <- ts(dados_petr, start = 1, frequency = 1) 
ts.plot(petr_ts, main = "Cotação PETR4", ylab = "Preço")

acf(petr_ts, lag.max = 40, main="ACF Preço") 
pacf(petr_ts, lag.max = 40, main="PACF Preço")
adf.test(petr_ts) #  p valor > 0.05 não estacionário

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
modelo <- auto.arima(petr_ts,trace=TRUE) # c(1,1,1)
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
m2 <- Arima(petr_ts, order = c(0,1,1))
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
# m1 é melhor que m2

melhor_modelo <- m1 

accuracy(melhor_modelo)
'accuracy(melhor_modelo)
ME      RMSE       MAE         MPE     MAPE     MASE        ACF1
Training set 0.005044264 0.2450356 0.1514653 -0.00126123 1.907175 1.001329 -0.00532868'

previsao <- forecast(melhor_modelo, h = 5, level=c(95))

plot(previsao, lwd = 2, col="black", xlab= "Tempo", main="Previsão PETR4 (Log-Price)")

#zoom
plot(previsao, lwd = 2, col="black", xlab= "Tempo", main="Previsão PETR4", xlim=c(4700,4755),ylim=c(28,42))
lines(dados, col="black", lwd=2)#legenda
#colocar legenda
legend("topleft", legend=c("Dados Reais", "Previsão"),
       col=c("black", "blue"), lty=1, cex=0.8)

# comparar valores reais com a média do modelo
previsao$mean
dados[(length(dados)-4):length(dados)]

diferencas <- dados[(length(dados)-4):length(dados)] - previsao$mean
diferencas
resultado <- data.frame(
  Dia = 1:5,
  Previsao = round(previsao$mean, 2),
  Real = round(dados[(length(dados)-4):length(dados)], 2),
  Diferenca = round(diferencas, 2)
)
resultado
# o modelo está até bom para prever valores de fechamento, mas ainda não explica completamente
# e eu fiz só pra 5 dias, se eu colocar mais o modelo perde completamente a acurácia

