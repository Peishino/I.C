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


getSymbols("PETR4.SA", src = "yahoo", from = "2007-01-01", to = Sys.Date())
dados <- as.data.frame(get("PETR4.SA"))
# criação da padronização do ad() para os máximos e minimos
fator_ajuste <- Ad(PETR4.SA) / Cl(PETR4.SA)
High_Adj <- Hi(PETR4.SA) * fator_ajuste
Low_Adj  <- Lo(PETR4.SA) * fator_ajuste
tail(fator_ajuste,1)
tail(High_Adj,2)
dados_high <- High_Adj
dados_low  <- Low_Adj
dados_open <- Op(PETR4.SA) * fator_ajuste


dados_high <- ts(dados_high, frequency=1)
ts.plot(dados_high)


dados_low  <- ts(dados_low, frequency=1)
ts.plot(dados_low)

dados_open <- ts(dados_open, frequency = 1)
ts.plot(dados_open)

horizonte <- 5
n <- length(dados_high)
treino_high <- ts(dados_high[1:(n-horizonte)], frequency=1)
treino_low  <- ts(dados_low[1:(n-horizonte)], frequency=1)
treino_open <- ts(dados_open[1:(n-horizonte)], frequency=1)

# como já fizemos a modelagem pro Ad() vamos só aproveitar a estrutura para os máximos e minimos

modelo_high <- auto.arima(treino_high, seasonal = FALSE, trace = T) 
modelo_low <- auto.arima(treino_low, seasonal = FALSE, trace = T)  
modelo_open <- auto.arima(treino_open, seasonal = FALSE, trace = T)
accuracy(modelo_high)
accuracy(modelo_low)
accuracy(modelo_open)
prev_high <- forecast(modelo_high, h = horizonte,level=c(95))
prev_low  <- forecast(modelo_low, h = horizonte,level=c(95))
prev_open <- forecast(modelo_open, h = horizonte,level=c(95))

real_high <- as.numeric(dados_high[(n-horizonte+1):n])
real_low  <- as.numeric(dados_low[(n-horizonte+1):n])
real_open <- as.numeric(dados_open[(n-horizonte+1):n])

tabela_operacao <- data.frame(
  Dia =  rownames(dados)[(n-horizonte+1):n],
  Abertura_prev = round(prev_open$mean, 2),
  Compra_Alvo = round(prev_low$mean, 2),    
  Venda_Alvo = round(prev_high$mean, 2),    
  Spread_Lucro = round(prev_high$mean - prev_low$mean, 2),
  Abertura = round(real_open, 2),
  Real_Low = round(real_low, 2),
  Real_High = round(real_high, 2),
  dif_low = round(real_low - prev_low$mean, 2),
  dif_high = round(real_high - prev_high$mean, 2)
)

print(tabela_operacao)
# Modelo interessante, acurácia ok para abertura e fechamento apenas.
# Pode ser usado para ter uma noção de máximos e minimos do mercado, mas não acho que ainda funcione para fazer qualquer tipo de operação.
# Vamos partir pro deep learning!!!
