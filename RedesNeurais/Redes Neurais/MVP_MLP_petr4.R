library(quantmod)
library(neuralnet)
library(tidyverse)

getSymbols("PETR4.SA", src = "yahoo", from = "2007-01-01")
dados <- as.data.frame(get("PETR4.SA"))
fator_ajuste <- Ad(PETR4.SA) / Cl(PETR4.SA)
lista_series_alvo <- list(
  "Fechamento_Adj" = Ad(PETR4.SA),
  "Abertura_Adj"   = Op(PETR4.SA) * fator_ajuste,
  "Maximo_Adj"     = Hi(PETR4.SA) * fator_ajuste,
  "Minimo_Adj"     = Lo(PETR4.SA) * fator_ajuste
)

lags_fixo <- 5

lista_arquiteturas <- list(
  c(2),        # Minimalista
  c(3),        # Média Geométrica
  c(5),        # Regra dos 2/3
  c(8),        # Limite superior
  c(4,2),      # Deep Funil (Seu atual campeão)
  c(5,3),      # Deep Leve
  c(10,5)      # Deep Robusta
)
nomes_arq <- c("(2)","(3)","(5)","(8)","(4,2)", "(5,3)", "(10,5)")

encontrar_melhor_arq <- function(dados_xts, nome_serie) {
  
  message(paste("\n--- Otimizando:", nome_serie, "---"))
  
  dados <- as.numeric(dados_xts)
  maxs <- max(dados, na.rm=TRUE)
  mins <- min(dados, na.rm=TRUE)
  dados_norm <- as.numeric(scale(dados, center = mins, scale = maxs - mins))
  n <- length(dados_norm)
  
  base_temp <- data.frame(Alvo = dados_norm[(lags_fixo+1):n])
  for(i in 1:lags_fixo){
    base_temp[paste0("Lag_", i)] <- dados_norm[(lags_fixo+1-i):(n-i)]
  }
  
  tam_teste <- 30
  treino <- base_temp[1:(nrow(base_temp) - tam_teste), ]
  teste  <- base_temp[(nrow(base_temp) - tam_teste + 1):nrow(base_temp), ]
  
  f <- as.formula(paste("Alvo ~", paste(names(base_temp)[-1], collapse = " + ")))
  
  resultados_locais <- data.frame(
    Arquitetura = character(),
    RMSE = double(),
    MAPE = double(),
    stringsAsFactors = FALSE
  )
  
  for (a in 1:length(lista_arquiteturas)) {
    set.seed(123) 
    
    tryCatch({
      modelo <- neuralnet(f, data = treino, hidden = lista_arquiteturas[[a]], 
                          linear.output = TRUE, stepmax = 1e7, rep=1)
      
      pred_norm <- neuralnet::compute(modelo, teste[, -1])$net.result
      
      pred_reais <- pred_norm * (maxs - mins) + mins
      y_reais    <- teste$Alvo * (maxs - mins) + mins
      
      # Métricas
      rmse <- sqrt(mean((y_reais - pred_reais)^2))
      mape <- mean(abs((y_reais - pred_reais) / y_reais)) * 100
      
      resultados_locais <- rbind(resultados_locais, data.frame(
        Arquitetura = nomes_arq[a],
        RMSE = round(rmse, 4),
        MAPE = round(mape, 4)
      ))
      
    }, error = function(e) {
      message(paste("Erro na arq", nomes_arq[a], ":", e$message))
    })
  }
  
  # Ordena pelo melhor RMSE
  ranking <- resultados_locais[order(resultados_locais$RMSE), ]
  
  return(list(
    nome = nome_serie,
    ranking = ranking,
    vencedor = ranking[1, ]
  ))
}

relatorio_final <- list()
tabela_campeoes <- data.frame()

for(serie in names(lista_series_alvo)) {
  resultado <- encontrar_melhor_arq(lista_series_alvo[[serie]], serie)
  
  relatorio_final[[serie]] <- resultado$ranking
  
  tabela_campeoes <- rbind(tabela_campeoes, data.frame(
    Variavel = serie,
    Melhor_Arq = resultado$vencedor$Arquitetura,
    RMSE = resultado$vencedor$RMSE,
    MAPE = resultado$vencedor$MAPE
  ))
  
  print(paste("Vencedor para", serie, ":", resultado$vencedor$Arquitetura, 
              "- RMSE:", resultado$vencedor$RMSE))
}

print(tabela_campeoes)


string_para_vetor <- function(str_arq) {
  nums <- str_extract_all(str_arq, "\\d+")[[1]]
  return(as.numeric(nums))
}


df_previsoes_consolidadas <- data.frame()

# Loop para rodar cada variável com seu respectivo mvp
for(i in 1:nrow(tabela_campeoes)) {
  
  nome_var <- tabela_campeoes$Variavel[i]
  arq_str  <- tabela_campeoes$Melhor_Arq[i]
  arq_num  <- string_para_vetor(arq_str) # Converte "(4,2)" para c(4,2)
  
  message(paste("Treinando MVP para:", nome_var, "com Arq:", arq_str))
  
  dados_xts <- lista_series_alvo[[nome_var]]
  dados <- as.numeric(dados_xts)
  maxs <- max(dados, na.rm=TRUE)
  mins <- min(dados, na.rm=TRUE)
  dados_norm <- as.numeric(scale(dados, center = mins, scale = maxs - mins))
  n <- length(dados_norm)
  
  base_ml <- data.frame(Alvo = dados_norm[(lags_fixo+1):n])
  for(j in 1:lags_fixo){
    base_ml[paste0("Lag_", j)] <- dados_norm[(lags_fixo+1-j):(n-j)]
  }
  
  # Split
  tam_teste <- 30
  treino <- base_ml[1:(nrow(base_ml) - tam_teste), ]
  teste  <- base_ml[(nrow(base_ml) - tam_teste + 1):nrow(base_ml), ]
  f <- as.formula(paste("Alvo ~", paste(names(base_ml)[-1], collapse = " + ")))
  
  # Treina o MVP
  modelo <- neuralnet(f, data = treino, hidden = arq_num, linear.output = TRUE, stepmax = 1e7)
  
  # Prevê
  pred_norm <- neuralnet::compute(modelo, teste[, -1])$net.result
  
  # Desnormaliza
  pred_reais <- pred_norm * (maxs - mins) + mins
  y_reais    <- teste$Alvo * (maxs - mins) + mins
  datas_reais <- index(dados_xts)[(n - tam_teste + 1):n]
  # Salva
  df_temp <- data.frame(
    Dia =  datas_reais,
    Serie = nome_var,
    Real = as.numeric(y_reais),
    Previsto = as.numeric(pred_reais)
  )
  
  df_previsoes_consolidadas <- rbind(df_previsoes_consolidadas, df_temp)
}

df_previsoes_consolidadas <- df_previsoes_consolidadas %>%
  mutate(Erro_Absoluto = abs(Real - Previsto))
df_previsoes_consolidadas
rmsees <- df_previsoes_consolidadas %>%
  group_by(Serie) %>%
  summarise(RMSE = round(sqrt(mean(Erro_Absoluto^2)),4),
            MAPE = round(mean(Erro_Absoluto / Real) * 100,4))
print(rmsees)

plots <- df_previsoes_consolidadas %>%
  filter(Serie == "Fechamento_Adj") %>%
  gather(key = "Tipo", value = "Valor", Real, Previsto)
ggplot(plots, aes(x = Dia, y = Valor, color = Tipo)) +
  geom_line(size=1) +
  labs(title = "Previsão vs Real - Fechamento Ajustado PETR4
", x = "Data", y = "Preço (R$)") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Previsto" =
 "blue")) +
  theme(legend.title = element_blank())

#install.packages("writexl")
'library(writexl)
write_xlsx(df_previsoes_consolidadas, "Previsoes_MLP_PETR4.xlsx")
getwd()
'