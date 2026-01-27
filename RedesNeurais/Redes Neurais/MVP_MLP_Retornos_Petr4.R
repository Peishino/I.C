pacotes <- c("quantmod", "neuralnet", "tidyverse", "stringr", "doParallel", "foreach")
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(novos_pacotes)) install.packages(novos_pacotes)

library(quantmod)
library(neuralnet)
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)

# ==============================================================================
# 2. CONFIGURAÇÃO DO AMBIENTE PARALELO
# ==============================================================================
# Detecta quantos núcleos seu PC tem e deixa 1 livre para o sistema operacional
num_cores <- detectCores() - 1 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

print(paste("--- MODO TURBO ATIVADO: Rodando em", num_cores, "núcleos ---"))

# ==============================================================================
# 3. AQUISIÇÃO E TRATAMENTO DOS DADOS (DESDE 2020)
# ==============================================================================
message("Baixando dados e calculando Log-Retornos...")

# DATA ALTERADA PARA 2020-01-01 (Foco no mercado recente/pós-pandemia)
getSymbols("PETR4.SA", src = "yahoo", from = "2020-01-01")

# Fator de ajuste
fator_ajuste <- Ad(PETR4.SA) / Cl(PETR4.SA)

# Séries de Preços Ajustados
precos_adj <- list(
  "Fechamento" = Ad(PETR4.SA),
  "Abertura"   = Op(PETR4.SA) * fator_ajuste,
  "Maximo"     = Hi(PETR4.SA) * fator_ajuste,
  "Minimo"     = Lo(PETR4.SA) * fator_ajuste
)

# Cálculo dos Log-Retornos para todas as séries
lista_retornos <- list()
for(nome in names(precos_adj)) {
  retorno <- diff(log(precos_adj[[nome]]))
  lista_retornos[[nome]] <- na.omit(retorno)
}

# Configurações do TCC
TAM_TESTE <- 30
LAGS_FIXO <- 5

lista_arquiteturas <- list(
  c(2), c(3), c(5), c(8), 
  c(4,2), c(5,3), c(10,5)
)
nomes_arq <- c("(2)","(3)","(5)","(8)","(4,2)", "(5,3)", "(10,5)")

# ==============================================================================
# 4. FUNÇÃO DE GRID SEARCH PARALELIZADA (CORRIGIDA)
# ==============================================================================
encontrar_melhor_arq_paralelo <- function(dados_xts, nome_serie) {
  
  message(paste("Otimizando:", nome_serie, "..."))
  
  # Preparação dos dados
  dados <- as.numeric(dados_xts)
  media <- mean(dados)
  desvio <- sd(dados)
  dados_norm <- as.numeric(scale(dados)) 
  n <- length(dados_norm)
  
  # Montagem da base
  base_temp <- data.frame(Alvo = dados_norm[(LAGS_FIXO+1):n])
  for(i in 1:LAGS_FIXO){
    base_temp[paste0("Lag_", i)] <- dados_norm[(LAGS_FIXO+1-i):(n-i)]
  }
  
  treino <- base_temp[1:(nrow(base_temp) - TAM_TESTE), ]
  teste  <- base_temp[(nrow(base_temp) - TAM_TESTE + 1):nrow(base_temp), ]
  f <- as.formula(paste("Alvo ~", paste(names(base_temp)[-1], collapse = " + ")))
  
  # --- CORREÇÃO AQUI: Adicionamos .export ---
  # Isso força o envio das variáveis globais para dentro dos workers
  ranking <- foreach(a = 1:length(lista_arquiteturas), 
                     .combine = rbind, 
                     .packages = 'neuralnet',
                     .export = c('nomes_arq', 'lista_arquiteturas', 'TAM_TESTE', 'LAGS_FIXO')) %dopar% {
                       
                       set.seed(123)
                       resultado_local <- data.frame(Arquitetura = nomes_arq[a], RMSE = NA, MAPE = NA)
                       
                       tryCatch({
                         # Treino rápido (threshold 0.1)
                         modelo <- neuralnet(f, data = treino, hidden = lista_arquiteturas[[a]], 
                                             linear.output = TRUE, stepmax = 1e6, threshold = 0.1, rep=1)
                         
                         pred_norm <- neuralnet::compute(modelo, teste[, -1])$net.result
                         
                         # Despadronizar para calcular erro no Retorno
                         pred_retorno <- pred_norm * desvio + media
                         y_retorno    <- teste$Alvo * desvio + media
                         
                         rmse <- sqrt(mean((y_retorno - pred_retorno)^2))
                         
                         resultado_local$RMSE <- rmse
                         resultado_local$MAPE <- mean(abs((y_retorno - pred_retorno) / y_retorno)) * 100
                         
                       }, error = function(e) { return(NULL) }) 
                       
                       resultado_local 
                     }
  
  # Ordena e pega o melhor
  ranking <- ranking[order(ranking$RMSE), ]
  return(list(vencedor = ranking[1, ]))
}

# ==============================================================================
# 5. EXECUTANDO O GRID SEARCH (SEQUENCIAL POR VARIÁVEL, PARALELO POR ARQ)
# ==============================================================================
tabela_campeoes <- data.frame()

start_time <- Sys.time()

for(serie in names(lista_retornos)) {
  resultado <- encontrar_melhor_arq_paralelo(lista_retornos[[serie]], serie)
  
  tabela_campeoes <- rbind(tabela_campeoes, data.frame(
    Variavel = serie,
    Melhor_Arq = resultado$vencedor$Arquitetura,
    RMSE_Retorno = resultado$vencedor$RMSE
  ))
  print(paste("-> Vencedor", serie, ":", resultado$vencedor$Arquitetura))
}

print("--- TABELA DE CAMPEÕES ---")
print(tabela_campeoes)

# ==============================================================================
# 6. PREVISÃO FINAL DE 30 DIAS (PARALELIZADO)
# ==============================================================================
message("\n--- Iniciando Treinamento Final e Reconstrução de Preços ---")

# Função auxiliar para converter string "(4,2)" em vetor numérico
string_para_vetor <- function(str_arq) {
  as.numeric(stringr::str_extract_all(str_arq, "\\d+")[[1]])
}

# Aqui usamos o foreach para rodar as 4 variáveis (Open, High, Low, Close) SIMULTANEAMENTE
df_previsoes_consolidadas <- foreach(i = 1:nrow(tabela_campeoes), .combine = rbind, .packages = c('neuralnet', 'quantmod', 'stringr')) %dopar% {
  
  nome_var <- tabela_campeoes$Variavel[i]
  arq_str  <- tabela_campeoes$Melhor_Arq[i]
  arq_num  <- string_para_vetor(arq_str)
  
  # Preparação (Repetida dentro do worker paralelo)
  dados_retorno_xts <- lista_retornos[[nome_var]]
  dados <- as.numeric(dados_retorno_xts)
  media <- mean(dados)
  desvio <- sd(dados)
  dados_norm <- as.numeric(scale(dados))
  n <- length(dados_norm)
  
  base_ml <- data.frame(Alvo = dados_norm[(LAGS_FIXO+1):n])
  for(j in 1:LAGS_FIXO){
    base_ml[paste0("Lag_", j)] <- dados_norm[(LAGS_FIXO+1-j):(n-j)]
  }
  
  treino <- base_ml[1:(nrow(base_ml) - TAM_TESTE), ]
  teste  <- base_ml[(nrow(base_ml) - TAM_TESTE + 1):nrow(base_ml), ]
  f <- as.formula(paste("Alvo ~", paste(names(base_ml)[-1], collapse = " + ")))
  
  # Treino do Modelo Final
  set.seed(123)
  modelo <- neuralnet(f, data = treino, hidden = arq_num, 
                      linear.output = TRUE, stepmax = 1e6, threshold = 0.1)
  
  # Previsão
  pred_norm <- neuralnet::compute(modelo, teste[, -1])$net.result
  
  # 1. Despadronizar para Log-Retorno
  pred_log_ret <- pred_norm * desvio + media
  
  # 2. Reconstrução do Preço
  dados_precos_originais <- as.numeric(precos_adj[[nome_var]])
  idx_precos_base <- (n - TAM_TESTE):(n - 1)
  precos_base <- dados_precos_originais[idx_precos_base]
  
  precos_previstos <- precos_base * exp(pred_log_ret)
  
  # Dados reais para comparação
  precos_reais_teste <- dados_precos_originais[(n - TAM_TESTE + 1):n]
  datas_teste <- index(precos_adj[[nome_var]])[(n - TAM_TESTE + 1):n]
  
  # Retorna o dataframe pronto
  data.frame(
    Dia = datas_teste,
    Serie = nome_var,
    Real = precos_reais_teste,
    Previsto = as.numeric(precos_previstos)
  )
}

# ==============================================================================
# 7. ENCERRAMENTO E ANÁLISE
# ==============================================================================
stopCluster(cl) # Importante: Libera os núcleos do processador
print(paste("Tempo Total:", round(difftime(Sys.time(), start_time, units="mins"), 2), "minutos"))

# Análise de Erro (EM REAIS)
df_previsoes_consolidadas <- df_previsoes_consolidadas %>%
  mutate(Erro_Absoluto = abs(Real - Previsto),
         Erro_Perc = abs(Real - Previsto)/Real)

metricas_finais <- df_previsoes_consolidadas %>%
  group_by(Serie) %>%
  summarise(
    RMSE_Reais = sqrt(mean((Real - Previsto)^2)),
    MAPE_Perc = mean(Erro_Perc) * 100
  )

print("--- RESULTADO FINAL (30 DIAS - PREÇOS RECONSTRUÍDOS) ---")
print(metricas_finais)

# Hit Ratio (Acerto de Direção) - Métrica de Ouro para TCC
df_direcao <- df_previsoes_consolidadas %>%
  group_by(Serie) %>%
  mutate(
    Dir_Real = sign(Real - lag(Real)),
    Dir_Prev = sign(Previsto - lag(Real)) # Previsto vs Real de ontem
  ) %>%
  na.omit() %>%
  summarise(
    Hit_Ratio = sum(Dir_Real == Dir_Prev) / n() * 100
  )

print("--- HIT RATIO (ACERTO DE TENDÊNCIA) ---")
print(df_direcao)

# Plot Exemplo (Fechamento)
df_plot <- df_previsoes_consolidadas %>% filter(Serie == "Fechamento")
plot(df_plot$Real, type="l", col="black", lwd=2, 
     main="Previsão 30 Dias: Fechamento (Reconstruído)", ylab="Preço (R$)")
lines(df_plot$Previsto, col="blue", lwd=2)
legend("topleft", legend=c("Real", "Previsto"), col=c("black", "blue"), lty=1)
