library(readxl)
library(dplyr)
library(bnlearn)
library(ggplot2)
library(rbmn)

"
Comentários Patricia:
Remover variável : Docentes com superior
Grafico de dispersão dos pares de variáveis com IDEB
Categorizar variáveis que não seguem distribuição normal
"


caminho <- choose.files()
dados <- read_xlsx(caminho)
dados


summary(dados)
str(dados)
dados$IDEB <- as.numeric(dados$IDEB)

# na
sum(is.na(dados$IDEB))
df <- na.omit(dados) 

# colunas continuas
df <- df[,6:12]

# Gráficos de dispersão das variávies em relação ao IDEB

par(mfrow = c(2, 3))
for (col in setdiff(names(df), "IDEB")) {
  plot(df[[col]], df$IDEB,
       xlab = col, ylab = "IDEB",
       main = paste("Dispersão:", col, "vs IDEB"),
       col = "steelblue", pch = 19)
}

# Correlação com ideb
correlacoes <- cor(df)
correlacoes[,"IDEB"]


nao_normais <- c()
for (col in colnames(df)) {
  shapiro_test <- shapiro.test(df[[col]])
  p_value <- shapiro_test$p.value
  
  if (p_value > 0.05) {
    cat(paste("A variável", col, "segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  } else {
    nao_normais <- c(col, nao_normais)
    cat(paste("A variável", col, "não segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  }
}

# histogramas

n <- length(colnames(df))
linhas <- ceiling(sqrt(n))
cols <- ceiling(n / linhas)

par(mfrow = c(linhas, cols))  

for (col in colnames(df)) {
  hist(df[[col]], main = paste("Histograma -", col), xlab = col, col = "lightblue", border = "white")
}

# A GBN requer que todas as variáveis sigam uma distribuição normal.

nao_normais
colnames(df)

# Log das variáveis que não seguem distribuição normal
for (col in nao_normais) {
  df[[col]] <- log(df[[col]] + 1)
}
# Verificar novamente a normalidade após a transformação
nao_normais2 <- c()
for (col in nao_normais) {
  shapiro_test <- shapiro.test(df[[col]])
  p_value <- shapiro_test$p.value
  
  if (p_value > 0.05) {
    cat(paste(col, "segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  } else {
    nao_normais2 <- c(col, nao_normais2)
    cat(paste(col, "não segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  }
}

# histogramas


par(mfrow = c(linhas, cols))  

for (col in colnames(df)) {
  hist(df[[col]], main = paste("Histograma -", col), xlab = col, col = "lightblue", border = "white")
}


# Montar dag

# hill climbing
dag_hc <- hc(df)

par(mfrow = c(1, 1))
graphviz.plot(dag_hc, layout = "dot")

# avaliar escores
score(dag_hc, df)

arcs(dag_hc)
arc.strength(dag_hc, df)

# dag2 - iamb

dag2 <- iamb(df, test = "cor")
graphviz.plot(dag2, layout = "dot")

#quero o layout de cima para baixo vem do graphviz


score(dag2, data = df)
arcs(dag2)

# BNFit

