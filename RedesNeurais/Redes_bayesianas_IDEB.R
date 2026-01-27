library(readxl)
library(dplyr)
library(bnlearn)
library(ggplot2)
library(rbmn)

"
Comentários Patricia:
Remover variável : Docentes com superior
Grafico de dispersão dos pares de variáveis com IDEB pra ver correlações
Categorizar variáveis que não seguem distribuição normal e fazer boxplot do ideb por elas
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

par(mfrow = c(3, 2))
for (col in setdiff(names(df), "IDEB")) {
  plot(df[[col]], df$IDEB,
       xlab = col, ylab = "IDEB",
       main = paste("Dispersão:", col, "vs IDEB"),
       col = "steelblue", pch = 19)
}

# Correlação com ideb
correlacoes <- cor(df)
correlacoes
correlacoes[,"IDEB"]

cortest <- cor.test(df$IDEB, df$Med_Hr_Aula)
cortest


for (col in colnames(df)) {
  shapiro_test <- shapiro.test(df[[col]])
  p_value <- shapiro_test$p.value
  
  if (p_value > 0.05) {
    cat(paste("A variável", col, "segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  } else {
    cat(paste("A variável", col, "não segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  }
}

# kolmogorov-smirnov

for (col in colnames(df)) {
  ks_test <- ks.test(df[[col]], "pnorm", mean = mean(df[[col]]), sd = sd(df[[col]]))
  p_value <- ks_test$p.value
  
  if (p_value > 0.05) {
    cat(paste("A variável", col, "segue uma distribuição normal (p =", round(p_value, 4), ")\n"))
  } else {
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

colnames(df)

# Remoção da variável "Docentes com superior"
df <- df[,-5]

# Transformar variáveis não normais em categóricas
summary(df$Med_Hr_Aula)
par(mfrow = c(1, 1))
hist(df$Med_Hr_Aula)
df$Med_Hr_Aula_cat <- cut(df$Med_Hr_Aula,
                          breaks = c(4,5,10),
                          labels = c("Até 5", "Acima de 5"))
min(df$IDEB)
df$IDEB_cat <- cut(df$IDEB,
                   breaks = c(0,4.6,5.49,6.5), 
                   labels = c("Muito abaixo da meta", "Abaixo da meta", "Na meta"))
summary(df$Med_Alunos_Turma)
df$Med_alunos_cat <- cut(df$Med_Alunos_Turma,
                        breaks = c(0,25,40),
                        labels = c("Até 25", "Acima de 25"))
                        

df$Distorcao_Idade_cat <- cut(df$Distorcao_Idade,
                              breaks = c(0,10,100),
                             labels = c("Até 10%", "Acima de 10%"))

df$re

par(mfrow = c(1, 2))
?cut

library(Hmisc)

df$Med_Hr_Aula_cat2 <- cut2(df$Med_Hr_Aula, g = 2)
table(df$Med_Hr_Aula_cat2)
boxplot(df$IDEB ~ df$Med_Hr_Aula_cat2,
        col = "lightblue", border = "gray",
        main = "Boxplot IDEB por categorias de Med_Hr_Aula (cut2)",
        xlab = "Categorias de Med_Hr_Aula (cut2)", ylab = "IDEB")

df$Baixa_regularidade_Docente_cat <- cut2(df$Baixa_regularidade_Docente, g = 2)
table(df$Baixa_regularidade_Docente_cat)
boxplot(df$IDEB ~ df$Baixa_regularidade_Docente_cat,
        col = "lightblue", border = "gray",
        main = "Boxplot IDEB por categorias de Baixa_regularidade_Docente (cut2)",
        xlab = "Categorias de Baixa_regularidade_Docente (cut2)", ylab = "IDEB")

# Categorização de todas as variáveis






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

modelo <- bn.fit(dag_hc, data = df)
modelo


