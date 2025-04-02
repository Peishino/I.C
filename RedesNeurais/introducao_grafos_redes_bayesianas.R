# Exemplo 1 do livro Redes Neurais Bayesianas - Marco Scutari e Jean-Baptiste
# Pacote para criação de DAGs
install.packages("bnlearn")
library(bnlearn)

# Primeiro passo: criar o grafo sem nenhuma aresta

dag <- empty.graph(nodes = c("A","S","E","O","R","T"))
# As variáveis são A = age, S = sex, E = education, O = occupation, R = residence, T = transport

# Sabemos que as variáveis age e sex não são influenciadas por nenhuma outra variável
# portanto não terão arestas apontadas para elas
# por outro lado sabemos que a education pode ser influenciada pela age e pelo sex

dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")

# entendemos também que a educação influencia diretamente na sua ocupation e na residence

dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")

# e por fim o meio de transporte mais utilizado (ou preferido) é influenciado diretamente pela ocupation e pela residence

dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")

# agora podemos ler pelo modelo: dag

dag

# as dependencias diretas são listadas para cada variável denotadas por (|)
# sendo também representadas por intervalos caso tenha duas dependencias

# também é possível visualizar o grafo:
library(Rgraphviz)
graphviz.plot(dag)


# um método mais rápidos de adiconar as arestas:

dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set
dag2

# valores possíveis das variáveis

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

# probabilidades "independentes"
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3, dimnames = list(A = A.lv))
A.prob

S.prob <- array(c(0.60, 0.40), dim = 2, dimnames = list(S = S.lv))
S.prob

# probabilidades com 1 pai: duas dimensões
# são representadas por uma tabela/matriz de probabilidades a depender dos valores da variável pai

O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),
                dimnames = list(O = O.lv, E = E.lv))
O.prob

R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2),
                dimnames = list(R = R.lv, E = E.lv))
R.prob

# probabilidade com 2 pais: três dimensões
# são representadas por n tabelas/matrizes de probabilidades a depender dos valores da variavel pai1
# sendo n o número de variáveis do pai2

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 0.36, 0.70,
                  0.30, 0.90, 0.10), dim = c(2, 3, 2),
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))

E.prob

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58, 0.24, 0.18,
                  0.70, 0.21, 0.09), dim = c(3, 2, 2),
                dimnames = list(T = T.lv, O = O.lv, R = R.lv))

# Outro método para criação de grafos (seguindo o modelo)
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

# adicionamos a probabilidade que guardamos:
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob,
            T = T.prob)
bn <- custom.fit(dag, cpt)

# número de parametros
nparams(bn)

