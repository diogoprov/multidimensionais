# install.packages("remotes")
# remotes::install_github("paternogbc/ecodados")
library(ecodados)

amb <- bocaina.env[, names(bocaina.env) != "hydro"]   # só as quantitativas
dim(amb)
head(amb)

Z <- scale(amb)
round(colMeans(Z), 10)   # médias: 0
apply(Z, 2, sd)          # desvios-padrão: 1
n <- nrow(Z)
p <- ncol(Z)

R <- t(Z) %*% Z / (n - 1)    # crossprod(Z) / (n - 1) faz o mesmo
round(R, 2)
all.equal(R, cor(amb), check.attributes = FALSE)

ea <- eigen(R)
lambda <- ea$values    # autovalores, em ordem decrescente
U <- ea$vectors        # autovetores, um por coluna
round(lambda, 3)

# (a) a soma dos autovalores é o traço de R (soma da diagonal), que é p
sum(lambda)
sum(diag(R))

# (b) os autovetores têm comprimento 1 e são ortogonais entre si: U'U = I
round(t(U) %*% U, 10)

# (c) R pode ser reconstruída a partir deles: R = U Λ U'
all.equal(R, U %*% diag(lambda) %*% t(U), check.attributes = FALSE)

round(100 * lambda / sum(lambda), 1)

F <- Z %*% U
round(F[, 1:2], 2)

# a variância dos escores de cada eixo é o seu autovalor
round(apply(F, 2, var), 3)
round(lambda, 3)

round(cor(amb, F[, 1:2]), 2)
round(U[, 1:2] %*% diag(sqrt(lambda[1:2])), 2)

sv <- svd(Z)
round(sv$d^2 / (n - 1), 3)   # valores singulares ao quadrado / (n - 1)
round(lambda, 3)             # = autovalores de R
round(abs(sv$v) - abs(U), 10)   # W = U, a menos do sinal

sum(sv$d > 1e-10)

pca <- prcomp(amb, scale. = TRUE)
round(pca$sdev^2, 3)                       # autovalores
round(abs(pca$rotation) - abs(U), 10)      # autovetores (a menos do sinal)
