library(vegan)
library(ade4)
library(pvclust)
library(labdsv)    # indval(); carregue-o depois do vegan

data(doubs, package = "ade4")
spe <- doubs$fish[-8, ]
env <- doubs$env[-8, ]

spe.norm <- decostand(spe, "normalize")
spe.ch <- dist(spe.norm)

spe.single   <- hclust(spe.ch, method = "single")
spe.complete <- hclust(spe.ch, method = "complete")
spe.upgma    <- hclust(spe.ch, method = "average")
spe.ward     <- hclust(spe.ch, method = "ward.D2")

par(mfrow = c(2, 2))
plot(spe.single,   main = "Ligação simples", hang = -1)
plot(spe.complete, main = "Ligação completa", hang = -1)
plot(spe.upgma,    main = "UPGMA", hang = -1)
plot(spe.ward,     main = "Ward", hang = -1)
par(mfrow = c(1, 1))

coph <- list(single = cophenetic(spe.single), complete = cophenetic(spe.complete),
             UPGMA = cophenetic(spe.upgma), Ward = cophenetic(spe.ward))
sapply(coph, \(x) cor(spe.ch, x))

plot(spe.ch, coph$UPGMA, asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, sqrt(2)),
     xlab = "Distância de corda", ylab = "Distância cofenética",
     main = paste("UPGMA, r cofenética =", round(cor(spe.ch, coph$UPGMA), 3)))
abline(0, 1)
lines(lowess(spe.ch, coph$UPGMA), col = "red")

sapply(coph, \(x) sum((spe.ch - x)^2))

k <- (nrow(spe) - 1):1   # número de grupos depois de cada fusão
plot(spe.ward$height, k, type = "S", xlab = "Nível de fusão",
     ylab = "Número de grupos", main = "Ward")
text(spe.ward$height, k, k, pos = 4, cex = 0.7, col = "red")

gr.ward <- cutree(spe.ward, k = 4)
table(gr.ward)
plot(spe.ward, hang = -1)
rect.hclust(spe.ward, k = 4)

set.seed(1)
spe.pv <- pvclust(t(spe.norm), method.hclust = "ward.D2",
                  method.dist = "euclidean", nboot = 1000)
plot(spe.pv, hang = -1)
pvrect(spe.pv, alpha = 0.95)

set.seed(1)
spe.km <- kmeans(spe.norm, centers = 4, nstart = 100)
spe.km$cluster
table(Ward = gr.ward, kmeans = spe.km$cluster)

set.seed(1)
spe.cascade <- cascadeKM(spe.norm, inf.gr = 2, sup.gr = 10, iter = 100,
                         criterion = "calinski")
plot(spe.cascade, sortg = TRUE)
spe.cascade$results

set.seed(1)
trecho <- kmeans(env$dfs, centers = 4, nstart = 100)$cluster
tapply(env$dfs / 10, trecho, range)   # km da nascente em cada trecho

set.seed(1)
iva <- indval(spe, trecho, numitr = 999)
sig <- iva$pval <= 0.05
tab.iv <- data.frame(grupo = iva$maxcls[sig], indval = round(iva$indcls[sig], 2),
                     p = iva$pval[sig], freq = colSums(spe > 0)[sig])
tab.iv[order(tab.iv$grupo, -tab.iv$indval), ]

lagoas <- read.table("ambiente.txt", header = TRUE, check.names = FALSE)
lagoas

girinos <- read.table("bocaina.txt", header = TRUE)
habitat <- substr(colnames(girinos), 1, 2)
table(habitat)
