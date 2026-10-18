library(vegan)
library(ape)      # pcoa()

data(doubs, package = "ade4")
spe <- doubs$fish[-8, ]
env <- doubs$env[-8, ]

env.pca <- rda(env, scale = TRUE)
env.pca
summary(env.pca)

ev <- eigenvals(env.pca)
ev[ev > mean(ev)]   # Kaiser-Guttman: autovalores acima da média

bs <- bstick(env.pca)
cbind(autovalor = ev, broken_stick = bs)
screeplot(env.pca, bstick = TRUE, type = "lines", main = "PCA do ambiente")

par(mfrow = c(1, 2))
biplot(env.pca, scaling = 1, main = "PCA, scaling 1")
biplot(env.pca, scaling = 2, main = "PCA, scaling 2")
par(mfrow = c(1, 1))

sit <- scores(env.pca, display = "sites", choices = 1:2, scaling = 1)
round(cor(env, sit), 2)

spe.hel <- decostand(spe, "hellinger")
spe.pca <- rda(spe.hel)
biplot(spe.pca, scaling = 1, main = "tb-PCA (Hellinger), scaling 1")

spe.ca <- cca(spe)
spe.ca
summary(spe.ca)

par(mfrow = c(1, 2))
plot(spe.ca, scaling = 1, main = "CA, scaling 1")
plot(spe.ca, scaling = 2, main = "CA, scaling 2")
par(mfrow = c(1, 1))

spe.dca <- decorana(spe)
spe.dca
diff(range(scores(spe.dca, display = "sites", choices = 1)))   # comprimento do DCA1
plot(spe.dca, display = "sites", type = "text")

spe.bray <- vegdist(spe)
spe.pcoa <- cmdscale(spe.bray, k = nrow(spe) - 1, eig = TRUE)
round(spe.pcoa$eig, 3)

# 1. raiz quadrada de Bray-Curtis, que é euclidiana
spe.pcoa.sq <- cmdscale(sqrt(spe.bray), k = nrow(spe) - 1, eig = TRUE)
min(spe.pcoa.sq$eig)   # praticamente zero: só erro numérico

# 2. correção de Lingoes e 3. correção de Cailliez
spe.pcoa.lin <- pcoa(spe.bray, correction = "lingoes")
spe.pcoa.cai <- pcoa(spe.bray, correction = "cailliez")
head(spe.pcoa.lin$values)

ev.pos <- spe.pcoa$eig[spe.pcoa$eig > 0]
bs.pcoa <- bstick(length(ev.pos), tot.var = sum(ev.pos))
which(ev.pos > bs.pcoa)   # eixos acima do broken stick
round(100 * ev.pos[1:2] / sum(ev.pos), 1)   # % da variação nos eixos 1 e 2

coord <- spe.pcoa$points[, 1:2]
plot(coord, type = "n", asp = 1, xlab = "PCoA 1", ylab = "PCoA 2",
     main = "PCoA (Bray-Curtis)")
abline(h = 0, v = 0, lty = 3)
text(coord, labels = rownames(spe))
spe.wa <- wascores(coord, spe)
text(spe.wa, labels = rownames(spe.wa), cex = 0.7, col = "red")

set.seed(1)
spe.nmds <- metaMDS(spe, distance = "bray", k = 2, trymax = 100, trace = FALSE)
spe.nmds
spe.nmds$stress

plot(spe.nmds, type = "t", main = paste("nMDS (Bray-Curtis), stress =",
                                        round(spe.nmds$stress, 3)))

stressplot(spe.nmds, main = "Diagrama de Shepard")

set.seed(1)
protest(spe.nmds, spe.pcoa$points[, 1:2], permutations = 999)
