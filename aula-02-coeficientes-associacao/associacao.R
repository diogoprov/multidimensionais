library(vegan)
library(ade4)
library(adespatial)   # dist.ldc()
library(cluster)      # daisy()

data(doubs, package = "ade4")
spe <- doubs$fish[-8, ]
env <- doubs$env[-8, ]

dim(spe)
range(spe)
table(unlist(spe))           # as abundâncias vão de 0 a 5 (classes)
sum(spe == 0) / prod(dim(spe))   # proporção de zeros
sort(colSums(spe > 0))           # em quantos locais cada espécie ocorre
rowSums(spe > 0)                 # riqueza de cada local

env.z <- decostand(env, "standardize")   # escore z: média 0, desvio-padrão 1
round(apply(env.z, 2, mean), 2)
apply(env.z, 2, sd)

env.de  <- dist(env.z)                        # euclidiana
env.man <- dist(env.z, method = "manhattan")  # Manhattan

spe.ch <- dist(decostand(spe, "normalize"))
range(spe.ch)

env3 <- env[, c("alt", "oxy", "bdo")]
d2 <- mahalanobis(env3, center = colMeans(env3), cov = cov(env3))
round(sqrt(d2), 2)   # distância de Mahalanobis de cada local ao centroide

spe.de <- dist(spe)        # euclidiana nas abundâncias brutas
spe.bc <- vegdist(spe)     # Bray-Curtis (o padrão do vegdist)

# quantas espécies cada par compartilha?
compart <- function(i, j) sum(spe[i, ] > 0 & spe[j, ] > 0)
compart("1", "23")
compart("16", "30")

as.matrix(spe.de)["1", "23"]; as.matrix(spe.de)["16", "30"]
as.matrix(spe.bc)["1", "23"]; as.matrix(spe.bc)["16", "30"]

spe.pa <- decostand(spe, "pa")

spe.jac <- vegdist(spe.pa, method = "jaccard", binary = TRUE)   # D = 1 - S
spe.sor <- vegdist(spe.pa, method = "bray", binary = TRUE)      # Sørensen
spe.sm  <- dist.binary(spe.pa, method = 2)   # Sokal & Michener: sqrt(1 - S)

spe.jac2 <- dist.binary(spe.pa, method = 1)   # Jaccard do ade4: sqrt(1 - S)
all.equal(as.vector(sqrt(spe.jac)), as.vector(spe.jac2))

is.euclid(spe.jac)
is.euclid(sqrt(spe.jac))

1 - as.matrix(spe.sm)["1", "23"]^2    # S de Sokal & Michener
1 - as.matrix(spe.jac)["1", "23"]     # S de Jaccard

spe.bc  <- vegdist(spe, method = "bray")                # Bray-Curtis (diferença percentual)
spe.hel <- dist(decostand(spe, "hellinger"))            # Hellinger
spe.chi <- dist.ldc(spe, method = "chisquare", silent = TRUE)   # qui-quadrado
spe.lch <- dist.ldc(spe, method = "log.chord", silent = TRUE)   # log-chord

is.euclid(spe.bc)
is.euclid(sqrt(spe.bc))
is.euclid(spe.hel)

env.mix <- env[, c("alt", "oxy", "bdo")]
env.mix$decliv <- cut(env$slo, breaks = quantile(env$slo), include.lowest = TRUE,
                      labels = c("baixa", "moderada", "alta", "muito alta"),
                      ordered_result = TRUE)
str(env.mix)

env.gow <- daisy(env.mix, metric = "gower")
summary(env.gow)

spe.tot <- decostand(spe, "total")       # perfil: proporção de cada espécie no local
spe.nrm <- decostand(spe, "normalize")   # chord
spe.hl  <- decostand(spe, "hellinger")   # raiz quadrada do perfil
spe.log <- log1p(spe)                    # log(x + 1)

rowSums(spe.tot)[1:5]          # cada local soma 1
rowSums(spe.nrm^2)[1:5]        # cada local tem comprimento 1

heatmap(as.matrix(spe.hel), symm = TRUE, scale = "none",
        col = hcl.colors(20, "YlGnBu", rev = TRUE), main = "Hellinger")
heatmap(as.matrix(spe.de), symm = TRUE, scale = "none",
        col = hcl.colors(20, "YlGnBu", rev = TRUE), main = "Euclidiana (bruta)")

dists <- list(euclidiana = spe.de, chord = spe.ch, hellinger = spe.hel,
              bray = spe.bc, jaccard = spe.jac)
round(cor(sapply(dists, as.vector), method = "spearman"), 2)

round(cor(env), 2)
pairs(env[, c("dfs", "alt", "flo", "oxy", "bdo")])

spe.t.chi <- dist.ldc(t(spe), method = "chisquare", silent = TRUE)
heatmap(as.matrix(spe.t.chi), symm = TRUE, scale = "none",
        col = hcl.colors(20, "YlGnBu", rev = TRUE), main = "Espécies (qui-quadrado)")
