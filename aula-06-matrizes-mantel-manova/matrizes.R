# install.packages("remotes")
# remotes::install_github("paternogbc/ecodados")
library(vegan)
library(ecodados)
library(adespatial)   # msr()
library(spdep)        # mst.nb(), nb2listw()

colnames(bocaina)
rownames(bocaina.env)
rownames(bocaina.xy)

spe <- t(bocaina)                       # locais nas linhas
env <- bocaina.env[rownames(spe), ]     # mesma ordem
xy  <- bocaina.xy[rownames(spe), ]
stopifnot(identical(rownames(spe), rownames(env)),
          identical(rownames(spe), rownames(xy)))

# composição: Bray-Curtis sobre os dados transformados por Hellinger
d.spe <- vegdist(decostand(spe, "hellinger"), "bray")

# ambiente: euclidiana das variáveis padronizadas (sem o hidroperíodo, categórico)
d.env <- dist(decostand(env[, names(env) != "hydro"], "standardize"))

# geografia: distância sobre a superfície da Terra (fórmula de haversine), em km
dist_km <- function(xy, raio = 6371) {
  lon <- xy[, 1] * pi / 180
  lat <- xy[, 2] * pi / 180
  n <- nrow(xy)
  d <- matrix(0, n, n, dimnames = list(rownames(xy), rownames(xy)))
  for (i in 1:n) for (j in 1:n) {
    a <- sin((lat[j] - lat[i]) / 2)^2 +
      cos(lat[i]) * cos(lat[j]) * sin((lon[j] - lon[i]) / 2)^2
    d[i, j] <- 2 * raio * asin(sqrt(a))
  }
  as.dist(d)
}
d.geo <- dist_km(xy)   # colunas: longitude, latitude
range(d.geo)

set.seed(1)
mantel(d.spe, d.env, permutations = 999)   # composição × ambiente
set.seed(1)
mantel(d.spe, d.geo, permutations = 999)   # composição × geografia
set.seed(1)
mantel(d.env, d.geo, permutations = 999)   # ambiente × geografia

plot(d.env, d.spe, xlab = "Distância ambiental",
     ylab = "Dissimilaridade (Bray-Curtis)", pch = 19, col = "grey40")

set.seed(1)
mantel.partial(d.spe, d.env, d.geo, permutations = 999)

viz <- mst.nb(d.geo)
plot(viz, as.matrix(xy))
lw <- nb2listw(viz)

set.seed(1)
(m.comum <- ade4::mantel.randtest(sqrt(d.spe), d.env, nrepet = 999))
set.seed(1)
(m.esp <- msr(m.comum, lw, method = "pair", nrepet = 999))

mrm <- function(Dy, ..., nperm = 999, semente = 1) {
  X <- sapply(list(...), as.vector)
  y <- as.vector(Dy)
  fit <- lm(y ~ X)
  r2 <- summary(fit)$r.squared
  n <- attr(Dy, "Size"); M <- as.matrix(Dy)
  set.seed(semente)
  r2p <- replicate(nperm, {
    o <- sample(n)
    summary(lm(as.vector(as.dist(M[o, o])) ~ X))$r.squared
  })
  list(coef = coef(fit)[-1], r2 = r2, p = (sum(r2p >= r2) + 1) / (nperm + 1))
}

mrm(d.spe, d.env, d.geo)

pcoa.peixes <- cmdscale(vegdist(fish_comm, "bray"))
pcoa.macro  <- cmdscale(vegdist(macroinv, "bray"))

set.seed(1)
(conc <- protest(pcoa.macro, pcoa.peixes, permutations = 999))
plot(conc, kind = 1)   # setas: de uma configuração à outra
plot(conc, kind = 2)   # resíduo de cada riacho

sp.anu <- anuros_permanova[, names(anuros_permanova) != "GRUPOS"]
fonte <- factor(anuros_permanova$GRUPOS)
table(fonte)

set.seed(1)
adonis2(sp.anu ~ fonte, method = "bray", permutations = 999)                  # abundância
set.seed(1)
adonis2(sp.anu ~ fonte, method = "bray", binary = TRUE, permutations = 999)   # incidência (Sørensen)

choose(9, 3)

round(cor(env_birds[, sapply(env_birds, is.numeric)]), 2)

aves.env <- env_birds[, c("mini.jan", "rain.tot", "altitude")]
aves.env$altitude <- factor(aves.env$altitude)
d.aves <- vegdist(decostand(com_birds, "hellinger"), "bray")

set.seed(1)
adonis2(d.aves ~ mini.jan + rain.tot + altitude, data = aves.env,
        permutations = 999)
set.seed(1)
adonis2(d.aves ~ mini.jan + rain.tot + altitude, data = aves.env,
        by = "margin", permutations = 999)

bd.anu <- betadisper(vegdist(sp.anu), fonte)
set.seed(1)
permutest(bd.anu, permutations = 999)
boxplot(bd.anu)

bd.aves <- betadisper(d.aves, aves.env$altitude)
set.seed(1)
permutest(bd.aves, permutations = 999)
plot(bd.aves)
