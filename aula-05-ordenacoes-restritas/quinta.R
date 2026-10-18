library(vegan)   # ordiR2step() precisa do vegan carregado, não basta vegan::
library(ade4)
library(adespatial)
library(MASS)

data(doubs, package = "ade4")
spe <- doubs$fish[-8, ]
env <- as.data.frame(scale(doubs$env[-8, -1]))   # sem o local 8 e sem dfs
xy  <- doubs$xy[-8, ]

spe.hel <- decostand(spe, "hellinger")

spe.cca <- cca(spe ~ ., data = env)
spe.cca

summary(spe.cca)   # scaling 2 é o padrão
RsquareAdj(spe.cca)

plot(spe.cca, scaling = 1, display = c("sp", "lc", "cn"),
     main = "CCA, scaling 1")
plot(spe.cca, scaling = 2, display = c("sp", "lc", "cn"),
     main = "CCA, scaling 2")

set.seed(1)
anova(spe.cca, permutations = 999)                 # modelo inteiro
set.seed(1)
anova(spe.cca, by = "axis", permutations = 999)    # cada eixo canônico
set.seed(1)
anova(spe.cca, by = "margin", permutations = 999)  # cada variável, dadas as outras

spe.rda <- rda(spe.hel ~ ., data = env)
summary(spe.rda)
RsquareAdj(spe.rda)

coef(spe.rda)

r2a <- RsquareAdj(spe.rda)$adj.r.squared
r2a * cumsum(spe.rda$CCA$eig) / sum(spe.rda$CCA$eig)

plot(spe.rda, scaling = 1, display = c("sp", "lc", "cn"),
     main = "RDA, scaling 1")
plot(spe.rda, scaling = 2, display = c("sp", "lc", "cn"),
     main = "RDA, scaling 2")

set.seed(1)
anova(spe.rda, permutations = 999)
set.seed(1)
anova(spe.rda, by = "axis", permutations = 999)

spe.rda$CA$eig[spe.rda$CA$eig > mean(spe.rda$CA$eig)]

spe.dca <- decorana(spe)
diff(range(scores(spe.dca, display = "sites", choices = 1)))

vif.cca(spe.rda)

mod0 <- rda(spe.hel ~ 1, data = env)   # só o intercepto
set.seed(1)
spe.sel <- ordiR2step(mod0, scope = formula(spe.rda),
                      permutations = 999, trace = FALSE)
spe.sel$anova
RsquareAdj(spe.sel)

spe.dbrda <- dbrda(spe ~ ., data = env, distance = "bray")
spe.dbrda
RsquareAdj(spe.dbrda)
set.seed(1)
anova(spe.dbrda, permutations = 999)

envtopo <- env[, c("alt", "slo", "flo")]                              # relevo
envchem <- env[, c("pH", "har", "pho", "nit", "amm", "oxy", "bdo")]   # química da água

spe.parc <- rda(spe.hel, envchem, envtopo)   # forma matricial: Y, X, W
spe.parc <- rda(spe.hel ~ pH + har + pho + nit + amm + oxy + bdo +
                  Condition(alt + slo + flo), data = env)
spe.parc
set.seed(1)
anova(spe.parc, permutations = 999)

mem <- as.data.frame(dbmem(xy, MEM.autocor = "positive", silent = FALSE))
ncol(mem)

mem.full <- rda(spe.hel ~ ., data = mem)
set.seed(1)
mem.sel <- ordiR2step(rda(spe.hel ~ 1, data = mem), scope = formula(mem.full),
                      permutations = 999, trace = FALSE)
mem.sel$anova
mem.x <- mem[, attr(terms(mem.sel), "term.labels"), drop = FALSE]
names(mem.x)

env.x <- env[, attr(terms(spe.sel), "term.labels"), drop = FALSE]

spe.vp <- varpart(spe.hel, env.x, mem.x)
spe.vp
plot(spe.vp, digits = 2, Xnames = c("Ambiente", "Espaço"), bg = c("seagreen", "goldenrod"))

set.seed(1)
anova(rda(spe.hel, env.x), permutations = 999)          # [a + b]
set.seed(1)
anova(rda(spe.hel, mem.x), permutations = 999)          # [b + c]
set.seed(1)
anova(rda(spe.hel, env.x, mem.x), permutations = 999)   # [a]
set.seed(1)
anova(rda(spe.hel, mem.x, env.x), permutations = 999)   # [c]

showvarparts(2)
showvarparts(3)

gr <- cutree(hclust(dist(spe.hel), "ward.D2"), k = 4)
table(gr)

env0 <- doubs$env[-8, ]
env.lda <- data.frame(alt = env0$alt, oxy = env0$oxy / 10, bdo = env0$bdo / 10)

set.seed(1)
permutest(betadisper(dist(env.lda), gr), permutations = 999)

env.ln <- data.frame(ln_alt = log(env.lda$alt), oxy = env.lda$oxy,
                     ln_bdo = log(env.lda$bdo), row.names = rownames(env0))
set.seed(1)
permutest(betadisper(dist(env.ln), gr), permutations = 999)

summary(manova(as.matrix(env.ln) ~ factor(gr)), test = "Wilks")

env.sc <- as.data.frame(scale(env.ln))
spe.lda <- lda(gr ~ ., data = env.sc)
spe.lda$scaling   # coeficientes das funções discriminantes (variáveis padronizadas)
spe.lda$svd^2     # autovalores canônicos: quanto cada função separa os grupos

classe <- predict(spe.lda)$class
(tab <- table(observado = gr, previsto = classe))
diag(prop.table(tab, 1))   # proporção de acertos em cada grupo

spe.cv <- lda(gr ~ ., data = env.sc, CV = TRUE)
(tab.cv <- table(observado = gr, previsto = spe.cv$class))
diag(prop.table(tab.cv, 1))

novos <- data.frame(ln_alt = log(c(900, 250)), oxy = c(9, 10),
                    ln_bdo = log(c(2.2, 2.7)))
novos.sc <- scale(novos, center = attr(scale(env.ln), "scaled:center"),
                  scale = attr(scale(env.ln), "scaled:scale"))
predict(spe.lda, newdata = as.data.frame(novos.sc))

fp <- predict(spe.lda)$x
plot(fp[, 1:2], type = "n", xlab = "LD1", ylab = "LD2")
text(fp[, 1:2], labels = rownames(env.sc), col = gr + 1)
