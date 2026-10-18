library(vegan)      # para comparar com a PERMANOVA
library(mvabund)
library(gllvm)
library(ecoCopula)
library(ecodados)   # remotes::install_github("paternogbc/ecodados")

anu <- mvabund(anuros_permanova[, names(anuros_permanova) != "GRUPOS"])
fonte <- factor(anuros_permanova$GRUPOS)

meanvar.plot(anu)

plot(anu ~ fonte, cex.axis = 0.8, cex = 0.8)   # abundância de cada espécie por fonte
m.anu <- manyglm(anu ~ fonte, family = "negative.binomial")
plot(m.anu)                                    # resíduos de Dunn-Smyth

set.seed(1)
anova(m.anu, nBoot = 999, p.uni = "adjusted")

set.seed(1)
adonis2(anuros_permanova[, names(anuros_permanova) != "GRUPOS"] ~ fonte,
        method = "bray", permutations = 999)

data(spider)
Y <- mvabund(spider$abund)
X <- as.data.frame(scale(spider$x))   # variáveis padronizadas

m.po <- manyglm(Y ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection,
                data = X, family = "poisson")
m.nb <- manyglm(Y ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection,
                data = X, family = "negative.binomial")

par(mfrow = c(1, 2))
plot(m.po, which = 1, caption = "Poisson")
plot(m.nb, which = 1, caption = "Binomial negativa")
par(mfrow = c(1, 1))

sum(AIC(m.po))
sum(AIC(m.nb))

set.seed(1)
an.nb <- anova(m.nb, nBoot = 999, p.uni = "adjusted")
an.nb$table      # teste multivariado de cada variável
an.nb$uni.p      # p de cada espécie, ajustados

set.seed(1)
g0 <- gllvm(spider$abund, family = "negative.binomial", num.lv = 2, n.init = 3)
g0
gllvm::ordiplot(g0, biplot = TRUE, main = "GLLVM, 2 variáveis latentes")

set.seed(1)
g1 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 1, n.init = 10,
            formula = ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection)
coefplot(g1)   # efeito de cada variável em cada espécie, com intervalos

cr0 <- getResidualCor(g0)
cr1 <- getResidualCor(g1)
par(mfrow = c(1, 2))
heatmap(cr0, symm = TRUE, Rowv = NA, main = "Sem preditores", zlim = c(-1, 1),
        col = hcl.colors(21, "Blue-Red 3"))
heatmap(cr1, symm = TRUE, Rowv = NA, main = "Com preditores", zlim = c(-1, 1),
        col = hcl.colors(21, "Blue-Red 3"))
par(mfrow = c(1, 1))

set.seed(1)
gc <- gllvm(spider$abund, X, family = "negative.binomial", num.lv.c = 2, n.init = 3,
            lv.formula = ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection)
gllvm::ordiplot(gc, biplot = TRUE, main = "Ordenação concorrente")

m0 <- manyglm(Y ~ 1, family = "negative.binomial")
set.seed(1)
co <- cord(m0)
plot(co, biplot = TRUE)

set.seed(1)
gr <- cgr(m.nb)
plot(gr, pad = 1)
