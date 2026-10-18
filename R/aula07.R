library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))

mo <- cores_familia[["Modelos"]]
ne <- cores[["accent_dark"]]
vm <- "#c0392b"

# ---- dados -------------------------------------------------------------------

#' Aranhas caçadoras (mvabund::spider): 12 espécies em 28 locais e 6
#' variáveis ambientais, padronizadas
aranhas <- function() {
  e <- new.env()
  utils::data("spider", package = "mvabund", envir = e)
  list(Y = e$spider$abund, X = as.data.frame(scale(e$spider$x)))
}

formula_amb <- ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection

nomes_amb <- c(soil.dry = "solo seco", bare.sand = "areia nua", fallen.leaves = "folhiço",
               moss = "musgo", herb.layer = "herbáceas", reflection = "reflexão")

#' Modelos usados na aula, ajustados uma vez
modelos_aula7 <- function() {
  a <- aranhas()
  y <- mvabund::mvabund(a$Y)
  dados <- cbind(a$X)
  m_nb <- mvabund::manyglm(y ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection,
                           data = dados, family = "negative.binomial")
  m_po <- mvabund::manyglm(y ~ soil.dry + bare.sand + fallen.leaves + moss + herb.layer + reflection,
                           data = dados, family = "poisson")
  set.seed(1)
  av <- mvabund::anova.manyglm(m_nb, nBoot = 999, p.uni = "adjusted", show.time = "none")
  set.seed(1)
  g0 <- gllvm::gllvm(a$Y, family = "negative.binomial", num.lv = 2, n.init = 3)
  set.seed(1)
  # com as preditoras, a segunda variável latente some (variância ~0): uma basta
  g1 <- gllvm::gllvm(a$Y, a$X, family = "negative.binomial", num.lv = 1, n.init = 10,
                     formula = formula_amb)
  set.seed(1)
  gc <- gllvm::gllvm(a$Y, a$X, family = "negative.binomial", num.lv.c = 2, n.init = 3,
                     lv.formula = formula_amb)
  m0 <- mvabund::manyglm(y ~ 1, family = "negative.binomial")
  set.seed(1)
  co <- ecoCopula::cord(m0)
  set.seed(1)
  gr <- ecoCopula::cgr(m_nb)
  list(a = a, m_nb = m_nb, m_po = m_po, av = av, g0 = g0, g1 = g1, gc = gc, co = co, gr = gr)
}

# ---- mapa --------------------------------------------------------------------

#' As famílias de métodos baseados em modelos e os pacotes do R
fig_mapa_modelos <- function() {
  cx <- tribble(
    ~x,   ~y,   ~w,   ~h,  ~rotulo,                                            ~fill, ~txt,    ~face,
    0,    2.4,  4.6,  .9,  "Métodos baseados em modelos",                     mo,    "white", "bold",
    -5.4, .6,   2.6,  .9,  "Agrupamento",                                     cores[["papel"]], mo, "bold",
    -1.8, .6,   2.6,  .9,  "Ordenação",                                       cores[["papel"]], mo, "bold",
    1.8,  .6,   2.6,  .9,  "Inferência",                                      cores[["papel"]], mo, "bold",
    5.4,  .6,   2.6,  .9,  "Modelos conjuntos\n(JSDM)",                       cores[["papel"]], mo, "bold",
    -5.4, -1.1, 2.6,  1.6, "Arquétipos de\nespécies (SAMs)\necomix",          cores[["papel"]], ne, "plain",
    -1.8, -1.1, 2.6,  1.6, "CQO, GLLVM\ngllvm, boral\ncópulas: ecoCopula",    cores[["papel"]], ne, "plain",
    1.8,  -1.1, 2.6,  1.6, "GLMs multivariados\nmvabund\n(manyglm, anova)",   cores[["papel"]], ne, "plain",
    5.4,  -1.1, 2.6,  1.6, "Hmsc\nsjSDM",                                     cores[["papel"]], ne, "plain"
  ) |> mutate(cor = mo)
  setas <- tibble(x = 0, y = 1.95, xend = c(-5.4, -1.8, 1.8, 5.4), yend = 1.1) |>
    bind_rows(tibble(x = c(-5.4, -1.8, 1.8, 5.4), y = .15, xend = c(-5.4, -1.8, 1.8, 5.4), yend = -.25))
  diagrama(cx, setas, base = 6)
}

# ---- motivação ---------------------------------------------------------------

#' Relação média-variância das espécies de ácaros (vegan::mite): a variância
#' cresce com a média, bem mais rápido que na Poisson (variância = média)
fig_media_variancia <- function() {
  e <- new.env(); utils::data("mite", package = "vegan", envir = e)
  d <- tibble(media = colMeans(e$mite), var = apply(e$mite, 2, stats::var)) |>
    filter(media > 0, var > 0)
  fit <- stats::lm(log10(var) ~ log10(media), d)
  x <- 10^seq(log10(min(d$media)), log10(max(d$media)), length.out = 100)
  ggplot(d, aes(media, var)) +
    geom_line(data = tibble(media = x, var = x), colour = cores[["neutro"]], linetype = "22",
              linewidth = 1) +
    geom_point(shape = 21, fill = mo, colour = "white", size = 3.6) +
    annotate("text", x = max(x) * .7, y = max(x) * .35, label = "Poisson:\nvariância = média",
             colour = cores[["neutro"]], size = 4.8, hjust = 1, lineheight = .9) +
    annotate("text", x = min(d$media) * 1.5, y = max(d$var) * .6, hjust = 0, size = 4.8, colour = mo,
             label = sprintf("inclinação = %s", format(round(coef(fit)[2], 2), decimal.mark = ","))) +
    scale_x_log10() + scale_y_log10() +
    labs(x = "Média da abundância (escala log)", y = "Variância (escala log)",
         subtitle = "35 espécies de ácaros em 70 parcelas (vegan::mite)") +
    tema_aula(base = 15)
}

#' Posição × dispersão: dois grupos que diferem só na média (posição) de
#' algumas espécies; na PCA dos dados brutos (distância euclidiana), o grupo
#' com médias maiores parece mais disperso, porque a variância cresce com a média
sim_posicao <- function() {
  set.seed(4)
  n <- 20; p <- 12
  mu <- exp(seq(log(1), log(40), length.out = p))
  muB <- mu; muB[c(3, 6, 9, 12)] <- mu[c(3, 6, 9, 12)] * 2.5
  gera <- function(m) sapply(m, \(k) stats::rnbinom(n, mu = k, size = 1.5))
  Y <- rbind(gera(mu), gera(muB))
  list(Y = Y, g = factor(rep(c("A", "B"), each = n)))
}

fig_posicao_dispersao_mv <- function(s = sim_posicao()) {
  pc <- stats::prcomp(s$Y)
  d <- tibble(x = pc$x[, 1], y = pc$x[, 2], g = s$g)
  cen <- d |> group_by(g) |> summarise(cx = mean(x), cy = mean(y))
  dd <- d |> left_join(cen, by = "g")
  bd <- vegan::betadisper(stats::dist(s$Y), s$g)
  disp <- tapply(bd$distances, s$g, mean)
  pal <- c(A = cores[["accent"]], B = cores[["ferrugem"]])
  ggplot(dd, aes(x, y, colour = g)) +
    geom_segment(aes(xend = cx, yend = cy), alpha = .35) +
    geom_point(size = 2.8) +
    geom_point(data = cen, aes(cx, cy), shape = 4, size = 5, stroke = 2) +
    scale_colour_manual(values = pal, name = NULL,
                        labels = sprintf("%s: dispersão média = %s", names(disp),
                                         format(round(disp, 1), decimal.mark = ","))) +
    coord_equal() +
    labs(x = "PC1", y = "PC2",
         subtitle = "Os grupos diferem só na média de 4 espécies (dados simulados)") +
    tema_aula(base = 15) + theme(legend.position = "top")
}

#' Resíduos de Dunn-Smyth contra os valores ajustados: Poisson × binomial
#' negativa nas aranhas
fig_residuos_po_nb <- function(md) {
  um <- function(m, tit) {
    d <- tibble(aj = as.vector(log(stats::fitted(m))), res = as.vector(stats::residuals(m)),
                sp = rep(colnames(m$y), each = nrow(m$y)))
    ggplot(d, aes(aj, res, colour = sp)) +
      geom_hline(yintercept = 0, linetype = "22", colour = cores[["neutro"]]) +
      geom_point(size = 1.8, alpha = .8) +
      scale_colour_manual(values = grDevices::hcl.colors(12, "Dark 3"), guide = "none") +
      coord_cartesian(ylim = c(-4.5, 4.5)) +
      labs(title = tit, x = "Preditor linear (log da média ajustada)", y = "Resíduo de Dunn-Smyth") +
      tema_aula(base = 14)
  }
  um(md$m_po, "Poisson: leque, resíduos grandes") + um(md$m_nb, "Binomial negativa: sem padrão")
}

# ---- agrupamento -----------------------------------------------------------------

#' Arquétipos de espécies (SAMs): poucas curvas de resposta ao gradiente, cada
#' espécie atribuída a uma delas (dados simulados)
fig_arquetipos <- function() {
  set.seed(3)
  x <- seq(0, 10, length.out = 120)
  fs <- list("Arquétipo 1" = \(x) 12 * plogis(-1.4 * (x - 3)),
             "Arquétipo 2" = \(x) 14 * exp(-(x - 5.5)^2 / 3),
             "Arquétipo 3" = \(x) 10 * plogis(1.3 * (x - 7)))
  d_arq <- bind_rows(lapply(names(fs), \(a) tibble(arq = a, x = x, y = fs[[a]](x))))
  d_sp <- bind_rows(lapply(1:15, \(i) {
    a <- names(fs)[(i - 1) %/% 5 + 1]
    tibble(sp = i, arq = a, x = x, y = runif(1, .5, 1.3) * fs[[a]](x + stats::rnorm(1, 0, .5)))
  }))
  pal <- c("Arquétipo 1" = cores[["accent"]], "Arquétipo 2" = mo, "Arquétipo 3" = cores[["verde"]])
  ggplot() +
    geom_line(data = d_sp, aes(x, y, group = sp, colour = arq), alpha = .35, linewidth = .6) +
    geom_line(data = d_arq, aes(x, y, colour = arq), linewidth = 2) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(x = "Gradiente ambiental", y = "Abundância esperada",
         subtitle = "15 espécies, 3 respostas: linhas grossas, os arquétipos; finas, as espécies") +
    tema_aula(base = 15) + theme(legend.position = "top")
}

#' As três estratégias para regionalizar com dados de espécies (adaptado de
#' Hill et al. 2020)
fig_estrategias_regionalizacao <- function() {
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,
    0,    1.9,   3.4, .8,  "Agrupar, depois prever",
    4.2,  1.9,   3.4, .8,  "Prever, depois agrupar",
    8.4,  1.9,   3.4, .8,  "Um passo só",
    0,    .5,    3.4, 1.4, "agrupamento dos locais\n→ modelo dos grupos\nem função do ambiente",
    4.2,  .5,    3.4, 1.4, "um SDM por espécie\n→ agrupamento das\nprevisões",
    8.4,  .5,    3.4, 1.4, "agrupa as espécies (SAMs)\nou os locais (RCPs) pela\nresposta ao ambiente"
  ) |> mutate(cor = c(ne, ne, mo, ne, ne, mo), face = c("bold", "bold", "bold", "plain", "plain", "plain"))
  setas <- tibble(x = c(0, 4.2, 8.4), y = 1.5, xend = c(0, 4.2, 8.4), yend = 1.2)
  diagrama(cx, setas, base = 4.8) +
    annotate("text", x = 8.4, y = -.6, label = "melhor desempenho em Hill et al. (2020)", size = 4.4,
             colour = mo, fontface = "italic")
}

# ---- ordenação ---------------------------------------------------------------

#' Curvas de resposta unimodais com larguras de nicho iguais ou diferentes
fig_larguras_nicho <- function() {
  x <- seq(-4, 4, length.out = 200)
  ot <- c(-2.2, -.6, .8, 2.3)
  d <- bind_rows(
    tidyr::crossing(sp = 1:4, x = x) |> mutate(tol = 1, caso = "Larguras iguais"),
    tidyr::crossing(sp = 1:4, x = x) |> mutate(tol = c(.5, 1.5, .8, 1.2)[sp],
                                               caso = "Larguras diferentes")) |>
    mutate(y = exp(-(x - ot[sp])^2 / (2 * tol^2)),
           caso = factor(caso, c("Larguras iguais", "Larguras diferentes")))
  ggplot(d, aes(x, y, colour = factor(sp))) +
    geom_line(linewidth = 1.3) +
    facet_wrap(~caso, nrow = 1) +
    scale_colour_manual(values = grDevices::hcl.colors(4, "Dark 3"), guide = "none") +
    labs(x = "Variável latente (gradiente não medido)", y = "Abundância esperada") +
    tema_aula(base = 15) + theme(axis.text.y = element_blank())
}

# escores e cargas de um gllvm, orientados por uma espécie de referência
gllvm_escores <- function(g, ref = "Pardlugu") {
  lv <- gllvm::getLV(g)
  th <- g$params$theta[, 1:2, drop = FALSE]
  if (!is.null(g$params$sigma.lv)) th <- sweep(th, 2, g$params$sigma.lv[1:2], `*`)
  s <- sign(th[ref, ]); s[s == 0] <- 1
  list(lv = sweep(lv[, 1:2], 2, s, `*`), th = sweep(th, 2, s, `*`))
}

#' Ordenação baseada em modelos das aranhas (GLLVM binomial negativo com duas
#' variáveis latentes)
fig_gllvm_ordenacao <- function(g, titulo = "GLLVM irrestrito") {
  e <- gllvm_escores(g)
  lv <- tibble(x = e$lv[, 1], y = e$lv[, 2], r = seq_len(nrow(e$lv)))
  k <- .8 * max(abs(e$lv)) / max(abs(e$th))
  th <- tibble(x = e$th[, 1] * k, y = e$th[, 2] * k, sp = rownames(e$th))
  ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_text(data = lv, aes(x, y, label = r), colour = cores[["neutro"]], size = 4) +
    geom_text(data = th, aes(x, y, label = sp), colour = mo, fontface = "italic", size = 4.4) +
    coord_equal() + labs(x = "Variável latente 1", y = "Variável latente 2", subtitle = titulo) +
    tema_aula(base = 15)
}

#' Ordenação concorrente (van der Veen et al. 2023): as variáveis latentes são
#' parcialmente explicadas pelas preditoras, que viram setas
fig_gllvm_concorrente <- function(g) {
  e <- gllvm_escores(g)
  b <- g$params$LvXcoef[, 1:2]
  s <- sign(g$params$theta["Pardlugu", 1:2]); s[s == 0] <- 1
  b <- sweep(b, 2, s, `*`)
  k <- .8 * max(abs(e$lv)) / max(abs(b))
  bp <- tibble(x = b[, 1] * k, y = b[, 2] * k, v = nomes_amb[rownames(b)])
  fig_gllvm_ordenacao(g, "Ordenação concorrente (num.lv.c = 2)") +
    geom_segment(data = bp, aes(0, 0, xend = x, yend = y), colour = cores[["verde"]], linewidth = 1,
                 arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    geom_label(data = bp, aes(x * 1.12, y * 1.12, label = v), colour = cores[["verde"]], size = 4.2,
               fontface = "bold", fill = alpha(cores[["papel"]], .85), linewidth = 0)
}

# matriz de correlação em mapa de calor (triângulo inferior)
mapa_cor <- function(R, titulo) {
  o <- stats::hclust(stats::as.dist(1 - R))$order
  R <- R[o, o]
  d <- as_tibble(R, rownames = "a") |> pivot_longer(-a, names_to = "b", values_to = "r") |>
    mutate(a = factor(a, rownames(R)), b = factor(b, rownames(R))) |>
    filter(as.integer(a) > as.integer(b))
  ggplot(d, aes(b, a, fill = r)) +
    geom_tile(colour = "white") +
    scale_fill_gradient2(low = vm, mid = "white", high = cores[["accent"]], limits = c(-1, 1),
                         name = "r") +
    coord_equal() + labs(x = NULL, y = NULL, title = titulo) +
    tema_aula(base = 13) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"),
          axis.text.y = element_text(face = "italic"), panel.grid = element_blank())
}

#' Correlação entre espécies sem preditoras (inclui as respostas comuns ao
#' ambiente) e a correlação residual, depois de incluir o ambiente
fig_cor_total_residual <- function(md) {
  mapa_cor(gllvm::getResidualCor(md$g0), "Sem preditoras") +
    mapa_cor(gllvm::getResidualCor(md$g1), "Residual, com as 6 preditoras") +
    plot_layout(guides = "collect")
}

#' Partição de variância de um GLLVM por espécie: cada preditora e as
#' variáveis latentes
fig_particao_gllvm <- function(md) {
  vp <- gllvm::VP(md$g1)
  P <- vp$PropExplainedVarSp
  colnames(P) <- c(nomes_amb[vp$groupnames[1:6]], rep("variáveis latentes", ncol(P) - 6))
  d <- as_tibble(P, rownames = "sp") |> pivot_longer(-sp, names_to = "comp", values_to = "prop") |>
    group_by(sp, comp) |> summarise(prop = sum(prop), .groups = "drop") |>
    mutate(comp = factor(comp, c(nomes_amb, "variáveis latentes")))
  pal <- c(grDevices::hcl.colors(6, "Dark 3"), cores[["neutro"]])
  names(pal) <- levels(d$comp)
  ggplot(d, aes(prop, sp, fill = comp)) +
    geom_col(width = .75) +
    scale_fill_manual(values = pal, name = NULL) +
    scale_x_continuous(labels = \(z) paste0(round(100 * z), "%")) +
    labs(x = "Proporção da variância explicada", y = NULL) +
    tema_aula(base = 14) + theme(axis.text.y = element_text(face = "italic"), legend.position = "right")
}

#' Cópula gaussiana: pontos de uma normal bivariada correlacionada viram
#' contagens discretas pelas marginais (binomial negativa e ZINB)
fig_copula <- function(rho = .6) {
  set.seed(5)
  n <- 400
  z <- MASS::mvrnorm(n, c(0, 0), matrix(c(1, rho, rho, 1), 2))
  u <- stats::pnorm(z)
  y1 <- stats::qnbinom(u[, 1], mu = 4, size = 1.5)
  pi0 <- .4
  y2 <- ifelse(u[, 2] < pi0, 0, stats::qnbinom(pmax(u[, 2] - pi0, 0) / (1 - pi0), mu = 6, size = 2))
  p1 <- ggplot(tibble(z1 = z[, 1], z2 = z[, 2]), aes(z1, z2)) +
    geom_point(colour = mo, alpha = .5, size = 1.6) + coord_equal() +
    labs(title = sprintf("1. Espaço da cópula (ρ = %s)", format(rho, decimal.mark = ",")),
         x = "z (espécie 1)", y = "z (espécie 2)") + tema_aula(base = 14)
  p2 <- ggplot(tibble(y1, y2) |> count(y1, y2), aes(y1, y2, size = n)) +
    geom_point(colour = cores[["accent"]], alpha = .8) +
    scale_size_area(max_size = 9, guide = "none") +
    labs(title = "2. Contagens observadas", x = "Espécie 1 (binomial negativa)",
         y = "Espécie 2 (ZINB)") + tema_aula(base = 14)
  p1 + p2
}

#' Ordenação com cópulas (ecoCopula::cord) das aranhas, sem preditoras
fig_cord <- function(md) {
  co <- md$co
  rownames(co$loadings) <- colnames(md$a$Y)
  s <- sign(co$loadings["Pardlugu", ]); s[s == 0] <- 1
  sc <- sweep(co$scores, 2, s, `*`); ld <- sweep(co$loadings, 2, s, `*`)
  k <- .8 * max(abs(sc)) / max(abs(ld))
  ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_text(data = tibble(x = sc[, 1], y = sc[, 2], r = seq_len(nrow(sc))), aes(x, y, label = r),
              colour = cores[["neutro"]], size = 4) +
    geom_text(data = tibble(x = ld[, 1] * k, y = ld[, 2] * k, sp = rownames(ld)), aes(x, y, label = sp),
              colour = mo, fontface = "italic", size = 4.4) +
    coord_equal() + labs(x = "Fator 1", y = "Fator 2", subtitle = "ecoCopula::cord(), aranhas") +
    tema_aula(base = 15)
}

# ---- inferência --------------------------------------------------------------

#' Estatística da razão de verossimilhança por espécie para cada preditora
#' (anova.manyglm com p.uni): quem responde ao ambiente
fig_lr_especies <- function(md) {
  u <- md$av$uni.test[-1, , drop = FALSE]
  pu <- md$av$uni.p[-1, , drop = FALSE]
  d <- as_tibble(u, rownames = "var") |> pivot_longer(-var, names_to = "sp", values_to = "LR") |>
    left_join(as_tibble(pu, rownames = "var") |> pivot_longer(-var, names_to = "sp", values_to = "p"),
              by = c("var", "sp")) |>
    mutate(var = factor(nomes_amb[var], nomes_amb), sig = p < .05)
  ggplot(d, aes(LR, sp, fill = sig)) +
    geom_col(width = .75) +
    facet_wrap(~var, nrow = 1) +
    scale_fill_manual(values = c(`TRUE` = mo, `FALSE` = cores[["neutro_cl"]]),
                      labels = c(`TRUE` = "p < 0,05 (ajustado)", `FALSE` = "p ≥ 0,05"), name = NULL) +
    labs(x = "Razão de verossimilhança (desvio)", y = NULL) +
    tema_aula(base = 13) + theme(axis.text.y = element_text(face = "italic"), legend.position = "top")
}

# ---- JSDM ----------------------------------------------------------------------

#' Os dados de um JSDM: ocorrências (Y), ambiente (X), atributos (T),
#' filogenia (C) e o contexto espaço-temporal (adaptado de Ovaskainen et al. 2017)
fig_hmsc_matrizes <- function() {
  mat <- function(x, y, r, w, h, cor, sub) tibble(x, y, w, h, rotulo = paste0(r, "\n", sub), cor,
                                                tam = 1, face = "bold")
  cx <- bind_rows(
    mat(0, 0, "Y", 2, 2.4, mo, "espécies\nnos locais"),
    mat(3, 0, "X", 1.6, 2.4, cores[["verde"]], "ambiente"),
    mat(0, 2.6, "T", 2, 1.2, cores[["ferrugem"]], "atributos"),
    mat(0, -2.6, "C", 2, 2, cores[["accent"]], "filogenia"),
    mat(-3, 0, "S", 1.6, 2.4, cores[["ocre"]], "espaço\ne tempo"))
  setas <- tibble(x = c(2.2, 0, 0, -2.2), y = c(0, 1.95, -1.55, 0),
                  xend = c(1.05, 0, 0, -1.05), yend = c(0, 1.25, -1.25, 0))
  diagrama(cx, setas, base = 5) +
    annotate("text", x = 3.3, y = -2.6, hjust = 0, size = 4.6, colour = ne, lineheight = .95,
             label = "Y: locais × espécies\nX: locais × variáveis\nT: espécies × atributos\nC: espécies × espécies\nS: coordenadas, datas")
}

#' O grafo do HMSC: dados (caixas) e parâmetros (elipses) (adaptado de
#' Ovaskainen et al. 2017)
fig_hmsc_dag <- function() {
  cx <- tribble(
    ~x,   ~y,   ~w,   ~h,  ~rotulo,                                 ~cor,
    -3.2, 2.2,  1.7,  .8,  "Atributos\nT",                          cores[["ferrugem"]],
    0,    2.2,  1.7,  .8,  "Filogenia\nC",                          cores[["accent"]],
    -1.6, .9,   3,    .8,  "γ, ρ: efeitos dos atributos\ne sinal filogenético", mo,
    -1.6, -.4,  3,    .8,  "β: resposta de cada\nespécie ao ambiente",   mo,
    -4.6, -.4,  1.7,  .8,  "Ambiente\nX",                           cores[["verde"]],
    2.6,  -.4,  3,    .8,  "η, Λ: variáveis latentes\n(associações residuais)", mo,
    2.6,  .9,   1.7,  .8,  "Espaço e\ntempo S",                     cores[["ocre"]],
    -1.6, -1.8, 3,    .8,  "Ocorrências Y",                         ne
  )
  setas <- tribble(~x, ~y, ~xend, ~yend,
                   -3.2, 1.8, -2.4, 1.3,   0, 1.8, -.8, 1.3,   -1.6, .5, -1.6, 0,
                   -3.75, -.4, -3.1, -.4,  -1.6, -.8, -1.6, -1.4,  2.6, .5, 2.6, 0,
                   1.4, -.8, -.4, -1.5)
  diagrama(cx, setas, base = 4.4)
}

#' Associação direta × indireta: duas espécies (A e B) correlacionadas só por
#' causa de uma mediadora (M); a correlação parcial some (dados simulados)
fig_mediadora <- function() {
  set.seed(2)
  n <- 300
  M <- stats::rnorm(n); A <- .8 * M + stats::rnorm(n, 0, .6); B <- .8 * M + stats::rnorm(n, 0, .6)
  Z <- cbind(A, M, B)
  R <- stats::cor(Z)
  P <- -stats::cov2cor(solve(stats::cov(Z))); diag(P) <- 1
  pos <- tibble(no = c("A", "M", "B"), x = c(0, 1, 2), y = c(0, 1.2, 0))
  rede <- function(Mx, tit) {
    pares <- tibble(i = c(1, 1, 2), j = c(2, 3, 3)) |>
      mutate(r = Mx[cbind(i, j)], x = pos$x[i], y = pos$y[i], xend = pos$x[j], yend = pos$y[j],
             lx = (x + xend) / 2, ly = (y + yend) / 2 + c(.12, -.15, .12))
    ggplot() +
      geom_segment(data = pares, aes(x, y, xend = xend, yend = yend, linewidth = abs(r)),
                   colour = ifelse(abs(pares$r) < .1, cores[["neutro_cl"]], cores[["accent"]])) +
      geom_label(data = pares, aes(lx, ly, label = format(round(r, 2), decimal.mark = ",")), size = 5,
                 fill = cores[["papel"]], linewidth = 0) +
      geom_point(data = pos, aes(x, y), size = 16, colour = mo) +
      geom_text(data = pos, aes(x, y, label = no), colour = "white", size = 7, fontface = "bold") +
      scale_linewidth(range = c(.5, 4), guide = "none") +
      coord_equal(xlim = c(-.4, 2.4), ylim = c(-.5, 1.6)) + labs(title = tit) + theme_void(base_size = 16) +
      theme(plot.title = element_text(hjust = .5, face = "bold", colour = ne),
            plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  }
  rede(R, "Correlação") + rede(P, "Correlação parcial")
}

#' Rede de associações diretas entre as aranhas, depois de controlar o
#' ambiente (ecoCopula::cgr, modelo gráfico)
fig_rede_aranhas <- function(md) {
  G <- md$gr$best_graph$part
  sp <- colnames(md$a$Y)
  n <- length(sp)
  ang <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1)[-(n + 1)]
  pos <- tibble(sp, x = cos(ang), y = sin(ang))
  ed <- which(upper.tri(G) & G != 0, arr.ind = TRUE)
  d <- tibble(i = ed[, 1], j = ed[, 2], r = G[ed]) |>
    mutate(x = pos$x[i], y = pos$y[i], xend = pos$x[j], yend = pos$y[j])
  ggplot() +
    geom_segment(data = d, aes(x, y, xend = xend, yend = yend, colour = r > 0, linewidth = abs(r))) +
    geom_point(data = pos, aes(x, y), size = 5, colour = mo) +
    geom_text(data = pos, aes(x * 1.2, y * 1.2, label = sp), size = 4.2, fontface = "italic") +
    scale_colour_manual(values = c(`TRUE` = cores[["accent"]], `FALSE` = vm),
                        labels = c(`TRUE` = "positiva", `FALSE` = "negativa"), name = NULL) +
    scale_linewidth(range = c(.4, 3), guide = "none") +
    coord_equal(xlim = c(-1.45, 1.45), ylim = c(-1.3, 1.3), clip = "off") + theme_void(base_size = 15) +
    theme(legend.position = "bottom", plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}
