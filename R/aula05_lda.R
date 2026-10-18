library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/aula05_rda.R"))
source(here::here("R/aula03_hierarquico.R"))   # doubs_hc()

#' Análise discriminante linear, sem pacote extra
#'
#' Autovetores de W⁻¹B (W: dispersão dentro dos grupos; B: entre grupos),
#' escalonados para variância dentro dos grupos igual a 1. A classificação
#' atribui cada objeto ao centroide mais próximo no espaço discriminante.
#'
#' @param X matriz de descritores (objetos nas linhas)
#' @param g vetor de grupos
lda_manual <- function(X, g) {
  X <- as.matrix(X); g <- factor(g)
  n <- nrow(X); k <- nlevels(g)
  Xc <- sweep(X, 2, colMeans(X))
  medias <- t(sapply(levels(g), \(l) colMeans(X[g == l, , drop = FALSE])))
  W <- Reduce(`+`, lapply(levels(g), \(l) {
    xi <- sweep(X[g == l, , drop = FALSE], 2, medias[l, ]); crossprod(xi) }))
  Tt <- crossprod(Xc)
  B <- Tt - W
  e <- eigen(solve(W) %*% B)
  r <- min(k - 1, ncol(X))
  U <- Re(e$vectors[, 1:r, drop = FALSE])
  # escala: variância dentro dos grupos = 1 em cada eixo
  s <- sqrt(diag(t(U) %*% (W / (n - k)) %*% U))
  U <- sweep(U, 2, s, "/")
  colnames(U) <- paste0("LD", 1:r)
  esc <- Xc %*% U
  cent <- t(sapply(levels(g), \(l) colMeans(esc[g == l, , drop = FALSE])))
  if (r == 1) cent <- t(cent)
  atrib <- levels(g)[apply(esc, 1, \(z) which.min(colSums((t(cent) - z)^2)))]
  list(escores = esc, centroides = cent, autovalores = Re(e$values[1:r]),
       wilks = det(W) / det(Tt), grupo = g,
       tabela = table(Observado = g, Atribuído = factor(atrib, levels(g))))
}

#' Dois grupos que se sobrepõem nos dois descritores, mas se separam bem num
#' eixo discriminante (inspirado na Fig. 11.11 de Legendre & Legendre 2012)
lda_sim <- function() {
  set.seed(12)
  n <- 30
  a <- cbind(rnorm(n, 4, 1.1), rnorm(n, 0, .45)); a[, 2] <- a[, 2] + a[, 1] - 4
  b <- cbind(rnorm(n, 4, 1.1), rnorm(n, 0, .45)); b[, 2] <- b[, 2] + b[, 1] - 2.6
  tibble(x1 = c(a[, 1], b[, 1]), x2 = c(a[, 2], b[, 2]) + 4, g = rep(c("A", "B"), each = n))
}

#' Distância de Mahalanobis entre os dois grupos simulados:
#' D² = d₁₂ V⁻¹ d₁₂', com V a matriz de covariância conjunta dentro dos grupos
mahal_ex <- function(d = lda_sim()) {
  X <- as.matrix(d[, 1:2]); g <- d$g
  m <- t(sapply(c("A", "B"), \(l) colMeans(X[g == l, ])))
  V <- Reduce(`+`, lapply(c("A", "B"), \(l) crossprod(sweep(X[g == l, ], 2, m[l, ])))) /
    (nrow(X) - 2)
  d12 <- m["A", ] - m["B", ]
  list(medias = m, d12 = d12, V = V, D2 = as.numeric(t(d12) %*% solve(V) %*% d12),
       euc = sqrt(sum(d12^2)))
}

#' Os dois grupos simulados com as médias, a diferença entre elas e a elipse da
#' covariância conjunta dentro dos grupos
fig_mahal_grupos <- function(d = lda_sim()) {
  mh <- mahal_ex(d)
  pal <- c(A = cores[["accent"]], B = cores[["ferrugem"]])
  t <- seq(0, 2 * pi, length.out = 200); e <- eigen(mh$V)
  elip <- lapply(c("A", "B"), \(l) {
    xy <- t(mh$medias[l, ] + e$vectors %*% diag(sqrt(e$values) * 2) %*% rbind(cos(t), sin(t)))
    tibble(x = xy[, 1], y = xy[, 2], g = l) }) |> bind_rows()
  m <- as_tibble(mh$medias, rownames = "g")
  ggplot() +
    geom_point(data = d, aes(x1, x2, fill = g), shape = 21, colour = "white", size = 2.6, alpha = .7) +
    geom_path(data = elip, aes(x, y, colour = g), linewidth = .8) +
    annotate("segment", x = m$x1[2], y = m$x2[2], xend = m$x1[1], yend = m$x2[1],
             colour = cores[["accent_dark"]], linewidth = 1.2,
             arrow = arrow(length = unit(.2, "cm"), ends = "both", type = "closed")) +
    geom_point(data = m, aes(x1, x2, fill = g), shape = 23, size = 5, colour = cores[["accent_dark"]]) +
    annotate("text", x = mean(m$x1) + .25, y = mean(m$x2), label = "d[12]", parse = TRUE,
             hjust = 0, size = 6, colour = cores[["accent_dark"]]) +
    scale_fill_manual(values = pal, name = NULL) +
    scale_colour_manual(values = pal, guide = "none") +
    coord_equal() +
    labs(x = "x1", y = "x2", subtitle = "Elipses: a covariância conjunta V, igual nos dois grupos") +
    tema_aula(base = 15) + theme(legend.position = "top")
}

fig_lda_ideia <- function() {
  d <- lda_sim()
  ld <- lda_manual(d[, 1:2], d$g)
  d$ld1 <- ld$escores[, 1]
  pal <- c(A = cores[["accent"]], B = cores[["ferrugem"]])
  bw <- .4; hs <- .2   # largura das classes e altura de cada objeto empilhado

  # histograma de quadradinhos: A embaixo, B em cima, em cada classe
  empilha <- function(v, g, larg = bw) {
    tibble(v, g) |> mutate(cl = floor(v / larg) * larg + larg / 2) |> arrange(cl, g) |>
      group_by(cl) |> mutate(k = row_number() - 1) |> ungroup()
  }
  y0 <- min(d$x2) - .5; x0 <- min(d$x1) - .5
  h1 <- empilha(d$x1, d$g) |> mutate(px = cl, py = y0 - k * hs)
  h2 <- empilha(d$x2, d$g) |> mutate(px = x0 - k * hs, py = cl)

  # direção de LD1 no plano de x1 e x2, passando pelo centro da nuvem
  dir <- coef(lm(ld1 ~ x1 + x2, d))[2:3]; dir <- dir / sqrt(sum(dir^2))
  if (dir[1] < 0) dir <- -dir
  cen <- colMeans(d[, 1:2])

  p1 <- ggplot() +
    geom_tile(data = h1, aes(px, py, fill = g), width = bw * .9, height = hs * .9) +
    geom_tile(data = h2, aes(px, py, fill = g), width = hs * .9, height = bw * .9) +
    annotate("segment", x = cen[1] - 2.2 * dir[1], y = cen[2] - 2.2 * dir[2],
             xend = cen[1] + 2.2 * dir[1], yend = cen[2] + 2.2 * dir[2],
             colour = cores[["accent_dark"]], linewidth = 1,
             arrow = arrow(length = unit(.22, "cm"), type = "closed")) +
    annotate("text", x = cen[1] + 2.4 * dir[1], y = cen[2] + 2.4 * dir[2], label = "LD1",
             hjust = 0, vjust = 1, size = 5, fontface = "bold", colour = cores[["accent_dark"]]) +
    geom_point(data = d, aes(x1, x2, fill = g), shape = 21, colour = "white", size = 3) +
    scale_fill_manual(values = pal, name = NULL) +
    coord_equal(clip = "off") +
    labs(x = "x1", y = "x2", title = "Os grupos se sobrepõem em x1 e em x2") +
    tema_aula(base = 14) +
    theme(legend.position = "inside", legend.position.inside = c(.08, .93))

  h3 <- empilha(d$ld1, d$g, larg = .5)
  p2 <- ggplot(h3, aes(cl, k + .5, fill = g)) +
    geom_tile(width = .45, height = .9) +
    scale_fill_manual(values = pal, guide = "none") +
    labs(x = "LD1", y = NULL, title = "...mas se separam no eixo discriminante") +
    tema_aula(base = 14) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  p1 / p2 + plot_layout(heights = c(2.4, 1))
}

#' Dados do exemplo de Borcard et al. (2018): três variáveis ambientais do
#' Doubs e os quatro grupos de locais definidos pelos peixes (Ward)
doubs_lda <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  env <- e$doubs$env[-8, ]
  X <- data.frame(ln_slo = log(env$slo), oxy = env$oxy, ln_bdo = log(env$bdo))
  X <- as.data.frame(scale(X))
  g <- stats::cutree(doubs_hc()$Ward, 4)
  list(X = X, g = g, fit = lda_manual(X, g))
}

#' Os locais do Doubs no espaço discriminante
fig_lda_doubs <- function() {
  d <- doubs_lda()
  sc <- as_tibble(d$fit$escores) |> mutate(g = factor(d$g), local = rownames(d$X))
  ce <- as_tibble(d$fit$centroides) |> mutate(g = factor(rownames(d$fit$centroides)))
  pal <- c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]], cores[["ocre"]])
  ggplot(sc, aes(LD1, LD2, colour = g)) +
    stat_ellipse(data = \(x) dplyr::filter(dplyr::add_count(x, g), n >= 4), level = .68,
                 linewidth = .7) +
    geom_text(aes(label = local), size = 3.8, fontface = "bold") +
    geom_point(data = ce, shape = 4, size = 5, stroke = 2) +
    scale_colour_manual(values = pal, name = "Grupo") +
    coord_equal() + tema_aula(base = 13)
}

# ---- métodos alternativos: esquemas de matrizes --------------------------------

# seta de duas pontas entre matrizes
seta2 <- function(x, y, xend, yend, cor = "#c0392b")
  annotate("segment", x = x, y = y, xend = xend, yend = yend, colour = cor, linewidth = 1.3,
           arrow = arrow(length = unit(.2, "cm"), ends = "both", type = "closed"))

# pilha de matrizes (tabelas repetidas no tempo ou em vários grupos)
pilha <- function(x, y, rotulo, n = 3, w = .9, h = 1.1, cor = cores[["accent_dark"]], dx = .14) {
  bind_rows(lapply(rev(seq_len(n) - 1), \(i)
    tibble(x = x - i * dx, y = y + i * dx, w, h, rotulo = if (i == 0) rotulo else "",
           cor, tam = 1.1, face = "bold", fill = "white")))
}

#' O que cada análise maximiza: cov² = corr² × var × var, com restrições
#' diferentes (redesenhado de Dray, Chessel & Thioulouse 2003)
fig_cancor_rda_coia <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(
    mat(0, 0, "Y", w = .8, h = 1.6, tam = 1.1), mat(1, 0, "X", w = .8, h = 1.6, tam = 1.1),
    mat(3.5, 0, "Y", w = 1.6, h = 1.6, tam = 1.1, cor = vd), mat(4.9, 0, "X", w = .8, h = 1.6, tam = 1.1, cor = vd),
    mat(7.3, 0, "Y", w = 1.6, h = 1.6, tam = 1.1), mat(9.1, 0, "X", w = 1.6, h = 1.6, tam = 1.1))
  diagrama(cx, base = 5.4) +
    annotate("text", x = 4.6, y = 2.6, size = 5.4, colour = ne, parse = TRUE,
             label = "'maximiza: '*cov^2*'(XQu, YRv)' == corr^2*'(XQu, YRv)' %*% 'var(XQu)' %*% 'var(YRv)'") +
    annotate("text", x = c(.5, 4.2, 8.2), y = 1.55, size = 5, fontface = "bold", lineheight = .9,
             colour = c(ne, vd, ne),
             label = c("Correlação\ncanônica (CCorA)", "RDA, CCA", "Co-inércia\n(CoIA)")) +
    annotate("text", x = c(.5, 4.2, 8.2), y = -1.25, size = 4.6, lineheight = .9, colour = ne,
             label = c("var(XQu) = 1\nvar(YRv) = 1", "var(XQu) = 1", "sem restrição")) +
    annotate("text", x = c(.5, 4.2, 8.2), y = -1.95, size = 4.2, lineheight = .9,
             colour = cores[["neutro"]], fontface = "italic",
             label = c("maximiza só a correlação", "a variância de Y\nexplicada por X",
                       "a covariância")) +
    annotate("segment", x = c(2.3, 6.1), xend = c(2.3, 6.1), y = -2.2, yend = 2.1,
             colour = cores[["neutro"]], linewidth = .4) +
    geom_blank(data = tibble(x = c(-.6, 10), y = c(-2.4, 2.9)), aes(x, y))
}

#' Análises derivadas da co-inércia (redesenhado de Dray, Chessel &
#' Thioulouse 2003)
fig_coinercia_derivados <- function() {
  ne <- cores[["accent_dark"]]
  m1 <- function(x, y, r, w = .9, h = 1.1, cor = ne) mutate(mat(x, y, r, w, h, cor, tam = 1.1), fill = "white")
  cx <- bind_rows(
    # co-inércia
    m1(0, 4.4, "Y"), m1(1.7, 4.4, "X"),
    # RLQ: L (locais × espécies), R (locais × ambiente), Q (atributos × espécies)
    m1(4.9, 4.2, "L"), m1(6.6, 4.2, "R"), m1(4.9, 5.75, "Q", h = .7),
    # co-inércia múltipla
    pilha(0.2, 1.7, "Yᵢ"),
    # análise de concordância
    pilha(4.7, 1.7, "Yᵢ"), m1(6.6, 1.7, "X"),
    # within/between
    m1(0, -1.1, "Y"), m1(1.7, -1.1, "X"),
    # STATICO
    pilha(4.7, -1.1, "Yᵢ"), pilha(6.6, -1.1, "Xᵢ"))
  rot <- tibble(x = c(.85, 5.75, .85, 5.65, .85, 5.65), y = c(5.35, 6.5, 2.85, 2.85, .15, .15),
                l = c("Co-inércia", "RLQ", "Co-inércia múltipla", "Análise de concordância",
                      "Within & between class", "STATICO"))
  diagrama(cx, base = 5) +
    seta2(.5, 4.4, 1.2, 4.4) + seta2(5.4, 4.2, 6.1, 4.2) + seta2(4.9, 4.8, 4.9, 5.35) +
    seta2(5.2, 1.7, 6.1, 1.7) + seta2(.5, -1.1, 1.2, -1.1) + seta2(5.2, -1.1, 6.1, -1.1) +
    annotate("segment", x = -.45, xend = 2.15, y = c(-1.3, -.9), yend = c(-1.3, -.9),
             colour = ne, linetype = "22", linewidth = .4) +
    annotate("text", x = c(4.9, 6.6, 5.4), y = c(3.5, 3.5, 5.75), size = 3.8, colour = cores[["neutro"]],
             label = c("locais ×\nespécies", "locais ×\nambiente", "atributos ×\nespécies"), hjust = c(.5, .5, 0),
             lineheight = .85) +
    annotate("text", x = .9, y = 1.7, size = 4, colour = cores[["neutro"]], label = "várias\ntabelas",
             hjust = 0, lineheight = .85) +
    annotate("text", x = 7.3, y = -1.1, size = 4, colour = cores[["neutro"]], label = "no tempo",
             hjust = 0) +
    geom_text(data = rot, aes(x, y, label = l), size = 5.2, fontface = "bold", colour = vd) +
    geom_blank(data = tibble(x = c(-.8, 9.2), y = c(-2, 6.8)), aes(x, y))
}


#' Matriz de confusão: grupos observados (linhas) × atribuídos (colunas); a
#' diagonal são os acertos
fig_confusao <- function(tab) {
  d <- as.data.frame(tab) |> setNames(c("obs", "atr", "n")) |>
    mutate(acerto = obs == atr, obs = factor(obs, rev(levels(obs))))
  ggplot(d, aes(atr, obs)) +
    geom_tile(aes(fill = ifelse(n == 0, "zero", ifelse(acerto, "acerto", "erro"))),
              colour = "white", linewidth = 1.5) +
    geom_text(aes(label = n), size = 9, fontface = "bold",
              colour = ifelse(d$n == 0, cores[["neutro"]], "white")) +
    scale_fill_manual(values = c(acerto = vd, erro = "#c0392b", zero = cores[["neutro_cl"]]),
                      labels = c(acerto = "acerto (diagonal)", erro = "erro", zero = "nenhum"),
                      name = NULL) +
    scale_x_discrete(position = "top") +
    coord_equal() +
    labs(x = "Grupo atribuído pela LDA", y = "Grupo observado") +
    tema_aula(base = 17) +
    theme(panel.grid = element_blank(), legend.position = "bottom")
}
