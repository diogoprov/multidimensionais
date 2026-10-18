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
fig_lda_ideia <- function() {
  set.seed(12)
  n <- 30
  a <- cbind(rnorm(n, 4, 1.1), rnorm(n, 0, .45)); a[, 2] <- a[, 2] + a[, 1] - 4
  b <- cbind(rnorm(n, 4, 1.1), rnorm(n, 0, .45)); b[, 2] <- b[, 2] + b[, 1] - 2.6
  d <- tibble(x1 = c(a[, 1], b[, 1]), x2 = c(a[, 2], b[, 2]) + 4, g = rep(c("A", "B"), each = n))
  ld <- lda_manual(d[, 1:2], d$g)
  d$ld1 <- ld$escores[, 1]
  pal <- c(A = cores[["accent"]], B = cores[["ferrugem"]])
  p1 <- ggplot(d, aes(x1, x2, fill = g)) +
    geom_point(shape = 21, colour = "white", size = 2.8) +
    scale_fill_manual(values = pal, name = NULL) +
    coord_equal() + labs(title = "Os grupos se sobrepõem em x1 e em x2") +
    tema_aula(base = 13)
  p2 <- ggplot(d, aes(x1, fill = g)) + geom_density(alpha = .55, colour = NA) +
    scale_fill_manual(values = pal, guide = "none") + labs(title = "x1", y = NULL) +
    tema_aula(base = 13) + theme(axis.text.y = element_blank())
  p3 <- ggplot(d, aes(ld1, fill = g)) + geom_density(alpha = .55, colour = NA) +
    scale_fill_manual(values = pal, guide = "none") + labs(title = "Eixo discriminante (LD1)",
                                                             x = "LD1", y = NULL) +
    tema_aula(base = 13) + theme(axis.text.y = element_blank())
  p1 + (p2 / p3) + plot_layout(widths = c(1.2, 1))
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
