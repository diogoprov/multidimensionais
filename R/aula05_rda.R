library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

vd <- cores_familia[["Ordenação restrita"]]

# ---- esquemas ---------------------------------------------------------------

# caixa de matriz para os diagramas (rótulo grande, sem moldura arredondada)
mat <- function(x, y, rotulo, w = 1.2, h = 1.5, cor = cores[["accent_dark"]], tam = 1.6)
  tibble(x, y, w, h, rotulo, cor, tam, face = "bold")

#' Análise indireta × direta de gradientes (inspirado na Fig. 10.4 de
#' Legendre & Legendre 2012)
fig_indireta_direta <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(
    mat(0, 2, "Y"), mat(2.2, 2, "Eixos de\nordenação", w = 1.6, tam = .9),
    mat(5.6, 2, "X"),
    mat(0, -.3, "Y", cor = vd), mat(5.6, -.3, "X", cor = vd))
  setas <- tribble(
    ~x,   ~y,   ~xend, ~yend,
    .65,  2,    1.35,  2,
    3.05, 2,    4.95,  2,
    .65,  -.3,  4.95,  -.3)
  diagrama(cx, setas, base = 5) +
    annotate("text", x = 4, y = 2.35, label = "comparação indireta\n(correlação, regressão)",
             size = 4.2, colour = ne, lineheight = .9) +
    annotate("text", x = 2.8, y = 0, label = "comparação direta: análise canônica",
             size = 4.4, colour = vd, fontface = "bold") +
    annotate("text", x = -1.1, y = c(2, -.3), label = c("Indireta", "Direta"), angle = 90,
             size = 5, fontface = "bold", colour = c(ne, vd))
}

#' Análises canônicas assimétricas e simétricas
fig_assim_sim <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(mat(0, 1.6, "Y", w = 1.1, h = 1.1), mat(3.6, 1.6, "X", w = 1.1, h = 1.1),
                  mat(0, -.4, "Y", w = 1.1, h = 1.1), mat(3.6, -.4, "X", w = 1.1, h = 1.1))
  setas <- tribble(~x, ~y, ~xend, ~yend, 3.0, 1.6, .6, 1.6)
  diagrama(cx, setas, base = 5) +
    annotate("segment", x = .6, xend = 3.0, y = -.4, yend = -.4, linewidth = .9,
             colour = ne, arrow = arrow(length = unit(.18, "cm"), ends = "both", type = "closed")) +
    annotate("text", x = 1.8, y = 1.85, label = "X explica Y", size = 4.2, colour = vd,
             fontface = "bold") +
    annotate("text", x = 1.8, y = -.15, label = "o que há em comum", size = 4.2, colour = ne,
             fontface = "bold") +
    annotate("text", x = 4.4, y = 1.6, hjust = 0, size = 4.4, lineheight = .95, colour = vd,
             label = "Assimétricas\nY com todas as variáveis: RDA, CCA\nY com uma classificação: LDA") +
    annotate("text", x = 4.4, y = -.4, hjust = 0, size = 4.2, lineheight = .95, colour = ne,
             label = "Simétricas\ncorrelação canônica, co-inércia,\nProcrustes") +
    geom_blank(data = tibble(x = 8.6, y = 0), aes(x, y))
}

#' Regressão múltipla (Y univariada) × RDA (Y multivariada)
fig_regressao_rda <- function() {
  cx <- bind_rows(mat(0, 1.5, "y", w = .5, h = 1.3), mat(2.2, 1.5, "X", w = 1.6, h = 1.3),
                  mat(0, -.5, "Y", w = 1.6, h = 1.3, cor = vd), mat(2.6, -.5, "X", w = 1.6, h = 1.3, cor = vd))
  diagrama(cx, base = 5) +
    annotate("text", x = c(.95, 1.2), y = c(1.5, -.5), label = "~", size = 14,
             colour = "#c0392b", fontface = "bold") +
    annotate("text", x = 3.4, y = 1.5, hjust = 0, size = 4.6,
             label = "Regressão múltipla:\nresposta univariada (vetor)") +
    annotate("text", x = 3.8, y = -.5, hjust = 0, size = 4.6, colour = vd,
             label = "RDA:\nresposta multivariada (matriz)")
}

#' A RDA em dois passos (inspirado na Fig. 11.2 de Legendre & Legendre 2012)
fig_rda_passos <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                              ~cor, ~face,
    0,    1.6,   2.2, 1,   "Y\n(variáveis centradas)",                          ne,   "bold",
    2.6,  1.6,   2.2, 1,   "X\n(variáveis centradas)",                          ne,   "bold",
    1.3,  0,     2.6, 1,   "Valores ajustados\nŶ = X[X'X]⁻¹X'Y",                vd,   "plain",
    5,    0,     2.2, 1,   "PCA de Ŷ:\nautovetores U",                          vd,   "plain",
    8.4,  1.6,   2.6, 1,   "F = YU\nescores dos locais\n(espaço de Y)",          vd,   "plain",
    8.4,  0,     2.6, 1,   "Z = ŶU\nescores ajustados\n(espaço de X)",           vd,   "plain",
    1.3,  -1.6,  2.6, 1,   "Resíduos\nYres = Y − Ŷ",                            ne,   "plain",
    5,    -1.6,  2.2, 1,   "PCA dos resíduos",                                  ne,   "plain"
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.3,   1.1,   1.3,   .55,
    2.6,   1.1,   1.9,   .55,
    2.6,   0,     3.85,  0,
    6.1,   .2,    7.05,  1.5,
    6.1,   0,     7.05,  0,
    1.3,   -.5,   1.3,   -1.05,
    2.6,   -1.6,  3.85,  -1.6
  )
  diagrama(cx, setas, base = 4.2) +
    annotate("text", x = -.3, y = .75, label = "Passo 1:\nregressão de cada\ncoluna de Y em X",
             size = 3.8, hjust = 1, colour = ne, lineheight = .9) +
    annotate("text", x = 5, y = .75, label = "Passo 2", size = 4.2, colour = vd, fontface = "bold")
}

#' Valores ajustados de uma regressão múltipla: o plano que melhor se ajusta
#' aos pontos num espaço de duas preditoras (dados simulados, vista 2D)
fig_ajustados <- function() {
  set.seed(4)
  n <- 40
  x <- runif(n, 0, 10)
  y <- 2 + .7 * x + rnorm(n, 0, 1.4)
  fit <- lm(y ~ x)
  d <- tibble(x, y, yhat = fitted(fit))
  ggplot(d, aes(x, y)) +
    geom_segment(aes(xend = x, yend = yhat), colour = cores[["neutro"]], linetype = "22") +
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], colour = "#c0392b", linewidth = 1.1) +
    geom_point(shape = 21, fill = cores[["accent"]], colour = "white", size = 3) +
    geom_point(aes(y = yhat), shape = 21, fill = "#c0392b", colour = "white", size = 2.4) +
    annotate("text", x = 9.6, y = 2.2, hjust = 1, size = 4.6, colour = "#c0392b",
             label = "valores ajustados Ŷ (sobre a reta)") +
    labs(x = expression(X[1]), y = expression(Y[1]),
         subtitle = "Com muitas preditoras, a reta vira um hiperplano") +
    tema_aula(base = 15)
}

# ---- Doubs ------------------------------------------------------------------

#' Peixes do Doubs (Hellinger) e variáveis ambientais padronizadas, sem o local
#' 8 e sem dfs (distância da nascente), como em Borcard et al. (2018)
doubs_rda_dados <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  spe <- e$doubs$fish[-8, ]
  env <- as.data.frame(scale(e$doubs$env[-8, -1]))
  list(spe = spe, spe.h = vegan::decostand(spe, "hellinger"), env = env)
}

doubs_rda <- function() {
  d <- doubs_rda_dados()
  vegan::rda(d$spe.h ~ ., d$env)
}

doubs_cca <- function() {
  d <- doubs_rda_dados()
  vegan::cca(d$spe ~ ., d$env)
}

#' Triplot de uma ordenação restrita do vegan
#'
#' @param ord rda() ou cca()
#' @param scaling 1 ou 2
#' @param ref espécie de referência para o sinal dos eixos
#' @param n_sp espécies mostradas (as de maior escore nos dois eixos)
#' @param projecao local cuja projeção sobre a seta de `variavel` é desenhada
fig_triplot <- function(ord, scaling = 2, ref = "Satr", n_sp = 9, projecao = NULL,
                        variavel = "oxy", base = 13) {
  e <- escores(ord, ref = ref, scaling = scaling)
  si <- e$sites; sp <- e$species; bp <- e$biplot
  nm <- names(si)[2:3]
  names(si)[2:3] <- names(sp)[2:3] <- names(bp)[2:3] <- c("e1", "e2")
  sp <- sp |> mutate(r = sqrt(e1^2 + e2^2)) |> arrange(desc(r)) |> head(n_sp)
  k <- .9 * max(abs(c(si$e1, si$e2, sp$e1, sp$e2))) / max(abs(c(bp$e1, bp$e2)))
  bp <- bp |> mutate(e1 = e1 * k, e2 = e2 * k)
  amp <- diff(range(c(si$e2, bp$e2, sp$e2)))
  bp <- bp |> mutate(lx = e1 * 1.1, ly = separa_rotulos(lx, e2 * 1.1, .2 * amp, .06 * amp))
  g <- ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_text(data = si, aes(e1, e2, label = rotulo), colour = cores[["neutro"]], size = 3.4) +
    geom_segment(data = bp, aes(0, 0, xend = e1, yend = e2), colour = vd, linewidth = .9,
                 arrow = arrow(length = unit(.16, "cm"))) +
    geom_text(data = bp, aes(lx, ly, label = rotulo), colour = vd, fontface = "bold", size = 4.2) +
    geom_text(data = sp, aes(e1, e2, label = rotulo), colour = "#c0392b", fontface = "italic",
              size = 3.8)
  if (!is.null(projecao)) {
    v <- unlist(bp[bp$rotulo == variavel, c("e1", "e2")]); v <- v / sqrt(sum(v^2))
    s <- unlist(sp[sp$rotulo == projecao, c("e1", "e2")]); t <- sum(s * v)
    g <- g + annotate("segment", x = s[1], y = s[2], xend = t * v[1], yend = t * v[2],
                      colour = cores[["ocre"]], linetype = "22", linewidth = 1) +
      annotate("point", x = t * v[1], y = t * v[2], colour = cores[["ocre"]], size = 3)
  }
  g + coord_equal() +
    labs(x = nm[1], y = nm[2], subtitle = paste("Scaling", scaling)) +
    tema_aula(base = base)
}

#' Gradiente curto (RDA) × longo (CCA): a resposta unimodal de uma espécie e
#' a aproximação linear num trecho curto e num trecho longo do gradiente
fig_gradiente <- function() {
  x <- seq(0, 10, length.out = 300)
  y <- exp(-(x - 5)^2 / 2)
  painel <- function(a, b, titulo) {
    sel <- x >= a & x <= b
    fit <- lm(y[sel] ~ x[sel])
    ggplot() +
      geom_line(aes(x, y), linewidth = 1.1, colour = cores[["texto"]]) +
      annotate("segment", x = a, xend = b, y = coef(fit)[1] + coef(fit)[2] * a,
               yend = coef(fit)[1] + coef(fit)[2] * b, colour = cores[["ocre"]], linewidth = 1.6) +
      annotate("segment", x = a, xend = b, y = -.06, yend = -.06, colour = "#c0392b",
               linewidth = 2) +
      labs(title = titulo, x = "Gradiente ambiental", y = "Abundância") +
      tema_aula(base = 14, conceitual = TRUE)
  }
  painel(5.2, 7.2, "Gradiente curto: RDA") + painel(1, 9, "Gradiente longo: CCA")
}

#' Escolha da CCA (inspirado na Fig. 21.1 de McCune & Grace 2002)
fig_escolha_cca <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                                         ~cor, ~face,
    0,    1,     3,   1.2, "O interesse é só a parte\nda estrutura da comunidade\nligada às variáveis medidas?", ne, "plain",
    4.3,  1,     3,   1.2, "Que relação as espécies\ntêm com as variáveis\nexplanatórias?",   ne,   "plain",
    7.6,  1.6,   1.8, .7,  "CCA",                                                           vd,   "bold",
    7.6,  .4,    1.8, .7,  "RDA",                                                           vd,   "bold",
    0,    -.8,   2.4, .7,  "Ordenação irrestrita",                                          ne,   "bold"
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.5,   1,     2.75,  1,
    5.8,   1.2,   6.65,  1.6,
    5.8,   .8,    6.65,  .4,
    0,     .4,    0,     -.4)
  diagrama(cx, setas, base = 4.2) +
    annotate("text", x = 2.1, y = 1.2, label = "sim", size = 4.2, colour = ne) +
    annotate("text", x = .25, y = 0, label = "não", size = 4.2, colour = ne, hjust = 0) +
    annotate("text", x = 6.2, y = c(1.62, .32), label = c("unimodal", "linear"), size = 4,
             colour = ne)
}
