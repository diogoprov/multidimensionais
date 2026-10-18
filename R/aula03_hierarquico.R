library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source(here::here("R/dendro.R"))
source(here::here("R/diagrama.R"))

lar <- cores_familia[["Classificação"]]

# ---- exemplo das lagoas (Legendre & Legendre 2012, seção 8.2) -------------

#' Abundância de 8 espécies em 5 lagoas
lagoas_especies <- function() {
  m <- matrix(c(3, 0, 0, 0, 4, 0, 0, 3,
                3, 0, 2, 0, 4, 2, 0, 3,
                0, 2, 3, 4, 0, 0, 0, 0,
                0, 2, 0, 3, 0, 3, 1, 0,
                0, 0, 2, 3, 0, 3, 2, 0), 8,
              dimnames = list(1:8, c("212", "214", "233", "431", "432")))
  t(m)
}

#' Distâncias D = 1 - S20 entre as lagoas (Legendre & Legendre 2012)
lagoas_dist <- function() {
  lag <- c("212", "214", "233", "431", "432")
  d <- matrix(0, 5, 5, dimnames = list(lag, lag))
  d[lower.tri(d)] <- c(.4, 1, 1, 1, .929, .937, .786, .7, .8, .5)
  stats::as.dist(d)
}

#' Matriz de distâncias como tabela, com as células já usadas destacadas
#'
#' @param k número de pares (em ordem crescente de distância) destacados
tab_lagoas <- function(k = 0) {
  d <- as.matrix(lagoas_dist())
  pares <- which(lower.tri(d), arr.ind = TRUE)
  v <- d[pares]
  ord <- order(v, pares[, 2])
  txt <- matrix("", 5, 5, dimnames = dimnames(d))
  diag(txt) <- "–"
  for (i in seq_len(nrow(pares))) {
    r <- pares[i, 1]; c <- pares[i, 2]
    s <- format(d[r, c], nsmall = 3, decimal.mark = ",")
    txt[r, c] <- if (which(ord == i) <= k) paste0("**[", s, "]{style='color:#c0392b'}**") else s
  }
  txt[upper.tri(txt)] <- ""
  # a última coluna fica vazia no triângulo inferior
  as.data.frame(txt[, -5])
}

#' Pares de lagoas em ordem crescente de distância
tab_pares <- function() {
  d <- as.matrix(lagoas_dist())
  pares <- which(lower.tri(d), arr.ind = TRUE)
  tibble(D = d[pares], par = paste(rownames(d)[pares[, 2]], rownames(d)[pares[, 1]], sep = "–")) |>
    arrange(D) |>
    mutate(D = format(D, nsmall = 3, decimal.mark = ","))
}

#' Single linkage passo a passo: o dendrograma até um nível de distância
fig_single_passo <- function(ate) {
  hc <- stats::hclust(lagoas_dist(), "single")
  fig_dendro(hc, ate = ate, corte = ate, xmax = 1, base = 15) +
    annotate("text", x = ate, y = .4, label = paste0("D = ", format(ate, decimal.mark = ",")),
             colour = "#c0392b", fontface = "bold", size = 4.8)
}

#' Single linkage × UPGMA nas lagoas
fig_single_upgma <- function() {
  d <- lagoas_dist()
  s <- stats::hclust(d, "single"); u <- stats::hclust(d, "average")
  ms <- max(s$height); mu <- max(u$height)
  p1 <- fig_dendro(s, titulo = "Single linkage", xmax = 1.02, base = 15) +
    annotate("text", x = ms, y = .3, label = format(round(ms, 3), decimal.mark = ","),
             colour = "#c0392b", fontface = "bold", size = 4.6)
  p2 <- fig_dendro(u, titulo = "UPGMA", xmax = 1.02, base = 15) +
    annotate("text", x = mu, y = .3, label = format(round(mu, 3), decimal.mark = ","),
             colour = "#c0392b", fontface = "bold", size = 4.6)
  p1 + p2
}

#' Ward nas lagoas: soma de quadrados dentro dos grupos a cada passo
#' (recalcula a Tabela 8.8 de Legendre & Legendre 2012)
tab_ward <- function() {
  d <- lagoas_dist()
  hc <- stats::hclust(d^2, "ward.D")       # alturas = D² mínimo = 2 ΔE
  de <- hc$height / 2
  ss_total <- sum(de)
  tibble(K = 5:1,
         `SS dentro` = c(0, cumsum(de)),
         `SS entre`  = ss_total - c(0, cumsum(de))) |>
    mutate(across(-K, \(v) format(round(v, 5), nsmall = 5, decimal.mark = ",")))
}

fig_ward_lagoas <- function() {
  hc <- stats::hclust(lagoas_dist()^2, "ward.D")
  fig_dendro(hc, xlab = "D² (Ward)", base = 15, xmax = 1.8)
}

# ---- dados do Doubs -------------------------------------------------------

#' Peixes do Doubs (sem o local 8) com a distância de chord
doubs_chord <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  fish <- e$doubs$fish[-8, ]
  stats::dist(vegan::decostand(fish, "normalize"))
}

#' Agrupamentos dos peixes do Doubs com quatro métodos
doubs_hc <- function() {
  d <- doubs_chord()
  list(`Single linkage` = stats::hclust(d, "single"),
       `Complete linkage` = stats::hclust(d, "complete"),
       UPGMA = stats::hclust(d, "average"),
       Ward = stats::hclust(d, "ward.D2"))
}

#' Dendrograma (Ward) do Doubs com um nível de corte e os grupos
fig_corte_doubs <- function(k = 4) {
  hc <- doubs_hc()$Ward
  g <- stats::cutree(hc, k)
  h <- sort(hc$height, decreasing = TRUE)
  corte <- mean(h[(k - 1):k])
  fig_dendro(hc, corte = corte, grupos = g, base = 12,
             xlab = "Distância de chord") +
    annotate("text", x = corte, y = 0, label = paste(k, "grupos"), hjust = -.1,
             colour = "#c0392b", fontface = "bold", size = 4.6)
}

#' Níveis de fusão: a altura de cada fusão contra o número de grupos
#' (Borcard et al. 2018, seção 4.7.3)
fig_fusao <- function() {
  hc <- doubs_hc()$Ward
  n <- length(hc$order)
  d <- tibble(grupos = (n - 1):1, altura = hc$height)
  ggplot(filter(d, grupos <= 15), aes(altura, grupos)) +
    geom_step(direction = "vh", colour = lar, linewidth = 1) +
    geom_point(colour = lar, size = 2.4) +
    geom_text(aes(label = grupos), hjust = -.6, size = 4, colour = cores[["texto"]]) +
    labs(x = "Nível de fusão (Ward, distância de chord)", y = "Número de grupos") +
    tema_aula(base = 15)
}

#' Correlação cofenética: distâncias originais × cofenéticas, quatro métodos
fig_cofenetica <- function() {
  d <- doubs_chord()
  hs <- doubs_hc()
  dd <- lapply(names(hs), \(n) {
    co <- stats::cophenetic(hs[[n]])
    tibble(metodo = paste0(n, "\nr = ", format(round(cor(d, co), 3), decimal.mark = ",")),
           orig = as.vector(d), cof = as.vector(co))
  }) |> bind_rows() |>
    mutate(metodo = factor(metodo, unique(metodo)))
  ggplot(dd, aes(orig, cof)) +
    geom_abline(colour = cores[["neutro"]]) +
    geom_point(colour = lar, alpha = .35, size = 1.2) +
    facet_wrap(~metodo, nrow = 1, scales = "free_y") +
    labs(x = "Distância de chord", y = "Distância cofenética") +
    tema_aula(base = 13)
}

cofenetica_r <- function() {
  d <- doubs_chord()
  sapply(doubs_hc(), \(h) cor(d, stats::cophenetic(h)))
}

#' Agrupamento flexível (Lance & Williams) com diferentes valores de beta
fig_flexivel <- function() {
  d <- doubs_chord()
  betas <- c(.98, .5, 0, -.25, -.5, -1)
  ps <- lapply(betas, \(b) {
    ag <- cluster::agnes(d, method = "flexible", par.method = (1 - b) / 2)
    hc <- stats::as.hclust(ag)
    hc$labels <- rep("", length(hc$labels))
    fig_dendro(hc, titulo = paste0("β = ", format(b, decimal.mark = ",")), base = 11,
               xlab = NULL) +
      theme(axis.text.x = element_blank(), plot.title = element_text(hjust = .5))
  })
  patchwork::wrap_plots(ps, nrow = 2)
}

#' Os tipos de análise de agrupamento
fig_tipos_agrupamento <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,    ~y,   ~w,   ~h,  ~rotulo,                       ~cor, ~face,
    5,     3,    4.2,  .7,  "Análise de agrupamento",      ne,   "bold",
    1.2,   1.7,  2.2,  .8,  "Particional\n(não hierárquica)", ne, "bold",
    6.8,   1.7,  2.8,  .8,  "Hierárquica\naglomerativa",   ne,   "bold",
    1.2,   .3,   1.6,  .7,  "k-means",                     lar,  "plain",
    3.6,   .3,   1.6,  .9,  "Single\nlinkage",             lar,  "plain",
    5.4,   .3,   1.6,  .9,  "Complete\nlinkage",           lar,  "plain",
    7.2,   .3,   1.6,  .7,  "UPGMA",                       lar,  "plain",
    9,     .3,   1.6,  .7,  "Ward",                        lar,  "plain"
  )
  s <- function(i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                             xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)
  diagrama(cx, bind_rows(s(1, 2), s(1, 3), s(2, 4), s(3, 5), s(3, 6), s(3, 7), s(3, 8)),
           base = 4.6)
}

#' Do dado ao agrupamento: dois caminhos
fig_fluxo <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                       ~cor,
    0,    1.4,   2.4, 1,   "Matriz de dados\n(locais × espécies)",        ne,
    4.4,  1.4,   2.4, 1,   "Matriz de\ndissimilaridades",                 ne,
    8.8,  1.4,   2.4, 1,   "Dendrograma",                                 lar,
    4.4,  -.6,   2.4, 1,   "Grupos ao redor de\ncentroides",               lar
  )
  setas <- tribble(
    ~x,   ~y,    ~xend, ~yend,
    1.2,  1.4,   3.15,  1.4,
    5.6,  1.4,   7.55,  1.4,
    .6,   .9,    3.15,  -.6
  )
  diagrama(cx, setas, base = 4.6) +
    annotate("text", x = 2.2, y = 1.68, label = "coeficiente", size = 4.2, colour = ne) +
    annotate("text", x = 6.6, y = 1.68, label = "algoritmo de\nagrupamento", size = 4.2,
             colour = ne, lineheight = .9) +
    annotate("text", x = 1.5, y = -.25, label = "k-means", size = 4.4, colour = lar,
             fontface = "bold")
}

#' UPGMA e suas variações nos peixes do Doubs: os métodos de centroide podem
#' produzir reversões
fig_variacoes_upgma <- function() {
  d <- doubs_chord()
  ms <- c(UPGMA = "average", WPGMA = "mcquitty", UPGMC = "centroid", WPGMC = "median")
  ps <- lapply(names(ms), \(n) {
    dd <- if (ms[[n]] %in% c("centroid", "median")) d^2 else d
    hc <- stats::hclust(dd, ms[[n]])
    hc$labels <- rep("", length(hc$labels))
    rev_n <- sum(diff(hc$height) < 0)
    fig_dendro(hc, titulo = paste0(n, if (rev_n > 0) paste0(" (", rev_n, " reversões)") else ""),
               base = 11, xlab = NULL) +
      theme(axis.text.x = element_blank(), plot.title = element_text(hjust = .5))
  })
  patchwork::wrap_plots(ps, nrow = 1)
}

#' Como se calcula a correlação cofenética
fig_cofenetica_esquema <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                   ~cor,
    0,    1.4,   2.6, 1,   "Matriz de distâncias\noriginal",          ne,
    4.6,  1.4,   2.4, 1,   "Dendrograma",                             lar,
    4.6,  -.6,   2.6, 1,   "Matriz de distâncias\ncofenéticas",       lar,
    0,    -.6,   2.6, 1,   "Correlação de\nPearson",                  ne
  )
  setas <- tribble(
    ~x,   ~y,    ~xend, ~yend,
    1.3,  1.4,   3.35,  1.4,
    4.6,  .9,    4.6,   -.05,
    3.3,  -.6,   1.35,  -.6,
    0,    .9,    0,     -.05
  )
  diagrama(cx, setas, base = 4.6) +
    annotate("text", x = 2.35, y = 1.68, label = "algoritmo", size = 4.2, colour = ne) +
    annotate("text", x = 5, y = .42, label = "nível em que cada\npar se une", size = 4,
             colour = ne, hjust = 0, lineheight = .9)
}
