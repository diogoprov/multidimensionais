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
    ~x,    ~y,   ~w,   ~h,  ~rotulo,                          ~cor, ~face,
    5.2,   3,    4.4,  .7,  "Análise de agrupamento",         ne,   "bold",
    1.3,   1.7,  3,    .95, "Particional\n(não hierárquica)", ne,   "bold",
    7,     1.7,  3,    .95, "Hierárquica\naglomerativa",      ne,   "bold",
    1.3,   .2,   1.9,  .8,  "k-means",                        lar,  "plain",
    3.9,   .2,   1.9,  .95, "Single\nlinkage",                lar,  "plain",
    5.95,  .2,   1.9,  .95, "Complete\nlinkage",              lar,  "plain",
    8,     .2,   1.9,  .8,  "UPGMA",                          lar,  "plain",
    10.05, .2,   1.9,  .8,  "Ward",                           lar,  "plain"
  )
  s <- function(i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                             xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)
  diagrama(cx, bind_rows(s(1, 2), s(1, 3), s(2, 4), s(3, 5), s(3, 6), s(3, 7), s(3, 8)),
           base = 4)
}

#' Do dado ao agrupamento: matriz, matriz de distâncias, dendrograma e
#' k-means (redesenho do esquema do slide original)
fig_fluxo <- function() {
  ne <- cores[["accent_dark"]]
  vazio <- theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  # matriz locais × espécies
  m <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = cores[["accent"]], alpha = .8) +
    annotate("text", x = .5, y = 1.12, label = "Locais", size = 4.6) +
    annotate("text", x = -.12, y = .5, label = "Espécies", angle = 90, size = 4.6) +
    coord_equal(xlim = c(-.2, 1.1), ylim = c(-.05, 1.2)) + vazio
  # triângulo de distâncias
  tr <- ggplot() +
    annotate("polygon", x = c(0, 0, 1), y = c(1, 0, 0), fill = cores[["accent"]], alpha = .8) +
    annotate("text", x = .5, y = 1.12, label = "Locais", size = 4.6) +
    annotate("text", x = -.12, y = .5, label = "Locais", angle = 90, size = 4.6) +
    coord_equal(xlim = c(-.2, 1.1), ylim = c(-.05, 1.2)) + vazio
  # dendrograma das lagoas
  hc <- stats::hclust(lagoas_dist(), "average"); hc$labels <- LETTERS[1:5]
  dn <- fig_dendro(hc, base = 11, xlab = "Distância") +
    theme(axis.text.x = element_blank(), panel.grid = element_blank())
  # k-means: três nuvens
  set.seed(2)
  km <- tibble(x = c(rnorm(12, 1, .25), rnorm(12, 2.6, .25), rnorm(12, 1.6, .25)),
               y = c(rnorm(12, 1, .25), rnorm(12, 1.5, .25), rnorm(12, 2.6, .25)),
               g = rep(c("a", "b", "c"), each = 12))
  cen <- km |> group_by(g) |> summarise(x = mean(x), y = mean(y))
  kp <- ggplot(km, aes(x, y)) +
    geom_point(size = 1.4, colour = cores[["texto"]]) +
    geom_point(data = cen, shape = 21, size = 15, fill = NA, colour = lar, stroke = 1) +
    coord_equal(xlim = c(.2, 3.4), ylim = c(.2, 3.4)) +
    labs(x = "Descritor 1", y = "Descritor 2") +
    tema_aula(base = 11) + theme(axis.text = element_blank(), panel.grid = element_blank(),
                                 axis.line = element_line(colour = cores[["texto"]]))
  seta <- function(rot, ang = 0) ggplot() +
    annotate("segment", x = 0, xend = .8 * cos(ang), y = 0, yend = .8 * sin(ang), linewidth = 3,
             colour = cores[["verde"]], arrow = arrow(length = unit(.35, "cm"), type = "closed")) +
    annotate("text", x = .4 * cos(ang) + ifelse(abs(cos(ang)) < .1, .15, 0),
             y = .4 * sin(ang) + ifelse(abs(cos(ang)) < .1, 0, .3), label = rot, size = 4.4,
             colour = ne, lineheight = .9, hjust = ifelse(abs(cos(ang)) < .1, 0, .5)) +
    coord_cartesian(xlim = c(-.5, 1.3), ylim = c(-.9, .5)) + vazio
  vazio_p <- ggplot() + vazio
  (m | seta("coeficiente de\ndissimilaridade") | tr) /
    (seta("k-means", -pi / 2) | vazio_p | seta("algoritmo de\nagrupamento", -pi / 2)) /
    (kp | vazio_p | dn) +
    plot_layout(heights = c(1, .45, 1.1)) &
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
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

#' Como se calcula a correlação cofenética: a matriz de distâncias, o
#' dendrograma, a matriz cofenética lida no dendrograma e a correlação entre
#' as duas (exemplo das lagoas, UPGMA)
fig_cofenetica_esquema <- function() {
  ne <- cores[["accent_dark"]]
  vazio <- theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  d <- lagoas_dist()
  hc <- stats::hclust(d, "average")
  co <- stats::cophenetic(hc)
  tri <- function(m, titulo, cor) {
    mm <- as.matrix(m); n <- nrow(mm)
    g <- expand_grid(i = seq_len(n), j = seq_len(n)) |> filter(i > j) |>
      mutate(v = mm[cbind(i, j)])
    ggplot(g, aes(j, i)) +
      geom_tile(aes(fill = v), colour = "white") +
      geom_text(aes(label = format(round(v, 2), nsmall = 2, decimal.mark = ",")), size = 3.4,
                colour = "white", fontface = "bold") +
      scale_fill_gradient(low = cores[["neutro"]], high = cor, guide = "none") +
      scale_y_reverse() + coord_equal() +
      labs(title = titulo) + vazio +
      theme(plot.title = element_text(hjust = .5, size = 13, colour = ne, face = "bold"))
  }
  seta <- function(rot) ggplot() +
    annotate("segment", x = 0, xend = 1, y = 0, yend = 0, linewidth = 2.4, colour = cores[["verde"]],
             arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("text", x = .5, y = .35, label = rot, size = 3.9, colour = ne, lineheight = .9) +
    coord_cartesian(xlim = c(-.1, 1.1), ylim = c(-.6, .8)) + vazio
  dn <- fig_dendro(hc, base = 11, xlab = NULL) +
    theme(axis.text.x = element_blank(), panel.grid = element_blank())
  r <- cor(d, co)
  sc <- ggplot(tibble(x = as.vector(d), y = as.vector(co)), aes(x, y)) +
    geom_abline(colour = cores[["neutro"]], linetype = "22") +
    geom_point(shape = 21, fill = lar, colour = "white", size = 3.4) +
    labs(x = "Distância original", y = "Distância cofenética",
         title = paste0("Correlação de Pearson: r = ", format(round(r, 3), decimal.mark = ","))) +
    tema_aula(base = 12) + theme(plot.title = element_text(size = 13, colour = ne))
  (tri(d, "Distâncias originais", cores[["accent"]]) | seta("algoritmo de\nagrupamento") | dn |
     seta("distância lida\nno dendrograma") | tri(co, "Distâncias cofenéticas", lar)) /
    (plot_spacer() | sc | plot_spacer()) +
    plot_layout(heights = c(1, 1.1)) &
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' Bootstrap multiescala (pvclust) dos peixes do Doubs: Ward com distância
#' de chord. A semente fixa o resultado.
doubs_pvclust <- function(nboot = 1000) {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  spe.ch <- vegan::decostand(e$doubs$fish[-8, ], "normalize")
  set.seed(2026)
  pvclust::pvclust(t(spe.ch), method.hclust = "ward.D2", method.dist = "euclidean",
                   nboot = nboot, quiet = TRUE)
}

#' Dendrograma do pvclust em ggplot2, com os valores AU (vermelho) e BP
#' (verde) em cada nó e os grupos com AU > 0,95 destacados
fig_pvclust <- function(fit = doubs_pvclust(), alfa = .95) {
  hc <- fit$hclust
  d <- dendro_seg(hc)
  n <- length(hc$order)
  pos <- numeric(n); pos[hc$order] <- seq_len(n)
  ym <- numeric(n - 1)
  for (i in seq_len(n - 1)) {
    yy <- sapply(hc$merge[i, ], \(k) if (k < 0) pos[-k] else ym[k])
    ym[i] <- mean(yy)
  }
  nos <- tibble(x = hc$height, y = ym, au = round(100 * fit$edges$au),
                bp = round(100 * fit$edges$bp))
  # grupos com AU > alfa (os maiores, sem sobreposição), como pvrect()
  pp <- pvclust::pvpick(fit, alpha = alfa)$clusters
  co <- as.matrix(stats::cophenetic(hc))
  ret <- lapply(pp, \(cl) {
    yy <- pos[match(cl, hc$labels)]
    tibble(ymin = min(yy) - .45, ymax = max(yy) + .45, xmax = max(co[cl, cl]) + .12)
  }) |> bind_rows()
  xmax <- max(hc$height) * 1.12
  ggplot() +
    geom_rect(data = ret, aes(xmin = -.1 * max(hc$height), xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, colour = "#c0392b", linewidth = .7, linetype = "22") +
    geom_segment(data = d$seg, aes(x, y, xend = xend, yend = yend), colour = lar, linewidth = .9) +
    geom_text(data = d$folhas, aes(0, y, label = rotulo), hjust = 1.2, size = 4.6,
              colour = cores[["texto"]]) +
    geom_text(data = nos, aes(x, y, label = au), hjust = 1.1, vjust = -.35, size = 4.3,
              colour = "#c0392b", fontface = "bold") +
    geom_text(data = nos, aes(x, y, label = bp), hjust = -.1, vjust = -.35, size = 4.3,
              colour = cores[["verde"]], fontface = "bold") +
    scale_x_continuous(limits = c(-.12 * xmax, xmax), labels = function(x) format(x, decimal.mark = ",")) +
    scale_y_reverse() +
    labs(x = "Distância (Ward, chord)", y = NULL,
         subtitle = "AU (vermelho) e BP (verde), em %") +
    tema_aula(base = 16) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank())
}
