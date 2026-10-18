library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/tema.R"))

roxo <- cores_familia[["Associação"]]

tema_vazio <- function() {
  theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          plot.title = element_text(face = "bold", colour = cores[["texto"]], size = 14))
}

#' Modos Q e R: da matriz objetos × descritores às matrizes de associação
fig_modos_rq <- function() {
  matriz <- function(x0, y0, l, a, rotulo = NULL, diag = FALSE) {
    g <- list(annotate("rect", xmin = x0, xmax = x0 + l, ymin = y0, ymax = y0 + a,
                       fill = cores[["neutro_cl"]], colour = cores[["texto"]], linewidth = .8))
    if (diag) g <- c(g, list(annotate("segment", x = x0, y = y0 + a, xend = x0 + l, yend = y0,
                                      colour = cores[["texto"]])))
    if (!is.null(rotulo)) g <- c(g, list(annotate("text", x = x0 + l / 2, y = y0 + a / 2,
                                                  label = rotulo, fontface = "bold", size = 16,
                                                  family = "serif", colour = roxo)))
    g
  }
  txt <- function(x, y, l, ang = 0) annotate("text", x = x, y = y, label = l, angle = ang,
                                             fontface = "bold", size = 5, colour = cores[["texto"]])
  seta <- arrow(length = unit(.3, "cm"), type = "closed")
  ggplot() +
    matriz(0, 2.6, 1.6, 2) +
    txt(.8, 4.85, "Descritores") + txt(-.25, 3.6, "Objetos", 90) +
    matriz(3.1, 2.6, 2, 2, "Q", TRUE) +
    txt(4.1, 4.85, "Objetos") + txt(2.85, 3.6, "Objetos", 90) +
    matriz(0, -.2, 2, 2, "R", TRUE) +
    txt(1, 2.05, "Descritores") + txt(-.25, .8, "Descritores", 90) +
    annotate("segment", x = 1.75, y = 3.6, xend = 2.6, yend = 3.6, linewidth = 1.6,
             colour = cores[["accent_dark"]], arrow = seta) +
    annotate("segment", x = .8, y = 2.45, xend = .8, yend = 2.15, linewidth = 1.6,
             colour = cores[["accent_dark"]], arrow = seta) +
    coord_equal(xlim = c(-.5, 5.3), ylim = c(-.3, 5.1)) +
    tema_vazio()
}

#' Desigualdade triangular
fig_triangulo <- function() {
  v <- tibble(x = c(0, 3, 4), y = c(0, 1.4, 0), r = c("a", "b", "c"))
  ggplot(v, aes(x, y)) +
    geom_polygon(fill = NA, colour = roxo, linewidth = 1.1) +
    geom_point(size = 3, colour = roxo) +
    geom_text(aes(label = r), nudge_y = c(-.2, .22, -.2), size = 6, fontface = "italic") +
    annotate("text", x = 1.3, y = .95, label = "D(a,b)", size = 5) +
    annotate("text", x = 3.85, y = .8, label = "D(b,c)", size = 5) +
    annotate("text", x = 2, y = -.22, label = "D(a,c)", size = 5) +
    coord_equal(xlim = c(-.3, 4.5), ylim = c(-.4, 1.7)) +
    tema_vazio()
}

#' Distância euclidiana entre quatro árvores medidas em dois ambientes
#'
#' @param triangulo desenha o triângulo retângulo do cálculo de d13
fig_euclid_arvores <- function(triangulo = FALSE) {
  p <- tibble(id = 1:4, mata = c(5.6, 6.2, 7, 8), campo = c(9, 9.8, 2.7, 2.3),
              cor = c(cores[["accent"]], cores[["accent"]], cores[["verde"]], cores[["verde"]]))
  seg <- tibble(a = c(1, 1, 3), b = c(2, 3, 4), rot = c("d12", "d13", "d34"),
                dx = c(-.3, .3, .1), dy = c(.35, .2, .35),
                cor = c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]])) |>
    mutate(x = p$mata[a], y = p$campo[a], xend = p$mata[b], yend = p$campo[b])
  g <- ggplot() +
    geom_segment(data = p, aes(mata, campo, xend = mata, yend = 0), linetype = "22",
                 colour = cores[["neutro"]], linewidth = .3) +
    geom_segment(data = p, aes(mata, campo, xend = 4.5, yend = campo), linetype = "22",
                 colour = cores[["neutro"]], linewidth = .3)
  if (triangulo) {
    g <- g +
      annotate("polygon", x = c(5.6, 5.6, 7), y = c(9, 2.7, 2.7), fill = cores[["neutro_cl"]],
               colour = cores[["texto"]]) +
      annotate("text", x = 5.5, y = 5.85, label = "9 − 2,7", hjust = 1, size = 5) +
      annotate("text", x = 6.3, y = 2.25, label = "7 − 5,6", size = 5)
  }
  g +
    geom_segment(data = seg, aes(x, y, xend = xend, yend = yend), colour = seg$cor,
                 linewidth = 1.1) +
    geom_text(data = seg, aes((x + xend) / 2 + dx, (y + yend) / 2 + dy, label = rot),
              colour = seg$cor, fontface = "bold.italic", size = 5) +
    geom_point(data = p, aes(mata, campo), shape = 21, fill = p$cor, colour = cores[["texto"]],
               size = 6, stroke = .8) +
    geom_text(data = p, aes(mata, campo, label = id), colour = "white", fontface = "bold",
              size = 3.6) +
    scale_x_continuous(breaks = c(5.6, 6.2, 7, 8), labels = c("5,6", "6,2", "7", "8")) +
    scale_y_continuous(breaks = c(2.3, 2.7, 9, 9.8), labels = c("2,3", "2,7", "9", "9,8")) +
    coord_cartesian(xlim = c(4.5, 8.6), ylim = c(0, 10.5), expand = FALSE) +
    labs(x = "Altura do tronco (mata)", y = "Altura do tronco (campo)") +
    tema_aula(base = 15) + theme(panel.grid = element_blank(),
                                 axis.line = element_line(colour = cores[["texto"]]))
}

#' Distância euclidiana entre duas plantas em duas variáveis morfológicas
#' (dados simulados, inspirado na Fig. 12.2 de Gotelli & Ellison 2011)
fig_euclid_plantas <- function() {
  p <- tibble(planta = c("i", "j"), altura = c(420, 640), envergadura = c(59.6, 55.3))
  ggplot(p, aes(altura, envergadura)) +
    geom_segment(aes(x = altura[1], y = envergadura[1], xend = altura[2], yend = envergadura[2]),
                 colour = cores[["verde"]], linewidth = 1.1) +
    geom_point(size = 3.5, colour = cores[["texto"]]) +
    geom_text(aes(label = paste("planta", planta)), nudge_y = .35, size = 4.6) +
    scale_y_continuous(expand = expansion(mult = .12)) +
    annotate("text", x = 560, y = 58.2, size = 5, colour = cores[["verde"]],
             label = "d[ij] == sqrt((y[i1] - y[j1])^2 + (y[i2] - y[j2])^2)", parse = TRUE) +
    labs(x = "Altura (mm)", y = "Envergadura do apêndice (mm)") +
    tema_aula(base = 15)
}

#' Distância euclidiana × de Manhattan (city-block) entre dois objetos
fig_manhattan <- function() {
  a <- c(1, 4); b <- c(5, 1)
  ggplot() +
    annotate("segment", x = a[1], y = a[2], xend = b[1], yend = b[2], colour = cores[["accent"]],
             linewidth = 1.3) +
    annotate("path", x = c(a[1], a[1], b[1]), y = c(a[2], b[2], b[2]), colour = cores[["ferrugem"]],
             linewidth = 1.3, linetype = "22") +
    annotate("point", x = c(a[1], b[1]), y = c(a[2], b[2]), size = 4, colour = cores[["texto"]]) +
    annotate("text", x = c(a[1], b[1]) + c(-.2, .2), y = c(a[2], b[2]) + .25, label = c("A", "B"),
             size = 6, fontface = "bold") +
    annotate("text", x = 3.3, y = 2.9, label = "euclidiana = 5", colour = cores[["accent"]],
             size = 5, angle = -30) +
    annotate("text", x = 3, y = .65, label = "Manhattan = 4 + 3 = 7", colour = cores[["ferrugem"]],
             size = 5) +
    coord_equal(xlim = c(0, 6), ylim = c(0, 5)) +
    labs(x = "Espécie 1", y = "Espécie 2") +
    tema_aula(base = 15)
}

#' Distância de chord: a distância euclidiana entre os objetos depois de
#' normalizados para comprimento 1 (projetados no círculo unitário)
fig_chord <- function() {
  a <- c(2, 9); b <- c(6, 1.5)              # abundâncias brutas
  na <- a / sqrt(sum(a^2)); nb <- b / sqrt(sum(b^2))
  arco <- tibble(t = seq(0, pi / 2, length.out = 100), x = cos(t), y = sin(t))
  ta <- atan2(na[2], na[1]); tb <- atan2(nb[2], nb[1])
  arco_ab <- tibble(t = seq(tb, ta, length.out = 50), x = cos(t), y = sin(t))
  ggplot() +
    geom_path(data = arco, aes(x, y), colour = cores[["neutro"]]) +
    annotate("segment", x = 0, y = 0, xend = c(na[1], nb[1]), yend = c(na[2], nb[2]),
             linetype = "22", colour = cores[["neutro"]]) +
    geom_path(data = arco_ab, aes(x, y), colour = cores[["ocre"]], linewidth = 1.1) +
    annotate("segment", x = na[1], y = na[2], xend = nb[1], yend = nb[2], colour = roxo,
             linewidth = 1.3) +
    annotate("point", x = c(na[1], nb[1]), y = c(na[2], nb[2]), size = 3.5) +
    annotate("text", x = c(na[1], nb[1]) + c(0, .06), y = c(na[2], nb[2]) + c(.07, .03),
             label = c("i", "h"), size = 6, fontface = "italic") +
    annotate("text", x = .5, y = .52, label = "chord", colour = roxo, size = 5, fontface = "bold") +
    annotate("text", x = .86, y = .78, label = "arco", colour = cores[["ocre"]], size = 5) +
    coord_equal(xlim = c(0, 1.08), ylim = c(0, 1.08)) +
    labs(x = "Espécie 1 (normalizada)", y = "Espécie 2 (normalizada)") +
    tema_aula(base = 15)
}

#' Distância de Mahalanobis: dois pontos à mesma distância euclidiana do
#' centro de uma nuvem correlacionada, mas a distâncias de Mahalanobis
#' muito diferentes (dados simulados)
fig_mahalanobis <- function() {
  set.seed(5)
  n <- 300
  x <- rnorm(n); y <- .85 * x + rnorm(n, 0, .45)
  d <- tibble(x, y)
  S <- cov(cbind(x, y)); m <- colMeans(cbind(x, y))
  r <- 1.8
  pts <- tibble(rot = c("A", "B"), x = m[1] + r * cos(c(pi / 4, 3 * pi / 4)),
                y = m[2] + r * sin(c(pi / 4, 3 * pi / 4)))
  pts$maha <- sqrt(stats::mahalanobis(cbind(pts$x, pts$y), m, S))
  elip <- lapply(1:3, \(k) {
    t <- seq(0, 2 * pi, length.out = 200)
    e <- eigen(S)
    xy <- t(m + e$vectors %*% diag(sqrt(e$values) * k) %*% rbind(cos(t), sin(t)))
    tibble(x = xy[, 1], y = xy[, 2], k = k)
  }) |> bind_rows()
  ggplot() +
    geom_point(data = d, aes(x, y), colour = cores[["neutro"]], alpha = .45, size = 1.2) +
    geom_path(data = elip, aes(x, y, group = k), colour = roxo, linewidth = .6) +
    annotate("segment", x = m[1], y = m[2], xend = pts$x, yend = pts$y,
             colour = cores[["texto"]], linewidth = .9) +
    geom_point(data = pts, aes(x, y), size = 4, colour = cores[["ferrugem"]]) +
    geom_text(data = pts, aes(x, y, label = paste0(rot, ": Mahalanobis = ",
                                                   format(round(maha, 1), decimal.mark = ","))),
              vjust = -1, hjust = c(0, 1), size = 4.6, colour = cores[["ferrugem"]],
              fontface = "bold") +
    annotate("point", x = m[1], y = m[2], size = 3, shape = 3, stroke = 1.5) +
    coord_equal(xlim = c(-4.6, 4.6)) +
    labs(x = "Descritor 1", y = "Descritor 2",
         subtitle = paste0("Distância euclidiana ao centro: ",
                           format(r, decimal.mark = ","), " para A e para B")) +
    tema_aula(base = 14)
}
