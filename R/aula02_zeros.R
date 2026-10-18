library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
source(here::here("R/diagrama.R"))

roxo <- cores_familia[["Associação"]]

tema_vazio <- function() {
  theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

circulo <- function(x0, y0, r, n = 200) {
  t <- seq(0, 2 * pi, length.out = n)
  tibble(x = x0 + r * cos(t), y = y0 + r * sin(t))
}

#' Duas comunidades em diagrama de Venn: espécies exclusivas, compartilhadas
#' e ausentes das duas
#'
#' @param letras mostra a notação a, b, c e d no lugar das espécies
fig_venn <- function(letras = FALSE) {
  c1 <- circulo(-1, 0, 2); c2 <- circulo(1.2, 0, 2)
  g <- ggplot() +
    geom_polygon(data = c2, aes(x, y), fill = cores[["neutro_cl"]], colour = cores[["texto"]],
                 linewidth = .8) +
    geom_polygon(data = c1, aes(x, y), fill = NA, colour = cores[["texto"]], linewidth = .8) +
    annotate("text", x = c(-1.6, 1.8), y = 2.35, label = c("Comunidade 1", "Comunidade 2"),
             fontface = "bold", size = 5, colour = cores[["texto"]])
  if (letras) {
    g <- g + annotate("text", x = c(-1.9, .1, 2.1, -3.3), y = c(0, 0, 0, -2.2),
                      label = c("b", "a", "c", "d"), size = 14, fontface = "bold.italic",
                      colour = c(cores[["accent"]], cores[["ferrugem"]], cores[["texto"]], roxo))
  } else {
    sp <- tribble(
      ~x,    ~y,    ~rot,
      -2,    .9,    "perereca",
      -1.9,  -.1,   "roedor",
      -1.9,  -1.1,  "sabiá",
      .1,    .8,    "mosca",
      .1,    0,     "jacaré",
      .1,    -.8,   "gafanhoto",
      2.2,   .9,    "lambari",
      2.2,   0,     "preguiça",
      2.2,   -.9,   "queixada",
      -3.4,  -2.3,  "lontra",
      -2.3,  -2.5,  "tamanduá"
    )
    g <- g + geom_text(data = sp, aes(x, y, label = rot), size = 4.8, fontface = "italic",
                       colour = cores[["texto"]]) +
      annotate("text", x = -2.9, y = -2.85, label = "ausentes das duas", size = 4,
               colour = cores[["neutro"]])
  }
  g + coord_equal(xlim = c(-4, 4.1), ylim = c(-3.1, 2.6)) + tema_vazio()
}

#' Curvas de resposta de espécies ao longo de um gradiente de pH (dados
#' simulados): nos extremos, várias espécies estão ausentes
fig_nicho <- function() {
  set.seed(8)
  ph <- seq(3.5, 7, length.out = 300)
  sp <- tibble(sp = paste0("sp", 1:8), otimo = runif(8, 3.6, 6.9), tol = runif(8, .15, .35),
               h = runif(8, 30, 150))
  d <- sp |> tidyr::crossing(ph = ph) |>
    mutate(abund = h * exp(-(ph - otimo)^2 / (2 * tol^2)))
  pal <- grDevices::hcl.colors(8, "Dark 3")
  ggplot(d, aes(ph, abund, colour = sp)) +
    geom_line(linewidth = .9) +
    scale_colour_manual(values = pal, guide = "none") +
    labs(x = "pH", y = "Abundância") +
    tema_aula(base = 15)
}

#' O problema do truncamento em zero: a abundância não fica negativa além
#' dos limites do nicho
fig_truncamento <- function() {
  x <- seq(-1, 11, length.out = 300)
  d <- tibble(x, y = 10 * exp(-(x - 5)^2 / (2 * 1.6^2)) - 1.2)
  ggplot(d) +
    geom_line(data = filter(d, y >= 0), aes(x, y), colour = roxo, linewidth = 1.3) +
    geom_line(data = filter(d, y < 0, x < 5), aes(x, y), colour = roxo, linetype = "22") +
    geom_line(data = filter(d, y < 0, x > 5), aes(x, y), colour = roxo, linetype = "22") +
    geom_hline(yintercept = 0, colour = cores[["texto"]]) +
    annotate("segment", x = c(1.2, 8.8), xend = c(-.6, 10.6), y = 2.5, yend = 2.5,
             arrow = arrow(length = unit(.2, "cm")), colour = cores[["ferrugem"]]) +
    annotate("text", x = c(.3, 9.7), y = 3.2, label = "só zeros", colour = cores[["ferrugem"]],
             size = 5, fontface = "bold") +
    annotate("text", x = 5, y = -1.1, label = "gradiente ambiental", size = 5) +
    labs(x = NULL, y = "Abundância") +
    tema_aula(base = 15, conceitual = TRUE)
}

#' O que significa um zero? Zeros falsos e verdadeiros (adaptado de
#' Blasco-Moreno et al. 2019)
fig_tipos_zero <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                              ~cor,              ~face,
    4.5,  4,     2,   .8,  "Observação",                                         ne,                "bold",
    2.5,  2.7,   1.2, .7,  "0",                                                  ne,                "bold",
    7.5,  2.7,   1.2, .7,  "> 0",                                                ne,                "bold",
    1,    1.4,   2.4, .8,  "Zeros falsos",                                       cores[["ferrugem"]], "bold",
    4.1,  1.4,   2.4, .8,  "Zeros verdadeiros",                                  roxo,              "bold",
    -.2,  0,     2.4, 1.1, "Erros do observador\n(presente, não\nencontrado)",   cores[["ferrugem"]], "plain",
    2.4,  0,     2.4, 1.1, "Erros de\ndelineamento (local,\nmomento, esforço)", cores[["ferrugem"]], "plain",
    5,    0,     2.4, 1.1, "Estruturais\n(razão ecológica:\nfora do nicho)",    roxo,              "plain",
    7.6,  0,     2.4, 1.1, "Aleatórios\n(sem razão\necológica)",                 roxo,              "plain"
  )
  s <- function(i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                             xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)
  setas <- bind_rows(s(1, 2), s(1, 3), s(2, 4), s(2, 5), s(4, 6), s(4, 7), s(5, 8), s(5, 9))
  diagrama(cx, setas, base = 4.3) +
    annotate("text", x = 1.1, y = -.95, label = "zeros a evitar", colour = cores[["ferrugem"]],
             size = 4.4, fontface = "italic") +
    annotate("text", x = 6.3, y = -.95, label = "inflação de zeros", colour = roxo,
             size = 4.4, fontface = "italic")
}

#' Pares de observações de presença e ausência de uma espécie ao longo de
#' um gradiente (redesenho de um slide de P. Legendre)
fig_pares_zeros <- function() {
  x <- seq(0, 10, length.out = 300)
  curva <- tibble(x, y = exp(-(x - 5)^2 / (2 * .9^2)))
  setas <- tibble(
    x   = c(1.1, 1.7, 2.3, 4.4, 5.6, 6.6, 8.6, 9.3),
    par = c("marrom", "azul", "marrom", "verde", "verde", "vermelho", "azul", "vermelho")
  )
  # o vermelho de fora fica longe da curva
  setas$x[setas$par == "vermelho"] <- c(5.2, 9.3)
  setas$x[setas$par == "verde"] <- c(4.3, 5.8)
  pal <- c(verde = cores[["verde"]], vermelho = "#c0392b", marrom = "#8b5a2b", azul = cores[["accent"]])
  ggplot() +
    geom_line(data = curva, aes(x, y), colour = "#c0392b", linewidth = 1.1) +
    geom_hline(yintercept = 0, colour = cores[["texto"]]) +
    geom_segment(data = setas, aes(x, -.35, xend = x, yend = -.05, colour = par),
                 linewidth = 1.2, arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
    scale_colour_manual(values = pal, guide = "none") +
    coord_cartesian(ylim = c(-.4, 1.05)) +
    labs(x = "Gradiente ambiental", y = NULL) +
    tema_aula(base = 15, conceitual = TRUE) +
    theme(axis.line.y = element_blank())
}
