library(tibble)
library(dplyr)
library(ggplot2)
source(here::here("R/tema.R"))

roxo <- cores_familia[["Associação"]]

# Espécies das parcelas: forma e cor de cada uma (as mesmas em todos os
# casos, para a turma reconhecer as espécies entre os slides)
especies <- tribble(
  ~sp,         ~shape, ~fill,
  "círculo",    21,    "#9cc3e6",
  "meia-lua",   21,    "#b8b8b8",
  "trapézio",   22,    "#f4b183",
  "pentágono",  23,    "#e6a0a0",
  "cilindro",   22,    "#7fa7d9",
  "triângulo",  24,    "#8cc63f",
  "octógono",   21,    "#d9ead3",
  "losango",    23,    "#c9bf96",
  "quadrado",   22,    "#ffe066",
  "estrela",    25,    "#c39bd3",
  "hexágono",   24,    "#76b7b2"
)

#' Duas parcelas com as espécies presentes em cada uma
#'
#' @param p1,p2 vetores com as espécies (nomes de `especies$sp`) de cada
#'   parcela; espécies repetidas são vários indivíduos
#' @param letras marca cada espécie com a, b ou c
fig_parcelas <- function(p1, p2, letras = TRUE) {
  set.seed(3)
  pos <- function(v, x0) {
    n <- length(v)
    tibble(sp = v, x = x0 + seq(.6, 4.4, length.out = n) + runif(n, -.15, .15),
           y = runif(n, .7, 2.6))
  }
  d <- bind_rows(pos(p1, 0) |> mutate(parcela = 1), pos(p2, 5.5) |> mutate(parcela = 2)) |>
    left_join(especies, by = "sp")
  u1 <- unique(p1); u2 <- unique(p2)
  d <- d |> mutate(tipo = case_when(sp %in% u1 & sp %in% u2 ~ "a",
                                    parcela == 1 ~ "b", TRUE ~ "c"))
  rot <- d |> group_by(parcela, sp) |> slice(1) |> ungroup()
  corl <- c(a = "#c0392b", b = cores[["accent"]], c = cores[["texto"]])
  g <- ggplot() +
    annotate("rect", xmin = c(0, 5.5), xmax = c(5, 10.5), ymin = 0, ymax = 3.2,
             fill = "#d8cfa0", colour = cores[["texto"]]) +
    geom_segment(data = d, aes(x, y - .45, xend = x, yend = y), linewidth = 1.2) +
    geom_point(data = d, aes(x, y, shape = I(shape), fill = I(fill)), size = 9, stroke = 1.1) +
    annotate("text", x = c(2.5, 8), y = -.35, label = c("Parcela 1", "Parcela 2"),
             fontface = "bold", size = 5)
  if (letras) {
    g <- g + geom_text(data = rot, aes(x, y + .5, label = tipo, colour = tipo), size = 6,
                       fontface = "bold") +
      scale_colour_manual(values = corl, guide = "none")
  }
  g + coord_equal(xlim = c(-.2, 10.7), ylim = c(-.6, 3.3)) +
    theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# Os três casos dos slides de Jaccard
parcelas_exemplo <- list(
  p1 = c("círculo", "círculo", "meia-lua", "trapézio", "pentágono", "pentágono",
         "cilindro", "triângulo"),
  p2 = c("octógono", "triângulo", "círculo", "triângulo", "losango", "losango", "quadrado")
)
parcelas_nenhuma <- list(
  p1 = c("círculo", "meia-lua", "trapézio", "pentágono", "cilindro", "cilindro"),
  p2 = c("triângulo", "octógono", "losango", "quadrado", "estrela", "hexágono", "triângulo")
)
parcelas_todas <- list(
  p1 = c("círculo", "meia-lua", "trapézio", "pentágono", "cilindro", "círculo"),
  p2 = c("círculo", "pentágono", "cilindro", "meia-lua", "trapézio", "pentágono")
)

#' Contagens a, b, c e o índice de Jaccard de duas parcelas
jaccard_abc <- function(p) {
  u1 <- unique(p$p1); u2 <- unique(p$p2)
  a <- length(intersect(u1, u2)); b <- length(setdiff(u1, u2)); c <- length(setdiff(u2, u1))
  c(a = a, b = b, c = c, J = a / (a + b + c))
}
