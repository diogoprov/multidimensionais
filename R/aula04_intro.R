library(tibble)
library(ggplot2)
source(here::here("R/diagrama.R"))

#' Árvore de decisão das análises de ordenação
#'
#' Os métodos da aula ficam na cor da ordenação irrestrita; os da Aula 5
#' (ordenação restrita), na cor dela; o resto, neutro.
fig_arvore <- function() {
  az <- cores_familia[["Ordenação irrestrita"]]
  vd <- cores_familia[["Ordenação restrita"]]
  ne <- cores[["accent_dark"]]
  caixa <- function(x, y, rotulo, w = 2.5, h = .72, fill = cores[["papel"]],
                    cor = ne, txt = cor, face = "plain")
    tibble(x, y, w, h, rotulo, fill, cor, txt, face)
  cx <- dplyr::bind_rows(
    caixa(0,    .6,  "Técnicas\ninformais", w = 1.9, h = .9),
    caixa(2.6,  3.1, "Análise indireta\nde gradientes", w = 2.5, h = .9, face = "bold"),
    caixa(2.6, -1.9, "Análise direta\nde gradientes", w = 2.5, h = .9, face = "bold"),
    caixa(5.5,  4.3, "Baseadas em\ndistâncias", w = 2.1, h = .85),
    caixa(5.5,  1.9, "Baseadas em\nautoanálise", w = 2.1, h = .85),
    caixa(8.3,  1.9, "Modelo\nlinear", w = 1.9, h = .85),
    caixa(8.3,   .7, "Modelo\nunimodal", w = 1.9, h = .85),
    caixa(5.6, -1.3, "Modelo\nlinear", w = 1.9, h = .85),
    caixa(5.6, -2.5, "Modelo\nunimodal", w = 1.9, h = .85),
    caixa(8.6,  5.2, "Ordenação polar\n(Bray-Curtis)", h = .8),
    caixa(8.6,  4.3, "PCoA", fill = az, cor = az, txt = "white", face = "bold"),
    caixa(8.6,  3.4, "nMDS", fill = az, cor = az, txt = "white", face = "bold"),
    caixa(11.2, 1.9, "PCA", w = 1.7, fill = az, cor = az, txt = "white", face = "bold"),
    caixa(11.2, 1.1, "CA", w = 1.7, fill = az, cor = az, txt = "white", face = "bold"),
    caixa(11.2,  .3, "DCA", w = 1.7, fill = az, cor = az, txt = "white", face = "bold"),
    caixa(8.4, -1.3, "RDA", w = 1.7, fill = vd, cor = vd, txt = "white", face = "bold"),
    caixa(8.4, -2.1, "CCA", w = 1.7, fill = vd, cor = vd, txt = "white", face = "bold"),
    caixa(8.4, -2.9, "DCCA", w = 1.7, fill = vd, cor = vd, txt = "white", face = "bold")
  )
  # seta da borda direita da caixa i até a borda esquerda da caixa j
  s <- function(i, j) {
    a <- cx[i, ]; b <- cx[j, ]
    tibble(x = a$x + a$w / 2, y = a$y, xend = b$x - b$w / 2 - .05, yend = b$y)
  }
  v <- function(i, j) {   # seta vertical (das técnicas informais)
    a <- cx[i, ]; b <- cx[j, ]; sg <- sign(b$y - a$y)
    tibble(x = a$x, y = a$y + sg * a$h / 2, xend = b$x - b$w / 2 + .4,
           yend = b$y - sg * b$h / 2 - .05)
  }
  setas <- dplyr::bind_rows(
    v(1, 2), v(1, 3),
    s(2, 4), s(2, 5), s(5, 6), s(5, 7), s(3, 8), s(3, 9),
    s(4, 10), s(4, 11), s(4, 12), s(6, 13), s(7, 14), s(7, 15),
    s(8, 16), s(9, 17), s(9, 18)
  )
  diagrama(cx, setas, base = 4.3)
}

#' Três abordagens de ordenação irrestrita de uma matriz locais × espécies
#'
#' Redesenho da Fig. 9.8 de Legendre & Legendre (2012), com a abordagem
#' baseada em transformação (tb-PCA) destacada.
fig_abordagens <- function() {
  az <- cores_familia[["Ordenação irrestrita"]]
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                   ~cor, ~face,
    0,    3,     2.2, 1,   "Y = dados brutos\n(locais × espécies)",    ne,   "plain",
    0,    1.2,   2.2, 1,   "Dados brutos\n(locais × espécies)",        az,   "plain",
    4,    1.2,   2.4, 1,   "Y = dados\ntransformados",                 az,   "plain",
    0,   -1.3,   2.2, 1,   "Dados brutos\n(locais × espécies)",        ne,   "plain",
    4,   -1.3,   2.2, 1,   "Matriz de\ndistâncias",                    ne,   "plain",
    9.2,  2.1,   2.5, 1.3, "Biplot\nespécies: setas\nlocais: símbolos", ne, "plain",
    9.2, -1.3,   2.5, 1.3, "Ordenação\ndos locais",                    ne,   "plain"
  )
  setas <- tribble(
    ~x,   ~y,   ~xend, ~yend,
    1.1,  3,    7.9,   2.5,
    1.1,  1.2,  2.75,  1.2,
    5.2,  1.2,  7.9,   1.8,
    1.1, -1.3,  2.85, -1.3,
    5.1, -1.3,  7.9,  -1.3
  )
  rot <- tribble(
    ~x,   ~y,    ~rotulo,                                        ~cor,
    4.5,  3.35,  "gradientes curtos: PCA (ou CA)\ngradientes longos: CA", ne,
    1.9,  .85,   "ex.: Hellinger",                               az,
    6.5,  1.75,  "PCA",                                          az,
    6.5, -1.0,   "PCoA, nMDS",                                   ne,
    -1.35, 3.85, "(a) Abordagem clássica",                       ne,
    -1.35, 2.05, "(b) Baseada em transformação (tb-PCA)",        az,
    -1.35, -.45, "(c) Baseada em distâncias",                    ne,
    2.1,  .3,    "distâncias com propriedades euclidianas",      az
  )
  diagrama(cx, setas, base = 4.4) +
    annotate("rect", xmin = -1.4, xmax = 5.45, ymin = .02, ymax = 2.3,
             fill = NA, colour = az, linewidth = .9, linetype = "22") +
    geom_text(data = rot, aes(x, y, label = rotulo), colour = rot$cor,
              size = 4.2, hjust = ifelse(rot$x < 0, 0, .5), lineheight = .95,
              fontface = ifelse(rot$x < 0, "bold", "plain"))
}
