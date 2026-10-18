library(tibble)
library(dplyr)
library(ggplot2)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

az <- cores_familia[["Ordenação irrestrita"]]

#' nMDS dos peixes do Doubs (Bray-Curtis, 2 dimensões). A semente fixa a
#' configuração inicial; o que interessa é a configuração final.
nmds_doubs <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  set.seed(2026)
  vegan::metaMDS(e$doubs$fish[-8, ], distance = "bray", k = 2, trace = FALSE,
                 autotransform = FALSE)
}

#' O algoritmo do nMDS: escolher k, calcular o stress, mexer na
#' configuração e repetir
fig_nmds_fluxo <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                             ~cor,
    0,    0,     2.3, 1.2, "X\nobjetos × descritores",                         ne,
    3.4,  0,     2.3, 1.2, "D\nqualquer coeficiente\n(métrico ou semimétrico)", ne,
    6.8,  0,     2.3, 1.2, "Configuração inicial\nem k dimensões\n(p. ex., k = 2)", ne,
    6.8, -2.2,   2.3, 1.2, "Calcula o stress",                                 az,
    3.4, -2.2,   2.3, 1.2, "Modifica o arranjo\ndos objetos",                  az,
    0,   -2.2,   2.3, 1.2, "Solução final:\nstress mínimo",                    ne
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend, ~curv,
    1.15,  0,     2.2,   0,      0,
    4.55,  0,     5.6,   0,      0,
    6.8,  -.6,    6.8,  -1.55,   0,
    5.65, -2.4,   4.6,  -2.4,    0,
    4.55, -1.95,  5.6,  -1.95,  -.0,
    2.25, -2.2,   1.2,  -2.2,    0
  )
  diagrama(cx, setas, base = 4.4) +
    annotate("text", x = 5.1, y = -1.7, label = "repete até\nconvergir", size = 3.8,
             colour = cores[["neutro"]], lineheight = .9) +
    annotate("text", x = 1.7, y = -1.9, label = "não melhora\nmais", size = 3.8,
             colour = cores[["neutro"]], lineheight = .9)
}

#' Ordenação dos locais do Doubs no nMDS, com o stress no subtítulo
fig_nmds_doubs <- function() {
  nm <- nmds_doubs()
  sc <- orienta_coord(vegan::scores(nm, display = "sites"), "1")
  d <- as_tibble(sc, rownames = "local")
  ggplot(d, aes(NMDS1, NMDS2)) +
    geom_path(colour = cores[["neutro_cl"]]) +
    geom_point(shape = 21, fill = az, colour = "white", size = 3.4) +
    geom_text(aes(label = local), vjust = -.9, size = 3.6, colour = cores[["texto"]]) +
    coord_equal() +
    labs(subtitle = paste0("Stress = ", format(round(nm$stress, 3), decimal.mark = ","))) +
    tema_aula(base = 14)
}
