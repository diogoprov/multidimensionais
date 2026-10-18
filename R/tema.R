library(ggplot2)

# Vírgula decimal nos eixos e em format(): os slides são em português.
options(OutDec = ",")

# Paleta do Biodiversity Synthesis Lab, a mesma do site e do tema dos slides
# (copiada de ~/GitHub/bioestatistica/R/tema.R).
cores <- c(
  accent      = "#1c6e8c",
  accent_dark = "#144f64",
  verde       = "#527238",
  ocre        = "#96661f",
  ferrugem    = "#a15e3f",
  neutro      = "#8a8172",
  neutro_cl   = "#e6e0d3",
  texto       = "#2d3748",
  papel       = "#faf7f0"
)

# Identidade visual do mapa dos métodos: uma cor por família, usada no
# diagrama (R/mapa.R) e nos selos dos slides (.familia-* em
# slides/theme.scss; mantenha as duas listas iguais). Todas dão contraste
# de pelo menos 4,5:1 com texto branco.
cores_familia <- c(
  "Associação"           = "#6b4c8a",
  "Classificação"        = "#a15e3f",
  "Ordenação irrestrita" = "#1c6e8c",
  "Ordenação restrita"   = "#527238",
  "Testes"               = "#96661f",
  "Modelos"              = "#8c3b5e"
)

#' Tema base dos gráficos das aulas
#'
#' @param base tamanho base do texto (pt); 15 fica legível projetado
#' @param conceitual TRUE apaga números dos eixos e desenha setas nas
#'   pontas, para gráficos que ilustram uma ideia e não um conjunto de dados
tema_aula <- function(base = 15, conceitual = FALSE) {
  t <- theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = cores[["papel"]], colour = NA),
      panel.background = element_rect(fill = cores[["papel"]], colour = NA),
      panel.grid.minor = element_blank(),
      text             = element_text(colour = cores[["texto"]]),
      axis.text        = element_text(colour = "#4a5568"),
      strip.text       = element_text(face = "bold", size = rel(1),
                                      colour = cores[["texto"]]),
      plot.title       = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle    = element_text(colour = "#4a5568"),
      legend.position  = "bottom"
    )
  if (conceitual) {
    t <- t + theme(
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.line  = element_line(
        colour = cores[["texto"]], linewidth = .7,
        arrow = grid::arrow(length = grid::unit(.22, "cm"), type = "closed"))
    )
  }
  t
}
