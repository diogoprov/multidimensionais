library(purrr)
library(tibble)
library(ggplot2)
library(dplyr)
source(here::here("R/tema.R"))   # cores, cores_familia

# Retângulo de cantos arredondados como polígono, para geom_polygon().
ret_arred <- function(x, y, w, h, r = .16, n = 12) {
  t <- seq(0, pi / 2, length.out = n)
  cx <- c(x + w / 2 - r, x - w / 2 + r, x - w / 2 + r, x + w / 2 - r)
  cy <- c(y + h / 2 - r, y + h / 2 - r, y - h / 2 + r, y - h / 2 + r)
  purrr::map_dfr(1:4, \(k) tibble::tibble(
    px = cx[k] + r * cos(t + (k - 1) * pi / 2),
    py = cy[k] + r * sin(t + (k - 1) * pi / 2)))
}

#' Mapa dos métodos multidimensionais: o fio condutor da disciplina
#'
#' Da matriz de dados saem dois caminhos: medir a semelhança entre objetos
#' (associação) e, a partir dela, classificar, ordenar ou testar; ou
#' modelar a abundância diretamente (métodos baseados em modelos). Cada
#' família tem a sua cor (`cores_familia`, em R/tema.R); as famílias fora
#' do destaque ficam em cinza.
#'
#' @param destaque famílias a destacar (nomes de `cores_familia`); NULL
#'   destaca todas
#' @param perguntas mostrar a pergunta que cada família responde
#' @param base tamanho base do texto
mapa_plot <- function(destaque = NULL, perguntas = TRUE, base = 6.2) {

  neutro      <- "#e6e0d3"
  neutro_bord <- "#b9b1a0"
  neutro_txt  <- "#5f5a4e"

  nos <- tibble::tribble(
    ~id,                    ~x,   ~y,    ~aula,     ~pergunta,
    "Associação",           3.4,  .75,   "Aula 2",  "Quão parecidos são\ndois objetos?",
    "Classificação",        7.3,  3.0,   "Aula 3",  "Os objetos formam\ngrupos discretos?",
    "Ordenação irrestrita", 7.3,  1.5,   "Aula 4",  "Quais são os principais\ngradientes?",
    "Ordenação restrita",   7.3,  0,     "Aula 5",  "Quanto da variação de Y\né explicada por X?",
    "Testes",               7.3, -1.5,   "Aula 6",  "Duas matrizes covariam?\nOs grupos diferem?",
    "Modelos",              3.4, -2.55,  "Aula 7",  "Como modelar a\nabundância diretamente?"
  ) |>
    mutate(
      w  = ifelse(x > 5, 3.3, 2.9), h = 1.22,
      on = if (is.null(destaque)) TRUE else id %in% destaque,
      cor_fill = ifelse(on, cores_familia[id], neutro),
      cor_bord = ifelse(on, cores_familia[id], neutro_bord),
      cor_txt  = ifelse(on, "white", neutro_txt)
    )
  dados <- tibble::tibble(x = 0, y = .75, w = 2.5, h = 1.22)

  caixas <- purrr::pmap_dfr(nos, function(id, x, y, w, h, cor_fill, cor_bord, ...)
    ret_arred(x, y, w, h) |> mutate(id = id, cor_fill = cor_fill, cor_bord = cor_bord))
  caixa_dados <- ret_arred(dados$x, dados$y, dados$w, dados$h)

  assoc <- nos[nos$id == "Associação", ]
  dir   <- nos[nos$x > 5, ]
  mod   <- nos[nos$id == "Modelos", ]
  seta  <- arrow(length = unit(0.2, "cm"), type = "closed")
  cor_seta <- cores[["accent_dark"]]

  dy_id <- if (perguntas) 0.3 else 0.12

  ggplot() +
    # matriz → associação → as quatro famílias baseadas em distância
    annotate("segment", x = dados$x + dados$w / 2 + .05, y = dados$y,
             xend = assoc$x - assoc$w / 2 - .1, yend = assoc$y,
             linewidth = .9, colour = cor_seta, arrow = seta) +
    geom_curve(data = dir, aes(x = assoc$x + assoc$w / 2 + .05, y = assoc$y,
                               xend = x - w / 2 - .1, yend = y),
               curvature = 0, linewidth = .9, colour = cor_seta, arrow = seta) +
    # matriz → modelos, sem passar por uma medida de distância
    annotate("curve", x = dados$x, y = dados$y - dados$h / 2 - .05,
             xend = mod$x - mod$w / 2 - .1, yend = mod$y,
             curvature = .35, linewidth = .9, colour = cor_seta,
             linetype = "22", arrow = seta) +
    annotate("text", x = .75, y = -.75, label = "sem matriz de\ndistância",
             colour = neutro_txt, size = base * .52, hjust = 0, lineheight = .95,
             fontface = "italic") +
    geom_polygon(data = caixa_dados, aes(px, py), fill = cores[["papel"]],
                 colour = cor_seta, linewidth = .7) +
    annotate("text", x = dados$x, y = dados$y + .17, label = "Matriz de dados",
             fontface = "bold", colour = cor_seta, size = base * .72) +
    annotate("text", x = dados$x, y = dados$y - .22, label = "objetos × descritores",
             colour = cor_seta, size = base * .58) +
    geom_polygon(data = caixas, aes(px, py, group = id),
                 fill = caixas$cor_fill, colour = caixas$cor_bord, linewidth = .6) +
    geom_text(data = nos, aes(x, y + dy_id, label = id),
              colour = nos$cor_txt, fontface = "bold", size = base * .72) +
    {if (perguntas)
      geom_text(data = nos, aes(x, y - .2, label = pergunta),
                colour = nos$cor_txt, size = base * .52, lineheight = .95)
     else NULL} +
    # número da aula: à direita das caixas da coluna da direita e abaixo das
    # da esquerda, onde à direita estão as setas
    geom_text(data = nos, aes(ifelse(x > 5, x + w / 2 + .12, x),
                              ifelse(x > 5, y, y - h / 2 - .2), label = aula),
              colour = ifelse(nos$on, cores[["texto"]], neutro_bord),
              size = base * .5, hjust = ifelse(nos$x > 5, 0, .5)) +
    coord_equal(clip = "off") +
    theme_void() +
    theme(plot.background  = element_rect(fill = cores[["papel"]], colour = NA),
          panel.background = element_rect(fill = cores[["papel"]], colour = NA),
          plot.margin = margin(4, 30, 4, 4))
}
