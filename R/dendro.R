library(tibble)
library(dplyr)
library(ggplot2)
source(here::here("R/tema.R"))

#' Segmentos de um dendrograma, para desenhar com geom_segment()
#'
#' Dendrograma horizontal, como em Legendre & Legendre: a distância no eixo
#' x e os objetos no eixo y, na ordem do agrupamento.
#'
#' @param hc objeto hclust
#' @param ate desenha só as fusões com altura <= ate (para mostrar o
#'   agrupamento passo a passo); Inf desenha todas
#' @return lista com `seg` (x, y, xend, yend, ativo) e `folhas` (rótulo, y)
dendro_seg <- function(hc, ate = Inf) {
  n <- length(hc$order)
  pos <- numeric(n); pos[hc$order] <- seq_len(n)
  ym <- numeric(n - 1); xm <- hc$height
  coord <- function(k) if (k < 0) c(0, pos[-k]) else c(xm[k], ym[k])
  seg <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    a <- coord(hc$merge[i, 1]); b <- coord(hc$merge[i, 2])
    ym[i] <- (a[2] + b[2]) / 2
    seg[[i]] <- tibble(
      x    = c(a[1], b[1], xm[i]),
      xend = c(xm[i], xm[i], xm[i]),
      y    = c(a[2], b[2], a[2]),
      yend = c(a[2], b[2], b[2]),
      passo = i, ativo = xm[i] <= ate + 1e-9
    )
  }
  folhas <- tibble(rotulo = hc$labels, y = pos)
  list(seg = bind_rows(seg), folhas = folhas)
}

#' Dendrograma horizontal em ggplot2
#'
#' @param hc objeto hclust
#' @param ate ver dendro_seg()
#' @param corte desenha uma linha vertical neste nível de distância
#' @param grupos vetor de grupos (na ordem de hc$labels) para colorir as
#'   folhas
#' @param titulo título do painel
#' @param xlab rótulo do eixo da distância
#' @param base tamanho base do texto
fig_dendro <- function(hc, ate = Inf, corte = NULL, grupos = NULL, titulo = NULL,
                       xlab = "Distância", base = 14, xmax = NULL) {
  d <- dendro_seg(hc, ate)
  fol <- d$folhas
  roxo <- cores_familia[["Classificação"]]
  if (!is.null(grupos)) {
    pal <- c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]], cores[["ocre"]],
             "#6b4c8a", "#8c3b5e")
    fol$cor <- pal[as.integer(factor(grupos))]
  } else fol$cor <- cores[["texto"]]
  xmax <- xmax %||% (max(hc$height) * 1.08)
  g <- ggplot() +
    geom_segment(data = filter(d$seg, !ativo), aes(x, y, xend = xend, yend = yend),
                 colour = cores[["neutro_cl"]], linewidth = .8) +
    geom_segment(data = filter(d$seg, ativo), aes(x, y, xend = xend, yend = yend),
                 colour = roxo, linewidth = 1) +
    geom_text(data = fol, aes(0, y, label = rotulo), hjust = 1.15, colour = fol$cor,
              size = base * .3, fontface = "bold")
  if (!is.null(corte)) {
    g <- g + geom_vline(xintercept = corte, colour = "#c0392b", linetype = "22", linewidth = .9)
  }
  g +
    scale_x_continuous(limits = c(-.12 * xmax, xmax), expand = expansion(mult = c(0, .02)),
                       labels = function(x) format(x, decimal.mark = ",")) +
    scale_y_reverse() +
    labs(x = xlab, y = NULL, title = titulo) +
    tema_aula(base = base) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank())
}
