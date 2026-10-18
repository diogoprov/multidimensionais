library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/ordena.R"))

az <- cores_familia[["Ordenação irrestrita"]]

# ---- exemplos numéricos de P. Legendre ---------------------------------

#' 6 objetos × 3 espécies (slides do curso de P. Legendre)
ca_ex1 <- function() {
  matrix(c(1, 4, 3, 1, 2, 4,  5, 4, 3, 3, 2, 1,  2, 6, 0, 5, 4, 0), 6,
         dimnames = list(paste0("Obj.", 1:6), paste0("Spec.", 1:3)))
}

#' 6 locais × 7 espécies, com duas espécies raras (Sp.6 e Sp.7)
ca_ex2 <- function() {
  matrix(c(45, 25, 7, 25, 7, 45,  10, 8, 15, 10, 15, 8,  15, 10, 20, 20, 10, 15,
           10, 3, 12, 3, 10, 12,  3, 2, 5, 3, 2, 5,  2, 3, 0, 2, 3, 0,
           0, 0, 10, 0, 0, 10), 6,
         dimnames = list(paste0("Site.", 1:6), paste0("Sp.", 1:7)))
}

#' Biplot de uma CA: objetos (pontos) e espécies (quadrados)
#'
#' @param m matriz de abundâncias
#' @param scaling 1 (distâncias entre objetos) ou 2 (entre espécies)
#' @param ref espécie(s) de referência para o sinal dos eixos
#' @param destaque espécies em destaque (cor de alerta)
fig_ca <- function(m, scaling = 1, ref, destaque = NULL) {
  ca <- vegan::cca(m)
  e <- escores(ca, ref = ref, scaling = scaling)
  sp <- e$species |> mutate(dest = rotulo %in% destaque)
  si <- e$sites
  ev <- ca$CA$eig[1:2]
  rot_eixo <- function(k) paste0("CA", k, " (λ = ",
                                 format(round(ev[k], 4), decimal.mark = ","), ")")
  ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_point(data = si, aes(CA1, CA2), colour = cores[["texto"]], size = 3) +
    geom_text(data = si, aes(CA1, CA2, label = rotulo), vjust = -.9, size = 4,
              colour = cores[["texto"]]) +
    geom_point(data = sp, aes(CA1, CA2, colour = dest), shape = 22, size = 3.6,
               stroke = 1.1) +
    geom_text(data = sp, aes(CA1, CA2, label = rotulo, colour = dest), vjust = 1.9,
              size = 4.2, fontface = "bold") +
    scale_colour_manual(values = c(`FALSE` = az, `TRUE` = cores[["ferrugem"]]),
                        guide = "none") +
    scale_x_continuous(expand = expansion(mult = .12)) +
    scale_y_continuous(expand = expansion(mult = .15)) +
    coord_equal() +
    labs(x = rot_eixo(1), y = rot_eixo(2), subtitle = paste("Scaling", scaling)) +
    tema_aula(base = 14)
}

# ---- coenoclina simulada -----------------------------------------------

#' Coenoclina: 19 espécies com resposta gaussiana a um gradiente, amostradas
#' em 99 locais (o desenho da simulação de P. Legendre)
coenoclina <- function(n_loc = 99, n_sp = 19, h = 20, tol = 6) {
  grad <- seq(1, 100, length.out = n_loc)
  otimo <- seq(5, 95, length.out = n_sp)
  m <- outer(grad, otimo, \(g, o) h * exp(-(g - o)^2 / (2 * tol^2)))
  m <- round(m)
  dimnames(m) <- list(paste0("L", seq_len(n_loc)), paste0("sp", seq_len(n_sp)))
  list(grad = grad, m = m)
}

fig_coenoclina <- function() {
  cc <- coenoclina()
  d <- as_tibble(cc$m) |> mutate(grad = cc$grad) |>
    pivot_longer(-grad, names_to = "sp", values_to = "abund") |>
    mutate(sp = factor(sp, colnames(cc$m)))
  pal <- grDevices::hcl.colors(ncol(cc$m), "Dark 3")
  ggplot(d, aes(grad, abund, colour = sp)) +
    geom_line(linewidth = .8) +
    scale_colour_manual(values = pal, guide = "none") +
    labs(x = "Gradiente ecológico", y = "Abundância") +
    tema_aula(base = 15)
}

#' Distâncias de qui-quadrado entre o local 1 e os demais ao longo da
#' coenoclina: crescem até um teto e não aumentam mais
fig_quiquadrado <- function() {
  cc <- coenoclina()
  m <- cc$m[rowSums(cc$m) > 0, ]
  d <- as.matrix(vegan::vegdist(vegan::decostand(m, "chi.square"), "euclidean"))
  dd <- tibble(grad = cc$grad[rowSums(cc$m) > 0], dist = d[1, ])
  ca <- vegan::cca(m)
  sc <- orienta_coord(vegan::scores(ca, display = "sites", choices = 1:2, scaling = 1),
                      rownames(m)[1])
  sc <- as_tibble(sc, rownames = "loc") |> mutate(grad = dd$grad)
  # o local 1 deve ficar à esquerda, como no original
  if (sc$CA1[1] > 0) sc$CA1 <- -sc$CA1
  t <- tema_aula(base = 14)
  p1 <- ggplot(dd, aes(grad, dist)) +
    geom_line(colour = az, linewidth = 1) +
    geom_point(colour = az, size = 1.6) +
    labs(title = "(a) Distância ao local 1", x = "Posição no gradiente",
         y = "Distância de qui-quadrado") + t
  p2 <- ggplot(sc, aes(CA1, CA2)) +
    geom_path(colour = cores[["neutro"]]) +
    geom_point(aes(colour = grad), size = 2) +
    geom_point(data = sc[1, ], shape = 21, size = 5, stroke = 1.2,
               colour = cores[["ferrugem"]]) +
    annotate("text", x = sc$CA1[1], y = sc$CA2[1], label = "local 1", vjust = -1.4,
             colour = cores[["ferrugem"]], fontface = "bold") +
    scale_colour_gradient(low = cores[["neutro_cl"]], high = az, guide = "none") +
    labs(title = "(b) CA dos 99 locais: o arco", x = "CA1", y = "CA2") + t
  p1 + p2
}

#' Arco (CA), ferradura (PCA) e o que a DCA faz com o arco, na mesma
#' coenoclina simulada
fig_arco <- function() {
  cc <- coenoclina()
  m <- cc$m[rowSums(cc$m) > 0, ]
  g <- cc$grad[rowSums(cc$m) > 0]
  ref <- rownames(m)[1]
  prep <- function(sc, metodo) {
    sc <- as.matrix(sc[, 1:2])
    sc <- -orienta_coord(sc, ref)   # local 1 à esquerda e embaixo
    tibble(x = sc[, 1], y = sc[, 2], grad = g, metodo = metodo)
  }
  ca  <- vegan::scores(vegan::cca(m), display = "sites", choices = 1:2)
  pca <- vegan::scores(vegan::rda(m), display = "sites", choices = 1:2)
  dca <- vegan::scores(vegan::decorana(m), display = "sites", choices = 1:2)
  # um painel por método, cada um com a mesma escala nos dois eixos: é o
  # que mostra o achatamento do arco na DCA
  painel <- function(d) {
    ggplot(d, aes(x, y)) +
      geom_path(colour = cores[["neutro"]]) +
      geom_point(aes(colour = grad), size = 1.8) +
      scale_colour_gradient(low = cores[["neutro_cl"]], high = az, guide = "none") +
      coord_equal() +
      labs(title = d$metodo[1], x = "Eixo 1", y = "Eixo 2") +
      tema_aula(base = 14) +
      theme(axis.text = element_blank())
  }
  (painel(prep(ca, "CA: efeito arco")) | painel(prep(pca, "PCA: efeito ferradura"))) /
    painel(prep(dca, "DCA: arco achatado")) +
    plot_layout(heights = c(1.6, 1))
}

#' Inércia total e autovalores da CA ao remover espécies raras (BCI,
#' Condit et al. 2002, via vegan)
fig_raras_inercia <- function(kmax = 10) {
  e <- new.env()
  utils::data("BCI", package = "vegan", envir = e)
  bci <- e$BCI
  occ <- colSums(bci > 0)
  d <- lapply(0:kmax, \(k) {
    mm <- bci[, occ > k]
    ca <- vegan::cca(mm)
    tibble(k = k, inercia = ca$tot.chi,
           eixo = paste("Autovalor", 1:4), lambda = unname(ca$CA$eig[1:4]))
  }) |> bind_rows()
  t <- tema_aula(base = 14)
  # quebra na curva da inércia: até k = 4 ela cai rápido; depois, devagar
  quebra <- 4
  n_raras <- sum(occ <= quebra)
  linha <- geom_vline(xintercept = quebra + .5, colour = "#c0392b", linewidth = 1)
  p1 <- d |> distinct(k, inercia) |>
    ggplot(aes(k, inercia)) + linha +
    annotate("text", x = quebra + .7, y = max(d$inercia), hjust = 0, vjust = 1, size = 4.2,
             colour = "#c0392b", lineheight = .95,
             label = paste0("quebra: as ", n_raras, " espécies\ncom até ", quebra,
                            " ocorrências\nseriam as raras")) +
    geom_line(colour = az, linewidth = 1) + geom_point(colour = az, size = 2.4) +
    scale_x_continuous(breaks = 0:kmax) +
    labs(title = "Inércia total da CA",
         x = "Espécies removidas: ocorrências ≤ k", y = "Inércia") + t
  p2 <- ggplot(d, aes(k, lambda, colour = eixo)) + linha +
    geom_line(linewidth = 1) + geom_point(size = 2.2) +
    scale_x_continuous(breaks = 0:kmax) +
    scale_colour_manual(values = unname(c(az, cores[["ferrugem"]], cores[["ocre"]],
                                          cores[["verde"]])), name = NULL) +
    labs(title = "Quatro primeiros autovalores",
         x = "Espécies removidas: ocorrências ≤ k", y = "Autovalor") + t
  p1 + p2
}
