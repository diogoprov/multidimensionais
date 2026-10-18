library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

roxo <- cores_familia[["Associação"]]

# ---- padronização -------------------------------------------------------

#' Variáveis ambientais de seis locais (exemplo dos slides originais)
ambiente_ex <- function() {
  data.frame(pH = c(8.2, 6.3, 5.3, 5.6, 6.6, 6.1), T = c(27, 32, 36, 31, 19, 15),
             Umid = c(90, 93, 98, 89, 96, 75), row.names = paste("Local", 1:6))
}

#' Tabela com os dados, a média e o desvio-padrão de cada coluna
#'
#' @param z TRUE devolve os escores z (dados padronizados)
tab_ambiente <- function(z = FALSE) {
  d <- ambiente_ex()
  if (z) d <- as.data.frame(scale(d))
  dig <- if (z) 2 else 1
  m <- colMeans(d); s <- apply(d, 2, stats::sd)
  out <- rbind(d, "Média" = m, "DP" = s)
  out[] <- lapply(out, \(v) format(round(v, dig), nsmall = dig, decimal.mark = ","))
  out
}

# ---- transformações -------------------------------------------------------

#' Curva de dominância: abundância relativa (escala log) das espécies de
#' árvores do BCI, da mais à menos abundante
fig_dominancia <- function() {
  e <- new.env()
  utils::data("BCI", package = "vegan", envir = e)
  ab <- sort(colSums(e$BCI), decreasing = TRUE)
  d <- tibble(posto = seq_along(ab), rel = ab / max(ab))
  ggplot(d, aes(posto, rel)) +
    geom_line(colour = cores[["ferrugem"]], linewidth = .9) +
    geom_point(colour = cores[["ferrugem"]], size = 1.2) +
    scale_y_log10(labels = function(x) format(x, decimal.mark = ",", scientific = FALSE,
                                              drop0trailing = TRUE)) +
    labs(x = "Posto de abundância (da mais à menos abundante)",
         y = "Abundância relativa (log)") +
    tema_aula(base = 15)
}

#' Transformação antes da ordenação (Legendre & Gallagher 2001): a
#' distância euclidiana entre os dados transformados é a distância de chord
#' (ou de Hellinger) entre os dados brutos
fig_tb_pca <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,   ~w,  ~h,  ~rotulo,                                   ~cor,
    0,    1.4,  2.4, 1,   "Dados brutos\n(locais × espécies)",       ne,
    5,    1.4,  2.4, 1,   "Dados transformados\n(locais × espécies)", roxo,
    2.5, -.6,   2.4, 1,   "Distância de chord\nentre os locais",     ne,
    7.5, -.6,   2.4, 1,   "PCA, RDA...\n(distância euclidiana)",     roxo
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.2,   1.4,   3.75,  1.4,
    .5,    .9,    1.9,   -.05,
    4.5,   .9,    3.15,  -.05,
    5.5,   .9,    6.9,   -.05
  )
  diagrama(cx, setas, base = 4.4) +
    annotate("text", x = 2.5, y = 1.7, label = "transformação de chord\nou de Hellinger",
             size = 4, colour = roxo, lineheight = .9) +
    annotate("text", x = 2.5, y = .55, label = "mesma distância", size = 4,
             colour = cores[["neutro"]], fontface = "italic")
}

# ---- efeito do coeficiente na ordenação -----------------------------------

#' Três locais com abundâncias diferentes (Orlóci 1978, via Legendre &
#' Legendre 2012)
orloci <- function() {
  rbind(x1 = c(2, 5, 2, 5, 3), x2 = c(3, 5, 2, 4, 3), x3 = c(9, 1, 1, 1, 1))
}

#' Três locais do exemplo do duplo zero (Legendre & Legendre 2012): x1 e
#' x2 compartilham todas as espécies; x2 e x3, nenhuma
duplo_zero <- function() rbind(x1 = c(0, 4, 8), x2 = c(0, 1, 1), x3 = c(1, 0, 0))

#' PCoA dos três locais do duplo zero com seis coeficientes diferentes
#' (inspirado na Fig. 7.9 de Legendre & Legendre 2012)
fig_coef_pcoa <- function() {
  x <- duplo_zero()
  euc <- function(m) stats::dist(m)
  ds <- list(
    "Euclidiana"                 = euc(x),
    "Chord"                      = euc(vegan::decostand(x, "normalize")),
    "Perfis de espécies"         = euc(vegan::decostand(x, "total")),
    "Hellinger"                  = euc(vegan::decostand(x, "hellinger")),
    "Qui-quadrado"               = euc(vegan::decostand(x, "chi.square")),
    "Diferença percentual"       = vegan::vegdist(x, "bray")
  )
  d <- lapply(names(ds), \(n) {
    pc <- suppressWarnings(stats::cmdscale(ds[[n]], k = 2))
    # com só um autovalor positivo, a PCoA tem um eixo
    if (ncol(pc) < 2) {
      pc <- cbind(pc, 0)
      n <- paste0(n, "\n(só 1 autovalor positivo)")
    }
    rownames(pc) <- rownames(x)
    pc <- orienta_coord(pc, "x3")
    tibble(metodo = n, local = rownames(x), e1 = pc[, 1], e2 = pc[, 2])
  }) |> bind_rows() |>
    mutate(metodo = factor(metodo, unique(metodo)))
  # um painel por coeficiente, com a mesma escala nos dois eixos: é a
  # proporção entre as distâncias que muda de um coeficiente para outro
  painel <- function(dd) {
    r <- max(abs(c(dd$e1, dd$e2))) * 1.25
    ggplot(dd, aes(e1, e2)) +
      geom_point(size = 3, colour = roxo) +
      geom_text(aes(label = local, vjust = ifelse(local == "x1", 1.9, -.9)), size = 4.2) +
      coord_equal(xlim = c(-r, r), ylim = c(-r, r)) +
      labs(title = dd$metodo[1], x = NULL, y = NULL) +
      tema_aula(base = 13) +
      theme(axis.text = element_blank(), plot.title = element_text(size = 12, hjust = .5))
  }
  patchwork::wrap_plots(lapply(split(d, d$metodo), painel), nrow = 2)
}

# ---- chaves de escolha -------------------------------------------------------

#' Escolha do coeficiente entre objetos com dados de espécies (resumo da
#' Tabela 7.4 de Legendre & Legendre 2012)
fig_chave_especies <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,   ~rotulo,                                                     ~cor, ~face,
    4.3,  3.4,   5.2, .7,   "Espécies (modo Q): coeficientes assimétricos",             ne,   "bold",
    1.5,  2.1,   3.2, .9,   "Presença e ausência",                                       ne,   "bold",
    6.8,  2.1,   3.4, .9,   "Abundâncias\n(quantitativas)",                              ne,   "bold",
    1.5,  .6,    3.2, 1.3,  "Jaccard (métrico)\nSørensen, Ochiai\n(semimétricos)",       roxo, "plain",
    5.2,  .6,    2.6, 1.3,  "Sem padronizar:\ndiferença percentual\n(Bray-Curtis)",      roxo, "plain",
    8.2,  .6,    2.8, 1.3,  "Padronizando o local:\nchord, Hellinger,\nqui-quadrado",     roxo, "plain"
  )
  s <- function(i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                             xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)
  diagrama(cx, bind_rows(s(1, 2), s(1, 3), s(2, 4), s(3, 5), s(3, 6)), base = 4.4)
}

#' Escolha do coeficiente entre objetos com descritores ambientais (resumo
#' da Tabela 7.5 de Legendre & Legendre 2012)
fig_chave_ambiente <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,   ~rotulo,                                                      ~cor, ~face,
    4.5,  3.4,   6,   .7,   "Variáveis ambientais (modo Q): coeficientes simétricos",    ne,   "bold",
    1.3,  2.1,   2.8, .9,   "Binárias ou\nmultiestado",                                   ne,   "bold",
    4.5,  2.1,   3,   .9,   "Quantitativas e\nmesma unidade",                             ne,   "bold",
    7.7,  2.1,   3,   .9,   "Tipos ou unidades\ndiferentes",                              ne,   "bold",
    1.3,  .6,    2.8, 1.1,  "Simple matching\n(Sokal & Michener)",                        roxo, "plain",
    4.5,  .6,    3,   1.1,  "Euclidiana\nManhattan",                                      roxo, "plain",
    7.7,  .6,    3,   1.1,  "Gower\n(ou padronizar e\nusar a euclidiana)",                roxo, "plain"
  )
  s <- function(i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                             xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)
  diagrama(cx, bind_rows(s(1, 2), s(1, 3), s(1, 4), s(2, 5), s(3, 6), s(4, 7)), base = 4.4)
}
