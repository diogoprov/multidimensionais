library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
library(vegan)      # ordiR2step() reavalia a chamada rda()
source(here::here("R/aula05_rda.R"))

# ---- db-RDA -----------------------------------------------------------------

#' Três abordagens de ordenação restrita (inspirado na Fig. 11.6 de
#' Legendre & Legendre 2012), com a tb-RDA e a db-RDA
fig_tb_db <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                ~cor,
    0,    2.2,   2.4, .9,  "Y bruto + X",                          ne,
    0,    .8,    2.4, .9,  "Y bruto",                              vd,
    3.4,  .8,    2.4, .9,  "Y transformado\n+ X",                  vd,
    0,    -.8,   2.4, .9,  "Y bruto",                              ne,
    3.4,  -.8,   2.4, .9,  "Matriz de\ndistâncias",                ne,
    6.8,  -.8,   2.4, .9,  "Coordenadas\nprincipais + X",          ne,
    10.2, .8,    2.2, 1.2, "Ordenação\nrestrita",                  vd
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.2,   2.2,   9.05,  1.2,
    1.2,   .8,    2.15,  .8,
    4.6,   .8,    9.05,  .8,
    1.2,   -.8,   2.15,  -.8,
    4.6,   -.8,   5.55,  -.8,
    8,     -.8,   9.05,  .4
  )
  rot <- tribble(
    ~x,    ~y,    ~rotulo,                                              ~cor,
    5.5,   2.45,  "clássica: RDA (gradientes curtos) ou CCA (longos)",  ne,
    6.6,   1.05,  "RDA",                                                 vd,
    1.7,   1.25,  "Hellinger, chord",                                    vd,
    5.1,   -.55,  "PCoA",                                                ne,
    8.9,   -.2,   "RDA",                                                 ne,
    -1.3,  1.45,  "(b) tb-RDA, baseada em transformação",                vd,
    -1.3,  -.05,  "(c) db-RDA, baseada em distâncias",                   ne
  )
  diagrama(cx, setas, base = 4.4) +
    geom_text(data = rot, aes(x, y, label = rotulo), colour = rot$cor, size = 4,
              hjust = ifelse(rot$x < 0, 0, .5), fontface = ifelse(rot$x < 0, "bold", "plain"))
}

# ---- mapas de autovetores (PCNM / dbMEM) --------------------------------------

#' PCNM num transecto de 35 pontos igualmente espaçados: alguns autovetores
fig_pcnm_transecto <- function(quais = c(1, 2, 15, 16)) {
  x <- 1:35
  pc <- vegan::pcnm(stats::dist(x))
  v <- vegan::scores(pc)
  d <- lapply(quais, \(k) tibble(pos = x, valor = v[, k], k = paste0("PCNM ", k))) |>
    bind_rows() |> mutate(k = factor(k, paste0("PCNM ", quais)))
  ggplot(d, aes(pos, valor)) +
    geom_hline(yintercept = 0, linetype = "22", colour = cores[["neutro"]]) +
    geom_line(colour = vd, linewidth = 1) + geom_point(colour = vd, size = 1.3) +
    facet_wrap(~k, nrow = 2, scales = "free_y") +
    labs(x = "Posição no transecto", y = "Valor do autovetor") +
    tema_aula(base = 13)
}

#' Os passos do PCNM: distâncias, truncamento e PCoA
fig_pcnm_passos <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,  ~w,   ~h,  ~rotulo,                                                  ~cor,
    0,    0,   2.6,  1.3, "1. Distâncias\neuclidianas entre\nos pontos",            ne,
    3.6,  0,   2.8,  1.3, "2. Truncamento:\nd > limiar vira\n4 × limiar",           ne,
    7.2,  0,   2.6,  1.3, "3. PCoA da matriz\ntruncada",                            vd,
    10.6, 0,   2.4,  1.3, "4. Autovetores com\nautovalor positivo\n= PCNMs",        vd
  )
  setas <- tribble(~x, ~y, ~xend, ~yend, 1.3, 0, 2.15, 0, 5, 0, 5.85, 0, 8.5, 0, 9.35, 0)
  diagrama(cx, setas, base = 4.3) +
    annotate("text", x = 3.6, y = -1, size = 3.9, colour = ne,
             label = "limiar: a maior aresta da árvore\ngeradora mínima (minimum spanning tree)",
             lineheight = .9)
}

# ---- RDA parcial ----------------------------------------------------------------

#' Os dois primeiros passos da RDA parcial: resíduos de Y e de X na regressão
#' contra a covariável W (dados simulados)
fig_parcial_passos <- function() {
  set.seed(9)
  w <- runif(25, 0, 10)
  y <- 1 + .8 * w + rnorm(25, 0, 1.3)
  x <- 2 + .6 * w + rnorm(25, 0, 1.3)
  painel <- function(r, titulo, ylab) {
    fit <- lm(r ~ w)
    d <- tibble(w, r, rhat = fitted(fit))
    ggplot(d, aes(w, r)) +
      geom_segment(aes(xend = w, yend = rhat), colour = "#c0392b", linetype = "22") +
      geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], colour = cores[["accent"]],
                  linewidth = 1.1) +
      geom_point(shape = 21, fill = cores[["neutro_cl"]], colour = cores[["texto"]], size = 2.8) +
      labs(title = titulo, x = "W (covariável)", y = ylab) +
      tema_aula(base = 14)
  }
  painel(y, "1º passo: resíduos de Y ~ W", "Y") +
    painel(x, "2º passo: resíduos de X ~ W", "X")
}

#' Os passos da RDA parcial
fig_parcial_esquema <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                ~cor, ~face,
    0,    1,     1.6, 1.1, "Y res | W",                               vd,   "bold",
    3.6,  1,     1.6, 1.1, "X res | W",                               vd,   "bold",
    0,    -.8,   1.6, 1.1, "Y",                                    ne,   "bold",
    3.6,  -.8,   1.6, 1.1, "X res | W",                               vd,   "bold"
  )
  setas <- tribble(~x, ~y, ~xend, ~yend, .85, 1, 2.75, 1, .85, -.8, 2.75, -.8)
  diagrama(cx, setas, base = 5) +
    annotate("text", x = 1.8, y = c(1.25, -.55), label = "RDA", size = 4.6, fontface = "bold",
             colour = ne) +
    annotate("text", x = 4.6, y = c(1, -.8), hjust = 0, size = 4.2, colour = vd,
             label = c("3º passo", "ou 3º passo")) +
    annotate("text", x = 4.6, y = .1, hjust = 0, size = 3.9, colour = ne, lineheight = .9,
             label = "X res | W: a variação ambiental\nnão explicada pelo espaço") +
    geom_blank(data = tibble(x = 7.5, y = 0), aes(x, y))
}

# ---- partição de variância --------------------------------------------------------

#' Partição de variância dos ácaros (Hellinger) entre ambiente (densidade do
#' substrato e conteúdo de água) e espaço (PCNMs selecionados)
mite_varpart <- function() {
  e <- new.env()
  utils::data("mite", "mite.env", "mite.xy", package = "vegan", envir = e)
  mh <- vegan::decostand(e$mite, "hellinger")
  pc <- vegan::pcnm(stats::dist(e$mite.xy))
  W <- as.data.frame(vegan::scores(pc))
  set.seed(1)
  r0 <- vegan::rda(mh ~ 1, W)
  rf <- vegan::rda(mh ~ ., W[, 1:40])
  fs <- vegan::ordiR2step(r0, rf, permutations = 499, trace = FALSE)
  sel <- attr(stats::terms(fs), "term.labels")
  X <- e$mite.env[, c("SubsDens", "WatrCont")]
  vp <- vegan::varpart(mh, X, W[, sel])
  f <- vp$part$indfract$Adj.R.squared
  list(vp = vp, sel = sel, a = f[1], c = f[2], b = f[3], d = f[4],
       ab = vp$part$fract$Adj.R.squared[1], bc = vp$part$fract$Adj.R.squared[2],
       abc = vp$part$fract$Adj.R.squared[3])
}

#' Diagrama da partição de variância em barras, na notação de Legendre &
#' Legendre: [a] só ambiente, [b] compartilhada, [c] só espaço, [d] resíduos
fig_varpart <- function(v = mite_varpart()) {
  fr <- c(a = v$a, b = v$b, c = v$c, d = v$d)
  x0 <- c(0, cumsum(fr))[1:4]
  d <- tibble(f = names(fr), xmin = x0, xmax = x0 + fr,
              cor = c(cores[["verde"]], "#b5a36a", cores[["ocre"]], cores[["papel"]]))
  pct <- \(z) paste0(format(round(100 * z), decimal.mark = ","), "%")
  ggplot(d) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1), fill = d$cor,
              colour = cores[["texto"]]) +
    geom_text(aes((xmin + xmax) / 2, .5, label = pct(xmax - xmin)), size = 7.5, fontface = "bold") +
    geom_text(aes((xmin + xmax) / 2, -.25, label = paste0("[", f, "]")), size = 6.5) +
    annotate("segment", x = 0, xend = v$a + v$b, y = 1.25, yend = 1.25, linewidth = 1,
             colour = cores[["verde"]]) +
    annotate("text", x = (v$a + v$b) / 2, y = 1.45, label = "explicada pelo ambiente (X)",
             colour = cores[["verde"]], size = 6) +
    annotate("segment", x = v$a, xend = v$a + v$b + v$c, y = -.55, yend = -.55, linewidth = 1,
             colour = cores[["ocre"]]) +
    annotate("text", x = v$a + (v$b + v$c) / 2, y = -.75, label = "explicada pelo espaço (W)",
             colour = cores[["ocre"]], size = 6) +
    annotate("text", x = v$a + v$b + v$c + v$d / 2, y = 1.45, label = "não explicada",
             colour = cores[["texto"]], size = 6) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-.9, 1.6)) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' Estrutura espacial: abundância de uma espécie de ácaro nas 70 parcelas
#' (vegan::mite), com as coordenadas das parcelas
fig_mite_mapa <- function(sp = "LRUG") {
  e <- new.env()
  utils::data("mite", "mite.xy", package = "vegan", envir = e)
  d <- tibble(x = e$mite.xy$x, y = e$mite.xy$y, ab = e$mite[[sp]])
  ggplot(d, aes(x, y)) +
    geom_point(aes(size = ab), shape = 21, fill = vd, colour = "white", alpha = .85) +
    scale_size_area(max_size = 9, name = "Abundância") +
    coord_equal() +
    labs(x = "x (m)", y = "y (m)", subtitle = paste("Ácaro", sp)) +
    tema_aula(base = 13) + theme(legend.position = "right")
}

#' Tabela de inércia de uma RDA parcial dos ácaros: ambiente controlado pelos
#' PCNMs selecionados
tab_rda_parcial <- function(v = mite_varpart()) {
  e <- new.env()
  utils::data("mite", "mite.env", "mite.xy", package = "vegan", envir = e)
  mh <- vegan::decostand(e$mite, "hellinger")
  W <- as.matrix(as.data.frame(vegan::scores(vegan::pcnm(stats::dist(e$mite.xy))))[, v$sel])
  X <- e$mite.env[, c("SubsDens", "WatrCont")]
  r <- vegan::rda(mh, X, W)
  tot <- r$tot.chi
  tibble(Componente = c("Total", "Condicional (W: espaço)", "Restrita (X | W: ambiente)",
                        "Não restrita (resíduos)"),
         Inércia = c(tot, r$pCCA$tot.chi, r$CCA$tot.chi, r$CA$tot.chi)) |>
    mutate(Proporção = Inércia / tot,
           across(c(Inércia, Proporção), \(z) format(round(z, 4), nsmall = 4, decimal.mark = ",")))
}
