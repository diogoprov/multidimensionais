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
    3.6,  .8,    2.4, .9,  "Y transformado\n+ X",                  vd,
    0,    -.8,   2.4, .9,  "Y bruto",                              ne,
    3.6,  -.8,   2.4, .9,  "Matriz de\ndistâncias",                ne,
    7.2,  -.8,   2.4, .9,  "Coordenadas\nprincipais + X",          ne,
    11,   .7,    2.2, 3.8, "Ordenação\nrestrita",                  vd
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.2,   2.2,   9.85,  2.2,
    1.2,   .8,    2.35,  .8,
    4.8,   .8,    9.85,  .8,
    1.2,   -.8,   2.35,  -.8,
    4.8,   -.8,   5.95,  -.8,
    8.4,   -.8,   9.85,  -.8
  )
  rot <- tribble(
    ~x,    ~y,    ~rotulo,                                  ~cor,
    5.5,   2.48,  "RDA (gradientes curtos) ou CCA (longos)", ne,
    7.3,   1.08,  "RDA",                                     vd,
    1.78,  1.08,  "Hellinger,\nchord",                       vd,
    5.38,  -.52,  "PCoA",                                    ne,
    9.13,  -.52,  "RDA",                                     ne
  )
  lado <- tibble(y = c(2.2, .8, -.8), cor = c(ne, vd, ne),
                 rotulo = c("(a) clássica", "(b) tb-RDA:\nbaseada em\ntransformação",
                            "(c) db-RDA:\nbaseada em\ndistâncias"))
  diagrama(cx, setas, base = 4.6) +
    geom_text(data = rot, aes(x, y, label = rotulo), colour = rot$cor, size = 4.4,
              lineheight = .85, vjust = 0) +
    geom_text(data = lado, aes(-1.45, y, label = rotulo), colour = lado$cor, size = 4.6,
              hjust = 1, fontface = "bold", lineheight = .9) +
    geom_blank(data = tibble(x = -4.2, y = 0), aes(x, y))
}

# ---- mapas de autovetores (PCNM / dbMEM) --------------------------------------

#' PCNM num transecto de 35 pontos igualmente espaçados: alguns autovetores
fig_pcnm_transecto <- function(quais = c(1, 2, 15, 16)) {
  x <- 1:35
  pc <- vegan::pcnm(stats::dist(x))
  v <- vegan::scores(pc)
  d <- lapply(quais, \(k) tibble(pos = x, valor = v[, k], k = paste0("dbMEM ", k))) |>
    bind_rows() |> mutate(k = factor(k, paste0("dbMEM ", quais)))
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

#' Mapas de dois PCNMs (= dbMEMs) nas 70 parcelas dos ácaros: quadrados pretos
#' para valores positivos e brancos para negativos, área proporcional ao valor
fig_pcnm_mite <- function(quais = c(1, 8)) {
  e <- new.env()
  utils::data("mite.xy", package = "vegan", envir = e)
  v <- vegan::scores(vegan::pcnm(stats::dist(e$mite.xy)))
  d <- lapply(quais, \(k) tibble(x = e$mite.xy$x, y = e$mite.xy$y, valor = v[, k],
                                 k = paste0("PCNM ", k))) |>
    bind_rows() |> mutate(k = factor(k, paste0("PCNM ", quais)), sinal = valor > 0)
  ggplot(d, aes(x, y)) +
    geom_point(aes(size = abs(valor), fill = sinal), shape = 22, colour = cores[["accent_dark"]]) +
    scale_fill_manual(values = c(`TRUE` = cores[["accent_dark"]], `FALSE` = "white"),
                      labels = c(`TRUE` = "positivo", `FALSE` = "negativo"), name = NULL) +
    scale_size_area(max_size = 6, guide = "none") +
    facet_wrap(~k, nrow = 1) +
    scale_x_continuous(breaks = c(0, 1, 2)) +
    coord_equal() +
    labs(x = "x (m)", y = "y (m)") +
    tema_aula(base = 15) + theme(legend.position = "bottom")
}

#' Exemplo numérico do dbMEM: transecto de 10 pontos igualmente espaçados
dbmem_ex <- function(n = 10) {
  D <- as.matrix(stats::dist(seq_len(n)))
  arv <- vegan::spantree(stats::as.dist(D))
  lim <- max(arv$dist)
  Dt <- D; Dt[D > lim] <- 4 * lim
  pc <- suppressWarnings(stats::cmdscale(stats::as.dist(Dt), k = n - 1, eig = TRUE))
  list(n = n, D = D, Dt = Dt, lim = lim, eig = pc$eig, vec = pc$points)
}

# triângulo inferior de uma matriz de distâncias, com os valores nas células
tri_dist <- function(M, x0, y0, cel = .42, cor_cel = NULL, tam = 3.8, titulo = NULL) {
  n <- nrow(M)
  d <- expand.grid(i = seq_len(n), j = seq_len(n)) |> subset(i > j)
  d$x <- x0 + (d$j - .5) * cel
  d$y <- y0 - (d$i - 1.5) * cel
  d$v <- M[cbind(d$i, d$j)]
  d$f <- if (is.null(cor_cel)) "white" else cor_cel(d$v)
  list(
    geom_tile(data = d, aes(x, y), fill = d$f, colour = cores[["accent_dark"]],
              width = cel, height = cel, linewidth = .3),
    geom_text(data = d, aes(x, y, label = v), size = tam, colour = cores[["accent_dark"]]),
    annotate("text", x = x0 - .15, y = y0 - (seq(2, n) - 1.5) * cel, label = seq(2, n),
             size = tam * .9, hjust = 1, colour = cores[["neutro"]]),
    annotate("text", x = x0 + (seq_len(n - 1) - .5) * cel, y = y0 - (n - .5) * cel,
             label = seq_len(n - 1), size = tam * .9, colour = cores[["neutro"]]),
    if (!is.null(titulo))
      annotate("text", x = x0, y = y0 + .55, label = titulo, size = 5.2, hjust = 0,
               fontface = "bold", colour = cores[["accent_dark"]])
  )
}

#' 1º passo do dbMEM: o transecto e a matriz de distâncias euclidianas
fig_dbmem_dist <- function(ex = dbmem_ex()) {
  n <- ex$n
  pts <- tibble(x = seq(0, by = .42, length.out = n), y = 1.2)
  ggplot() +
    annotate("segment", x = 0, xend = max(pts$x), y = 1.2, yend = 1.2, colour = cores[["neutro"]]) +
    geom_point(data = pts, aes(x, y), shape = 22, size = 6, fill = "white",
               colour = cores[["accent_dark"]]) +
    annotate("text", x = pts$x, y = 1.55, label = seq_len(n), size = 4.2,
             colour = cores[["neutro"]]) +
    annotate("text", x = 0, y = 2.05, hjust = 0, size = 5.2, colour = cores[["accent_dark"]],
             fontface = "bold", label = "Transecto: 10 pontos, a 1 m um do outro") +
    tri_dist(ex$D, 0, 0, titulo = "D: distâncias euclidianas entre os pontos") +
    coord_equal(clip = "off") + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' 2º passo do dbMEM: o limiar (maior aresta da árvore geradora mínima) separa
#' as distâncias pequenas das grandes; as grandes viram 4 × limiar
fig_dbmem_trunca <- function(ex = dbmem_ex()) {
  pq <- "#c0392b"; gr <- "#1f5fa8"
  cor <- function(v) ifelse(v <= ex$lim, alpha(pq, .25), alpha(gr, .18))
  cel <- .42; n <- ex$n
  ggplot() +
    tri_dist(ex$D, 0, 0, cel = cel, cor_cel = cor, titulo = "D") +
    annotate("segment", x = n * cel + .3, xend = n * cel + 1.3, y = -1.6, yend = -1.6,
             linewidth = 1, colour = cores[["accent_dark"]],
             arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
    annotate("text", x = n * cel + .8, y = -1.3, label = "truncar", size = 4.6,
             colour = cores[["accent_dark"]]) +
    tri_dist(ex$Dt, n * cel + 1.7, 0, cel = cel, cor_cel = cor,
             titulo = "matriz de vizinhança truncada") +
    annotate("text", x = 0, y = -(n + .6) * cel, hjust = 0, size = 5, lineheight = .95,
             label = sprintf("limiar = %g m (maior aresta da árvore geradora mínima)", ex$lim),
             colour = cores[["accent_dark"]]) +
    annotate("text", x = 0, y = -(n + 1.6) * cel, hjust = 0, size = 5, fontface = "bold",
             label = "distância pequena (≤ limiar): mantida", colour = pq) +
    annotate("text", x = n * cel + 1.7, y = -(n + 1.6) * cel, hjust = 0, size = 5, fontface = "bold",
             label = sprintf("distância grande: vira 4 × limiar = %g", 4 * ex$lim), colour = gr) +
    coord_equal(clip = "off") + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' 3º passo do dbMEM: PCoA da matriz truncada; os autovetores com autovalor
#' positivo são os dbMEMs
fig_dbmem_pcoa <- function(ex = dbmem_ex()) {
  d <- tibble(k = seq_along(ex$eig), eig = ex$eig) |>
    mutate(tipo = case_when(eig > 1e-8 ~ "positivo: dbMEM", eig < -1e-8 ~ "negativo", TRUE ~ "zero"))
  ggplot(d, aes(factor(k), eig, fill = tipo)) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]]) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("positivo: dbMEM" = vd, negativo = "#c0392b",
                                 zero = cores[["neutro"]]), name = NULL) +
    labs(x = "Autovetor da PCoA", y = "Autovalor",
         subtitle = sprintf("PCoA da matriz truncada (%d pontos): %d autovalores positivos",
                            ex$n, sum(d$eig > 1e-8))) +
    tema_aula(base = 16) + theme(legend.position = "top")
}

#' Redes de conexão entre pontos usadas nos MEMs, nas 70 parcelas dos ácaros
fig_redes_mem <- function() {
  e <- new.env()
  utils::data("mite.xy", package = "vegan", envir = e)
  xy <- as.matrix(e$mite.xy)
  arestas <- function(nb) {
    l <- lapply(seq_along(nb), \(i) if (nb[[i]][1] > 0)
      tibble(x = xy[i, 1], y = xy[i, 2], xend = xy[nb[[i]], 1], yend = xy[nb[[i]], 2]))
    bind_rows(l)
  }
  nb_del <- spdep::tri2nb(xy)
  nb_gab <- spdep::graph2nb(spdep::gabrielneigh(xy), sym = TRUE)
  nb_rel <- spdep::graph2nb(spdep::relativeneigh(xy), sym = TRUE)
  arv <- vegan::spantree(stats::dist(xy))
  mst <- tibble(x = xy[-1, 1], y = xy[-1, 2], xend = xy[arv$kid, 1], yend = xy[arv$kid, 2])
  d <- bind_rows(
    mutate(arestas(nb_del), rede = "Triangulação\nde Delaunay"),
    mutate(arestas(nb_gab), rede = "Grafo de\nGabriel"),
    mutate(arestas(nb_rel), rede = "Vizinhança\nrelativa"),
    mutate(mst, rede = "Árvore geradora\nmínima")) |>
    mutate(rede = factor(rede, unique(rede)))
  pts <- tibble(x = xy[, 1], y = xy[, 2])
  ggplot(d) +
    geom_segment(aes(x, y, xend = xend, yend = yend), colour = vd, linewidth = .4) +
    geom_point(data = pts, aes(x, y), size = 1.1, colour = cores[["accent_dark"]]) +
    facet_wrap(~rede, nrow = 1) +
    coord_equal() +
    labs(x = NULL, y = NULL, subtitle = "Do mais conectado ao menos conectado") +
    tema_aula(base = 15) + theme(axis.text = element_blank())
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

# ---- esquemas da partição de variância ----------------------------------------

cor_x <- function() cores[["verde"]]
cor_w <- function() cores[["ocre"]]

#' Partição de variância: Y explicada por dois conjuntos, X (ambiente) e W (espaço)
fig_varpart_ideia <- function() {
  cx <- bind_rows(mat(0, 0, "Y", w = 1.3, h = 2.4),
                  mat(3.4, 1, "X", w = 1.4, h = 1.3, cor = cor_x()),
                  mat(3.4, -1, "W", w = 1.4, h = 1.3, cor = cor_w()))
  setas <- tribble(~x, ~y, ~xend, ~yend, 2.65, 1, .7, .35, 2.65, -1, .7, -.35)
  diagrama(cx, setas, base = 5.4) +
    annotate("text", x = 4.3, y = c(1, -1), hjust = 0, size = 5.6, fontface = "bold",
             colour = c(cor_x(), cor_w()), label = c("ambiental", "espacial")) +
    geom_blank(data = tibble(x = 6, y = 0), aes(x, y))
}

# linha de regressão em caixas: Y ~ X (+ W)
linha_reg <- function(y, termos, rotulo, valor, x0 = 0, larg = .9, alt = .9) {
  cor_t <- c(X = cor_x(), W = cor_w())
  cx <- bind_rows(mat(x0, y, "Y", w = larg, h = alt, tam = 1.1),
                  lapply(seq_along(termos), \(i)
                    mat(x0 + 1.5 + (i - 1) * 1.5, y, termos[i], w = larg, h = alt,
                        cor = cor_t[[termos[i]]], tam = 1.1)))
  ops <- tibble(x = c(x0 + .75, if (length(termos) > 1) x0 + 2.25), y = y,
                l = c("~", if (length(termos) > 1) "+"))
  list(cx = cx, ops = ops,
       txt = tibble(x = x0 + 1.5 * length(termos) + 1, y = y, l = rotulo, v = valor))
}

#' Passos 1 a 3 da partição: as três regressões e os seus R² ajustados
fig_varpart_regressoes <- function(v = mite_varpart()) {
  f <- function(z) sub(".", ",", sprintf("%.3f", z), fixed = TRUE)
  l <- list(linha_reg(2.4, "X", "R[YX]^2", paste0("[a] + [b] = ", f(v$ab))),
            linha_reg(1.2, "W", "R[YW]^2", paste0("[b] + [c] = ", f(v$bc))),
            linha_reg(0, c("X", "W"), "R[YXW]^2", paste0("[a] + [b] + [c] = ", f(v$abc))))
  cx <- bind_rows(lapply(l, `[[`, "cx")); ops <- bind_rows(lapply(l, `[[`, "ops"))
  txt <- bind_rows(lapply(l, `[[`, "txt")); txt$x <- 4.6
  diagrama(cx, base = 5) +
    geom_text(data = ops, aes(x, y, label = l), size = 9, colour = "#c0392b", fontface = "bold") +
    geom_text(data = txt, aes(x, y, label = paste0(l, " == '", v, "'")), hjust = 0, size = 6.4,
              colour = "#c0392b", parse = TRUE) +
    annotate("text", x = -.9, y = c(2.4, 1.2, 0), label = paste0("Passo ", 1:3), hjust = 1,
             size = 5.2, colour = cores[["accent_dark"]], fontface = "bold") +
    geom_blank(data = tibble(x = c(-2.2, 11.4), y = c(-.6, 3)), aes(x, y))
}

#' Passos 4 e 5: as frações individuais por diferença entre os R²
fig_varpart_fracoes <- function(v = mite_varpart()) {
  f <- function(z) sub(".", ",", sprintf("%.3f", z), fixed = TRUE)
  mini <- function(x0, y, termos) {
    cor_t <- c(X = cor_x(), W = cor_w())
    bind_rows(mat(x0, y, "Y", w = .5, h = .5, tam = .75),
              lapply(seq_along(termos), \(i) mat(x0 + .8 * i, y, termos[i], w = .5, h = .5,
                                                 cor = cor_t[[termos[i]]], tam = .75)))
  }
  cx <- bind_rows(mini(1, 2.4, c("X", "W")), mini(3.6, 2.4, "W"),
                  mini(1, 1.2, "X"), mini(2.8, 1.2, "W"), mini(4.6, 1.2, c("X", "W")),
                  mini(1, 0, c("X", "W")), mini(3.6, 0, "X"))
  ops <- tibble(x = c(3.1, 2.3, 4.1, 3.1), y = c(2.4, 1.2, 1.2, 0), l = c("−", "+", "−", "−"))
  diagrama(cx, base = 5) +
    geom_text(data = ops, aes(x, y, label = l), size = 9, colour = "#c0392b", fontface = "bold") +
    annotate("text", x = .45, y = c(2.4, 1.2, 0), hjust = 1, size = 7, fontface = "bold",
             label = c("[a] =", "[b] =", "[c] ="), colour = c(cor_x(), "#8a7a45", cor_w())) +
    annotate("text", x = 6.6, y = c(2.4, 1.2, 0), hjust = 0, size = 6, fontface = "bold",
             label = paste("=", f(c(v$a, v$b, v$c))), colour = "#c0392b") +
    annotate("text", x = -.6, y = -1.2, hjust = 0, size = 6, fontface = "bold",
             colour = cores[["accent_dark"]],
             label = paste0("Passo 5:  [d] = 1 − ([a] + [b] + [c]) = ", f(v$d))) +
    annotate("text", x = .45, y = 3.2, hjust = 1, size = 5.2, fontface = "bold",
             colour = cores[["accent_dark"]], label = "Passo 4") +
    geom_blank(data = tibble(x = c(-1.4, 8.6), y = c(-1.5, 3.4)), aes(x, y))
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
