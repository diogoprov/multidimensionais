library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

vd <- cores_familia[["Ordenação restrita"]]

# ---- esquemas ---------------------------------------------------------------

# caixa de matriz para os diagramas (rótulo grande, sem moldura arredondada)
mat <- function(x, y, rotulo, w = 1.2, h = 1.5, cor = cores[["accent_dark"]], tam = 1.6)
  tibble(x, y, w, h, rotulo, cor, tam, face = "bold")

#' Análise indireta × direta de gradientes (inspirado na Fig. 10.4 de
#' Legendre & Legendre 2012)
fig_indireta_direta <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(
    mat(0, 2, "Y"), mat(2.2, 2, "Eixos de\nordenação", w = 1.6, tam = .9),
    mat(5.6, 2, "X"),
    mat(0, -.3, "Y", cor = vd), mat(5.6, -.3, "X", cor = vd))
  setas <- tribble(
    ~x,   ~y,   ~xend, ~yend,
    .65,  2,    1.35,  2,
    3.05, 2,    4.95,  2,
    .65,  -.3,  4.95,  -.3)
  diagrama(cx, setas, base = 5) +
    annotate("text", x = 4, y = 2.35, label = "comparação indireta\n(correlação, regressão)",
             size = 4.2, colour = ne, lineheight = .9) +
    annotate("text", x = 2.8, y = 0, label = "comparação direta: análise canônica",
             size = 4.4, colour = vd, fontface = "bold") +
    annotate("text", x = -1.1, y = c(2, -.3), label = c("Indireta", "Direta"), angle = 90,
             size = 5, fontface = "bold", colour = c(ne, vd))
}

#' Análises canônicas assimétricas e simétricas
fig_assim_sim <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(mat(0, 1.6, "Y", w = 1.1, h = 1.1), mat(3.6, 1.6, "X", w = 1.1, h = 1.1),
                  mat(0, -.4, "Y", w = 1.1, h = 1.1), mat(3.6, -.4, "X", w = 1.1, h = 1.1))
  setas <- tribble(~x, ~y, ~xend, ~yend, 3.0, 1.6, .6, 1.6)
  diagrama(cx, setas, base = 5) +
    annotate("segment", x = .6, xend = 3.0, y = -.4, yend = -.4, linewidth = .9,
             colour = ne, arrow = arrow(length = unit(.18, "cm"), ends = "both", type = "closed")) +
    annotate("text", x = 1.8, y = 1.85, label = "X explica Y", size = 4.2, colour = vd,
             fontface = "bold") +
    annotate("text", x = 1.8, y = -.15, label = "o que há em comum", size = 4.2, colour = ne,
             fontface = "bold") +
    annotate("text", x = 4.4, y = 1.6, hjust = 0, size = 4.4, lineheight = .95, colour = vd,
             label = "Assimétricas\nY com todas as variáveis: RDA, CCA\nY com uma classificação: LDA") +
    annotate("text", x = 4.4, y = -.4, hjust = 0, size = 4.2, lineheight = .95, colour = ne,
             label = "Simétricas\ncorrelação canônica, co-inércia,\nProcrustes") +
    geom_blank(data = tibble(x = 8.6, y = 0), aes(x, y))
}

#' Regressão múltipla (Y univariada) × RDA (Y multivariada)
fig_regressao_rda <- function() {
  cx <- bind_rows(mat(0, 1.5, "y", w = .5, h = 1.3), mat(2.2, 1.5, "X", w = 1.6, h = 1.3),
                  mat(0, -.5, "Y", w = 1.6, h = 1.3, cor = vd), mat(2.6, -.5, "X", w = 1.6, h = 1.3, cor = vd))
  diagrama(cx, base = 5) +
    annotate("text", x = c(.95, 1.2), y = c(1.5, -.5), label = "~", size = 14,
             colour = "#c0392b", fontface = "bold") +
    annotate("text", x = 3.4, y = 1.5, hjust = 0, size = 4.6,
             label = "Regressão múltipla:\nresposta univariada (vetor)") +
    annotate("text", x = 3.8, y = -.5, hjust = 0, size = 4.6, colour = vd,
             label = "RDA:\nresposta multivariada (matriz)")
}

#' A RDA em dois passos (inspirado na Fig. 11.2 de Legendre & Legendre 2012)
fig_rda_passos <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                              ~cor, ~face,
    0,    1.6,   2.2, 1,   "Y\n(variáveis centradas)",                          ne,   "bold",
    2.6,  1.6,   2.2, 1,   "X\n(variáveis centradas)",                          ne,   "bold",
    1.3,  0,     2.6, 1,   "Valores ajustados\nŶ = X[X'X]⁻¹X'Y",                vd,   "plain",
    5,    0,     2.2, 1,   "PCA de Ŷ:\nautovetores U",                          vd,   "plain",
    8.4,  1.6,   2.6, 1,   "F = YU\nescores dos locais\n(espaço de Y)",          vd,   "plain",
    8.4,  0,     2.6, 1,   "Z = ŶU\nescores ajustados\n(espaço de X)",           vd,   "plain",
    1.3,  -1.6,  2.6, 1,   "Resíduos\nYres = Y − Ŷ",                            ne,   "plain",
    5,    -1.6,  2.2, 1,   "PCA dos resíduos",                                  ne,   "plain"
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.3,   1.1,   1.3,   .55,
    2.6,   1.1,   1.9,   .55,
    2.6,   0,     3.85,  0,
    6.1,   .2,    7.05,  1.5,
    6.1,   0,     7.05,  0,
    1.3,   -.5,   1.3,   -1.05,
    2.6,   -1.6,  3.85,  -1.6
  )
  diagrama(cx, setas, base = 4.2) +
    annotate("text", x = -.3, y = .75, label = "Passo 1:\nregressão de cada\ncoluna de Y em X",
             size = 3.8, hjust = 1, colour = ne, lineheight = .9) +
    annotate("text", x = 5, y = .75, label = "Passo 2", size = 4.2, colour = vd, fontface = "bold")
}

#' Valores ajustados de uma regressão múltipla: o plano que melhor se ajusta
#' aos pontos num espaço de duas preditoras (dados simulados, vista 2D)
fig_ajustados <- function() {
  set.seed(4)
  n <- 40
  x <- runif(n, 0, 10)
  y <- 2 + .7 * x + rnorm(n, 0, 1.4)
  fit <- lm(y ~ x)
  d <- tibble(x, y, yhat = fitted(fit))
  ggplot(d, aes(x, y)) +
    geom_segment(aes(xend = x, yend = yhat), colour = cores[["neutro"]], linetype = "22") +
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], colour = "#c0392b", linewidth = 1.1) +
    geom_point(shape = 21, fill = cores[["accent"]], colour = "white", size = 3) +
    geom_point(aes(y = yhat), shape = 21, fill = "#c0392b", colour = "white", size = 2.4) +
    annotate("text", x = 9.6, y = 2.2, hjust = 1, size = 4.6, colour = "#c0392b",
             label = "valores ajustados Ŷ (sobre a reta)") +
    labs(x = expression(X[1]), y = expression(Y[1]),
         subtitle = "Com muitas preditoras, a reta vira um hiperplano") +
    tema_aula(base = 15)
}

# chave (brace) vertical, com a ponta voltada para a direita, de y0 a y1 em x
chave_v <- function(x, y0, y1, larg = .18, cor = cores[["accent_dark"]]) {
  ym <- (y0 + y1) / 2
  t <- seq(0, 1, length.out = 40)
  meia <- function(a, b) tibble(px = x + larg / 2 * sin(pi * t)^.6 * 0 + larg / 2 * (1 - cos(pi * t)) / 2,
                                py = a + (b - a) * t)
  d <- bind_rows(
    tibble(px = x + larg * c(0, .5, .5), py = c(y0, y0 + .08, ym - .08)),
    tibble(px = x + larg * c(1), py = ym),
    tibble(px = x + larg * c(.5, .5, 0), py = c(ym + .08, y1 - .08, y1)))
  annotate("path", x = d$px, y = d$py, colour = cor, linewidth = .8, linejoin = "round")
}

# chave horizontal, com a ponta voltada para baixo, de x0 a x1 em y
chave_h <- function(y, x0, x1, alt = .18, cor = cores[["accent_dark"]]) {
  xm <- (x0 + x1) / 2
  d <- tibble(px = c(x0, x0 + .08, xm - .08, xm, xm + .08, x1 - .08, x1),
              py = y - alt * c(0, .5, .5, 1, .5, .5, 0))
  annotate("path", x = d$px, y = d$py, colour = cor, linewidth = .8, linejoin = "round")
}

# matriz desenhada como tabela: cabeçalho de colunas, rótulos de linhas e valores
tabela_mat <- function(x0, y0, valores, col_rot, lin_rot, cor, dx = .62, dy = .38,
                       tam = 4.6, titulo = NULL) {
  nl <- nrow(valores); nc <- ncol(valores)
  cel <- expand.grid(i = seq_len(nl), j = seq_len(nc))
  cel$x <- x0 + (cel$j - .5) * dx
  cel$y <- y0 - (cel$i - .5) * dy
  cel$v <- valores[cbind(cel$i, cel$j)]
  list(
    annotate("rect", xmin = x0, xmax = x0 + nc * dx, ymin = y0 - nl * dy, ymax = y0,
             fill = "white", colour = cor, linewidth = 1),
    annotate("segment", x = x0 + seq_len(nc - 1) * dx, xend = x0 + seq_len(nc - 1) * dx,
             y = y0, yend = y0 - nl * dy, colour = cor, linewidth = .2, alpha = .5),
    annotate("text", x = cel$x, y = cel$y, label = cel$v, size = tam, colour = cores[["accent_dark"]]),
    annotate("text", x = x0 + (seq_len(nc) - .5) * dx, y = y0 + .08, label = col_rot,
             size = tam * .95, angle = 35, hjust = 0, vjust = 0, colour = cor, fontface = "bold"),
    annotate("text", x = x0 - .08, y = y0 - (seq_len(nl) - .5) * dy, label = lin_rot,
             size = tam * .95, hjust = 1, colour = cores[["neutro"]]),
    if (!is.null(titulo))
      annotate("text", x = x0 + nc * dx + .3, y = y0 - nl * dy / 2, label = titulo,
               size = 13, fontface = "bold", colour = cor, hjust = 0)
  )
}

#' Girinos e poças: as variáveis ambientais formam X e as espécies formam Y
#' (redesenhado de slide de Thiago Gonçalves-Souza; silhuetas do PhyloPic)
fig_girinos_matrizes <- function() {
  ne <- cores[["accent_dark"]]
  az <- "#1f5fa8"; la <- "#d9822b"
  sil <- function(f) png::readPNG(here::here("assets/phylopic", f))
  pocas <- paste("poça", 1:5)
  vx <- matrix(c("0,8", "0,3", "0,5", "0,9", "0,1",
                 "arb.", "capim", "arb.", "árv.", "capim",
                 "20", "75", "40", "10", "90"), 5)
  vy <- matrix(c(1, 0, 1, 1, 0,  0, 1, 0, 1, 1,  1, 1, 0, 0, 1,  0, 0, 1, 1, 0), 5)
  rot <- tibble(x = 1.1, y = c(4.45, 3.95, 3.35),
                rotulo = c("Tipos de vegetação", "Densidade da vegetação",
                           "% da superfície\nda poça exposta"),
                cor = c(az, ne, la))
  g <- ggplot() +
    geom_label(data = rot, aes(x, y, label = rotulo), colour = rot$cor, fill = "white",
               size = 5, fontface = "bold", lineheight = .9, label.r = unit(0, "pt"),
               linewidth = .8, label.padding = unit(5, "pt")) +
    chave_v(2.35, 3.0, 4.75) +
    annotate("segment", x = 2.6, xend = 3.15, y = 3.875, yend = 3.875, colour = ne,
             linewidth = .8, arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    tabela_mat(4.0, 4.7, vx, c("densidade", "tipo", "% exposta"), pocas, ne, dx = .75,
               titulo = "X") +
    annotation_raster(sil("hylidae.png"), .2, 1.0, 1.0, 1.75) +
    annotation_raster(sil("bufonidae.png"), 1.2, 2.0, .15, .95) +
    annotation_raster(sil("leptodactylidae.png"), .2, 1.0, -.35, .35) +
    annotate("text", x = c(1.05, 2.05, 1.05), y = c(1.65, .9, .3), label = c("sp1", "sp2", "sp3"),
             size = 4.4, fontface = "italic", colour = ne) +
    chave_v(2.35, -.4, 1.8) +
    annotate("segment", x = 2.6, xend = 3.15, y = .7, yend = .7, colour = vd,
             linewidth = .8, arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    tabela_mat(4.0, 1.55, vy, paste0("sp", 1:4), pocas, vd, dx = .6, titulo = "Y") +
    annotate("text", x = 4.0, y = -.5, hjust = 0, size = 4.2, colour = cores[["neutro"]],
             label = "mesmas poças, na mesma ordem, nas duas matrizes") +
    coord_equal(xlim = c(0, 8), ylim = c(-.7, 5.2), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  g
}

#' Passo 2 da RDA: a PCA de Ŷ dá autovalores e autovetores; a ordenação
#' reduz o espaço aos eixos mais importantes
fig_rda_passo2_pca <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(
    mat(0, 0, "Ŷ", w = .8, h = 2.2, cor = vd),
    mat(2.6, 0, "Autovalores\n\nvariação\nde cada eixo\ncanônico", w = 1.8, h = 2.2, tam = .9),
    mat(4.6, 0, "Autovetores U\n\npesos das\nespécies", w = 1.8, h = 2.2, tam = .9),
    mat(6.6, 0, "Escores\n\nlocais,\nespécies,\npreditoras", w = 1.8, h = 2.2, tam = .9))
  setas <- tribble(~x, ~y, ~xend, ~yend, .5, 0, 1.6, 0)
  diagrama(cx, setas, base = 5.2) +
    annotate("text", x = 1.05, y = .25, label = "PCA", size = 5.6, fontface = "bold", colour = vd) +
    annotate("segment", x = 0, xend = 0, y = -1.2, yend = -1.85, colour = "#c0392b",
             linewidth = 2.2) +
    annotate("segment", x = 0, xend = 1.4, y = -1.85, yend = -1.85, colour = "#c0392b",
             linewidth = 2.2, arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("text", x = 1.6, y = -1.85, hjust = 0, size = 5, lineheight = .95, colour = ne,
             label = "redução do espaço multidimensional: objetos, espécies e\npreditoras nos poucos eixos que concentram a variação explicada por X")
}

#' Passo 2 da RDA: os dois tipos de escores dos locais, F = YU (site scores,
#' "wa") e Z = ŶU (site constraints, "lc")
fig_rda_escores <- function() {
  ne <- cores[["accent_dark"]]
  cx <- bind_rows(
    mat(0, 1.5, "Y", w = 1.5, h = 1.6),
    mat(2.4, 1.5, "Autovetores\nU", w = 1.5, h = 1.6, tam = 1),
    mat(0, -1.4, "Ŷ", w = 1.5, h = 1.6, cor = vd),
    mat(2.4, -1.4, "Autovetores\nU", w = 1.5, h = 1.6, tam = 1, cor = vd))
  diagrama(cx, base = 5.2) +
    annotate("text", x = 1.2, y = c(1.5, -1.4), label = "×", size = 10, colour = "#c0392b",
             fontface = "bold") +
    annotate("segment", x = 3.4, xend = 4.1, y = c(1.5, -1.4), yend = c(1.5, -1.4),
             colour = ne, linewidth = .8, arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
    annotate("text", x = 4.3, y = 1.5, hjust = 0, size = 5, lineheight = .95, colour = ne,
             label = "F = YU\ncombinação linear dos dados observados\nno vegan: site scores (\"wa\")") +
    annotate("text", x = 4.3, y = -1.4, hjust = 0, size = 5, lineheight = .95, colour = vd,
             label = "Z = ŶU\ncombinação linear dos valores ajustados\nno vegan: site constraints (\"lc\")") +
    chave_h(-2.35, -.75, 3.15, cor = vd) +
    annotate("text", x = 1.2, y = -2.85, label = "distribuição das espécies predita pelo ambiente",
             size = 4.6, colour = vd, fontface = "italic") +
    geom_blank(data = tibble(x = c(-.8, 9.6), y = c(-3, 2.4)), aes(x, y))
}

#' Escores "wa" (F = YU) e "lc" (Z = ŶU) dos locais na RDA do Doubs: cada
#' segmento liga a posição observada à posição predita pelo ambiente
fig_wa_lc <- function(ord) {
  wa <- escores(ord, ref = "Satr", scaling = 1)$sites
  lc <- vegan::scores(ord, display = "lc", choices = 1:2, scaling = 1)
  sg <- sinais_eixos(vegan::scores(ord, display = "species", choices = 1:2), "Satr")
  lc <- sweep(lc, 2, sg, `*`)
  d <- tibble(rotulo = wa$rotulo, w1 = wa[[2]], w2 = wa[[3]], l1 = lc[, 1], l2 = lc[, 2])
  ggplot(d) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .3) +
    geom_segment(aes(w1, w2, xend = l1, yend = l2), colour = cores[["neutro"]], linewidth = .4) +
    geom_point(aes(w1, w2), shape = 21, fill = cores[["accent"]], colour = "white", size = 3.2) +
    geom_point(aes(l1, l2), shape = 21, fill = vd, colour = "white", size = 3.2) +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4, size = 5, lineheight = .95,
             label = "azul: F = YU (\"wa\")", colour = cores[["accent"]], fontface = "bold") +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 3.2, size = 5,
             label = "verde: Z = ŶU (\"lc\")", colour = vd, fontface = "bold") +
    coord_equal() +
    labs(x = "RDA1", y = "RDA2", subtitle = "Peixes do Doubs (Hellinger), scaling 1") +
    tema_aula(base = 15)
}

# ---- Doubs ------------------------------------------------------------------

#' Peixes do Doubs (Hellinger) e variáveis ambientais padronizadas, sem o local
#' 8 e sem dfs (distância da nascente), como em Borcard et al. (2018)
doubs_rda_dados <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  spe <- e$doubs$fish[-8, ]
  env <- as.data.frame(scale(e$doubs$env[-8, -1]))
  list(spe = spe, spe.h = vegan::decostand(spe, "hellinger"), env = env)
}

doubs_rda <- function() {
  d <- doubs_rda_dados()
  vegan::rda(d$spe.h ~ ., d$env)
}

doubs_cca <- function() {
  d <- doubs_rda_dados()
  vegan::cca(d$spe ~ ., d$env)
}

#' Triplot de uma ordenação restrita do vegan
#'
#' @param ord rda() ou cca()
#' @param scaling 1 ou 2
#' @param ref espécie de referência para o sinal dos eixos
#' @param n_sp espécies mostradas (as de maior escore nos dois eixos)
#' @param projecao local cuja projeção sobre a seta de `variavel` é desenhada
fig_triplot <- function(ord, scaling = 2, ref = "Satr", n_sp = 9, projecao = NULL,
                        variavel = "oxy", base = 13) {
  e <- escores(ord, ref = ref, scaling = scaling)
  si <- e$sites; sp <- e$species; bp <- e$biplot
  nm <- names(si)[2:3]
  names(si)[2:3] <- names(sp)[2:3] <- names(bp)[2:3] <- c("e1", "e2")
  sp <- sp |> mutate(r = sqrt(e1^2 + e2^2)) |> arrange(desc(r)) |> head(n_sp)
  k <- .9 * max(abs(c(si$e1, si$e2, sp$e1, sp$e2))) / max(abs(c(bp$e1, bp$e2)))
  bp <- bp |> mutate(e1 = e1 * k, e2 = e2 * k)
  amp <- diff(range(c(si$e2, bp$e2, sp$e2)))
  bp <- bp |> mutate(lx = e1 * 1.1, ly = separa_rotulos(lx, e2 * 1.1, .2 * amp, .06 * amp))
  g <- ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_text(data = si, aes(e1, e2, label = rotulo), colour = cores[["neutro"]], size = 3.4) +
    geom_segment(data = bp, aes(0, 0, xend = e1, yend = e2), colour = vd, linewidth = .9,
                 arrow = arrow(length = unit(.16, "cm"))) +
    geom_text(data = bp, aes(lx, ly, label = rotulo), colour = vd, fontface = "bold", size = 4.2) +
    geom_text(data = sp, aes(e1, e2, label = rotulo), colour = "#c0392b", fontface = "italic",
              size = 3.8)
  if (!is.null(projecao)) {
    v <- unlist(bp[bp$rotulo == variavel, c("e1", "e2")]); v <- v / sqrt(sum(v^2))
    s <- unlist(sp[sp$rotulo == projecao, c("e1", "e2")]); t <- sum(s * v)
    g <- g + annotate("segment", x = s[1], y = s[2], xend = t * v[1], yend = t * v[2],
                      colour = cores[["ocre"]], linetype = "22", linewidth = 1) +
      annotate("point", x = t * v[1], y = t * v[2], colour = cores[["ocre"]], size = 3)
  }
  g + coord_equal() +
    labs(x = nm[1], y = nm[2], subtitle = paste("Scaling", scaling)) +
    tema_aula(base = base)
}

#' Triplot esquemático montado em camadas (redesenhado de slides de Thiago
#' Gonçalves-Souza): 1 locais; 2 + espécies; 3 + preditoras; 4 ângulo entre
#' setas (scaling 2); 5 distância entre locais (scaling 1); 6 projeção de uma
#' espécie sobre uma seta (scaling 1)
fig_triplot_camadas <- function(passo = 1) {
  ne <- cores[["accent_dark"]]; ve <- "#c0392b"; az <- "#1f5fa8"; dest <- "#16a34a"
  si <- tibble(r = paste("local", 1:8),
               x = c(-.85, -.85, -.85, .68, .78, -.3, .12, .12),
               y = c(.8, .3, -.6, -.28, .3, .06, -.88, .88))
  sp <- tibble(r = paste0("sp", 1:6),
               x = c(-.6, -.18, .32, .36, -.18, -.12),
               y = c(-.3, .64, .74, -.3, -.2, -.78))
  bp <- tibble(r = c("Temp", "Umidade", "Luminosidade", "pH"),
               x = c(-.42, .62, .58, .14), y = c(.42, .5, -.04, -.62))
  g <- ggplot() +
    annotate("rect", xmin = -1.1, xmax = 1.1, ymin = -1.1, ymax = 1.1, fill = NA,
             colour = ne, linewidth = .5) +
    geom_hline(yintercept = 0, colour = ne, linewidth = .4) +
    geom_vline(xintercept = 0, colour = ne, linewidth = .4) +
    geom_text(data = si, aes(x, y, label = r), size = 5, fontface = "italic", colour = ne)
  if (passo >= 2)
    g <- g + geom_text(data = sp, aes(x, y, label = r), size = 5.4, fontface = "bold", colour = az)
  if (passo >= 3)
    g <- g + geom_segment(data = bp, aes(0, 0, xend = x, yend = y), colour = ve, linewidth = 1.2,
                          arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
      geom_label(data = bp, aes(x * 1.13, y * 1.13 + c(0, .05, 0, -.03), label = r), size = 5,
                 fontface = "bold", colour = ve, fill = alpha(cores[["papel"]], .85),
                 linewidth = 0, label.padding = unit(1, "pt"))
  if (passo == 4) {
    a <- seq(atan2(-.04, .58), atan2(-.62, .14), length.out = 30)
    g <- g + annotate("path", x = .22 * cos(a), y = .22 * sin(a), colour = dest, linewidth = 1.6)
  }
  if (passo == 5)
    g <- g + annotate("segment", x = -.78, y = .74, xend = .06, yend = .84, colour = dest,
                      linewidth = 1.4, arrow = arrow(length = unit(.22, "cm"), ends = "both",
                                                     type = "closed"))
  if (passo == 6) {
    v <- c(.14, -.62) / sqrt(.14^2 + .62^2); s4 <- c(.36, -.3); t <- sum(s4 * v)
    g <- g + annotate("segment", x = s4[1] - .07, y = s4[2], xend = t * v[1], yend = t * v[2],
                      colour = dest, linewidth = 1.4, linetype = "22") +
      annotate("point", x = t * v[1], y = t * v[2], colour = dest, size = 3.5)
  }
  g + coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), expand = FALSE, clip = "off") +
    labs(x = "RDA 1", y = "RDA 2") +
    theme_void(base_size = 16) +
    theme(axis.title.x = element_text(colour = ne, margin = margin(t = 6)),
          axis.title.y = element_text(colour = ne, angle = 90, margin = margin(r = 6)),
          plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' RDA com uma preditora categórica: as classes viram centroides (pontos), as
#' quantitativas continuam como setas (vegan::dune, Hellinger; manejo e A1)
dune_rda <- function() {
  e <- new.env()
  utils::data("dune", "dune.env", package = "vegan", envir = e)
  y <- vegan::decostand(e$dune, "hellinger")
  vegan::rda(y ~ A1 + Management, data = e$dune.env)
}

fig_triplot_categorica <- function(ord, scaling = 1) {
  sc <- vegan::scores(ord, choices = 1:2, scaling = scaling,
                      display = c("sites", "species", "bp", "cn"))
  s <- sinais_eixos(sc$species, "Lolipere")
  f <- function(m) { m <- sweep(as.matrix(m), 2, s, `*`); tibble::as_tibble(m, rownames = "rotulo") |>
    setNames(c("rotulo", "e1", "e2")) }
  si <- f(sc$sites); sp <- f(sc$species); bp <- f(sc$biplot); cn <- f(sc$centroids)
  env <- local({ e <- new.env(); utils::data("dune.env", package = "vegan", envir = e); e$dune.env })
  si$manejo <- env$Management
  bp <- bp |> filter(rotulo == "A1")
  cn$rotulo <- sub("Management", "", cn$rotulo)
  nomes <- c(BF = "BF (biológico)", HF = "HF (hobby)", NM = "NM (natural)", SF = "SF (padrão)")
  cn$nome <- nomes[cn$rotulo]
  sp <- sp |> mutate(r = sqrt(e1^2 + e2^2)) |> arrange(desc(r)) |> head(8)
  k <- .8 * max(abs(c(si$e1, si$e2))) / max(abs(c(bp$e1, bp$e2)))
  bp <- bp |> mutate(e1 = e1 * k, e2 = e2 * k)
  pal <- c(BF = "#d9822b", HF = "#7c3aed", NM = "#16a34a", SF = "#1f5fa8")
  eig <- ord$CCA$eig / ord$tot.chi
  ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_segment(data = left_join(si, transmute(cn, manejo = rotulo, c1 = e1, c2 = e2),
                                  by = "manejo"),
                 aes(e1, e2, xend = c1, yend = c2, colour = manejo), linewidth = .35, alpha = .6) +
    geom_point(data = si, aes(e1, e2, colour = manejo), size = 2.6) +
    geom_text(data = sp, aes(e1, e2, label = rotulo), colour = "#c0392b", fontface = "italic",
              size = 4.4) +
    geom_segment(data = bp, aes(0, 0, xend = e1, yend = e2), colour = vd, linewidth = 1.1,
                 arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
    geom_text(data = bp, aes(e1, e2, label = "A1 (espessura\ndo horizonte A)"),
              colour = vd, fontface = "bold", size = 4.4, lineheight = .9, hjust = 1.05,
              vjust = .2) +
    geom_point(data = cn, aes(e1, e2, fill = rotulo), shape = 23, size = 6, colour = "white") +
    ggrepel::geom_label_repel(data = cn, aes(e1, e2, label = nome, colour = rotulo), size = 4.4,
                              fontface = "bold", fill = alpha(cores[["papel"]], .9),
                              label.size = 0, box.padding = .6, min.segment.length = 0,
                              seed = 1) +
    scale_colour_manual(values = pal, guide = "none") +
    scale_fill_manual(values = pal, guide = "none") +
    coord_equal(clip = "off") +
    labs(x = sprintf("RDA1 (%s)", pct(eig[1])), y = sprintf("RDA2 (%s)", pct(eig[2])),
         subtitle = paste0("Prados de dunas (vegan::dune), Hellinger, scaling ", scaling)) +
    tema_aula(base = 15)
}

#' Duplos zeros na RDA: PCoA de três locais (x1 e x2 compartilham todas as
#' espécies; x2 e x3, nenhuma; exemplo de Legendre & Legendre 2012, Aula 2)
#' com a distância euclidiana e com a Hellinger
fig_rda_hellinger <- function() {
  x <- rbind(x1 = c(0, 4, 8), x2 = c(0, 1, 1), x3 = c(1, 0, 0))
  ds <- list("Euclidiana (dados brutos)" = stats::dist(x),
             "Hellinger" = stats::dist(vegan::decostand(x, "hellinger")))
  pain <- lapply(names(ds), \(n) {
    pc <- stats::cmdscale(ds[[n]], k = 2)
    if (ncol(pc) < 2) pc <- cbind(pc, 0)
    pc <- orienta_coord(pc, "x3")
    d <- tibble(local = rownames(x), e1 = pc[, 1], e2 = pc[, 2])
    r <- max(abs(c(d$e1, d$e2))) * 1.3
    ggplot(d, aes(e1, e2)) +
      geom_point(size = 4, colour = vd) +
      geom_text(aes(label = local, vjust = ifelse(local == "x1", 2, -1)), size = 5.4) +
      coord_equal(xlim = c(-r, r), ylim = c(-r, r)) +
      labs(title = n, x = NULL, y = NULL) +
      tema_aula(base = 15) +
      theme(axis.text = element_blank(), plot.title = element_text(hjust = .5))
  })
  patchwork::wrap_plots(pain, nrow = 1)
}

#' Gradiente curto (RDA) × longo (CCA): a resposta unimodal de uma espécie e
#' a aproximação linear num trecho curto e num trecho longo do gradiente
fig_gradiente <- function() {
  x <- seq(0, 10, length.out = 300)
  y <- exp(-(x - 5)^2 / 2)
  painel <- function(a, b, titulo) {
    sel <- x >= a & x <= b
    fit <- lm(y[sel] ~ x[sel])
    ggplot() +
      geom_line(aes(x, y), linewidth = 1.1, colour = cores[["texto"]]) +
      annotate("segment", x = a, xend = b, y = coef(fit)[1] + coef(fit)[2] * a,
               yend = coef(fit)[1] + coef(fit)[2] * b, colour = cores[["ocre"]], linewidth = 1.6) +
      annotate("segment", x = a, xend = b, y = -.06, yend = -.06, colour = "#c0392b",
               linewidth = 2) +
      labs(title = titulo, x = "Gradiente ambiental", y = "Abundância") +
      tema_aula(base = 14, conceitual = TRUE)
  }
  painel(5.2, 7.2, "Gradiente curto: RDA") + painel(1, 9, "Gradiente longo: CCA")
}

#' Escolha da CCA (inspirado na Fig. 21.1 de McCune & Grace 2002)
fig_escolha_cca <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                                         ~cor, ~face,
    0,    1,     3,   1.2, "O interesse é só a parte\nda estrutura da comunidade\nligada às variáveis medidas?", ne, "plain",
    4.3,  1,     3,   1.2, "Que relação as espécies\ntêm com as variáveis\nexplanatórias?",   ne,   "plain",
    7.6,  1.6,   1.8, .7,  "CCA",                                                           vd,   "bold",
    7.6,  .4,    1.8, .7,  "RDA",                                                           vd,   "bold",
    0,    -.8,   2.4, .7,  "Ordenação irrestrita",                                          ne,   "bold"
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.5,   1,     2.75,  1,
    5.8,   1.2,   6.65,  1.6,
    5.8,   .8,    6.65,  .4,
    0,     .4,    0,     -.4)
  diagrama(cx, setas, base = 4.2) +
    annotate("text", x = 2.1, y = 1.2, label = "sim", size = 4.2, colour = ne) +
    annotate("text", x = .25, y = 0, label = "não", size = 4.2, colour = ne, hjust = 0) +
    annotate("text", x = 6.2, y = c(1.62, .32), label = c("unimodal", "linear"), size = 4,
             colour = ne)
}
