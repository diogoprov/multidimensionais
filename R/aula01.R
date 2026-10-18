library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))

ne <- cores[["accent_dark"]]

seta_v <- function(cx, i, j) tibble(x = cx$x[i], y = cx$y[i] - cx$h[i] / 2,
                                    xend = cx$x[j], yend = cx$y[j] + cx$h[j] / 2 + .05)

# ---- uma boa pergunta --------------------------------------------------------

#' Da pergunta às predições (inspirado no fluxograma do cap. 2 de Ruxton &
#' Colegrave 2016)
fig_pergunta_fluxo <- function() {
  cx <- tribble(
    ~x,  ~y,    ~w,  ~h,  ~rotulo,
    0,   5,     5.6, .7,  "Formule uma pergunta clara",
    0,   4,     5.6, .7,  "Formule hipóteses que respondam à pergunta",
    0,   3,     5.6, .7,  "Formule predições a partir das hipóteses",
    0,   2,     5.6, .7,  "As predições decorrem logicamente das hipóteses?",
    0,   1,     5.6, .7,  "As predições são testáveis?",
    0,   0,     5.6, .7,  "Cada predição é o teste mais forte possível?",
    0,   -1,    5.6, .7,  "Siga para o delineamento"
  )
  setas <- bind_rows(lapply(1:6, \(i) seta_v(cx, i, i + 1)))
  volta <- tibble(x = c(2.8, 2.8, 2.8), y = c(2, 1, 0), xend = 3.2, yend = c(2, 1, 0))
  diagrama(cx, setas, base = 4.2) +
    geom_segment(data = volta, aes(x, y, xend = xend, yend = yend), colour = "#c0392b",
                 linewidth = .7) +
    annotate("segment", x = 3.2, xend = 3.2, y = 0, yend = 3, colour = "#c0392b", linewidth = .7) +
    annotate("segment", x = 3.2, xend = 2.85, y = 3, yend = 3, colour = "#c0392b", linewidth = .7,
             arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    annotate("text", x = 3.3, y = 1.5, label = "não:\nreformule", hjust = 0, size = 4,
             colour = "#c0392b", lineheight = .9) +
    annotate("text", x = .15, y = c(1.5, .5, -.5), label = "sim", hjust = 0, size = 3.6,
             colour = ne)
}

#' Fluxograma da pergunta sobre nitrogênio e crescimento de Pinus
fig_hipotese_pinus <- function() {
  cx <- tribble(
    ~x,  ~y, ~w,  ~h,  ~rotulo,                                   ~cor,
    0,   0,  3.2, 1.1, "Quantidade de\nnitrogênio no solo",       cores[["verde"]],
    5.4, 0,  3.2, 1.1, "Taxa de crescimento\nper capita de Pinus", cores[["accent"]]
  )
  diagrama(cx, tibble(x = 1.65, y = 0, xend = 3.7, yend = 0), base = 5) +
    annotate("text", x = 2.7, y = .3, label = "efeito positivo", size = 5, colour = ne) +
    annotate("text", x = c(0, 5.4), y = -.85, label = c("preditora (X)", "resposta (Y)"),
             size = 4.6, colour = c(cores[["verde"]], cores[["accent"]]), fontface = "italic")
}

#' Do estudo observacional ao experimento (inspirado em James & McCulloch 1990)
fig_observacao_experimento <- function() {
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,
    3,    3,     2.4, .7,  "Observações",
    1,    1.9,   2.6, .8,  "Análise exploratória\nde dados",
    5,    1.9,   2.6, .8,  "Informação sobre\ncausas",
    1,    .8,    2.6, .7,  "Modelo descritivo",
    3,    -.3,   2.4, .7,  "Modelo causal",
    1,    -1.4,  3,   .8,  "Estudo observacional\n(quase experimental)",
    5,    -1.4,  3,   .8,  "Experimento controlado\nde laboratório ou campo"
  )
  cx$cor <- ifelse(seq_len(nrow(cx)) == 2, "#c0392b", ne)
  setas <- bind_rows(seta_v(cx, 1, 2), seta_v(cx, 2, 4), seta_v(cx, 4, 5), seta_v(cx, 3, 5),
                     seta_v(cx, 5, 6), seta_v(cx, 5, 7),
                     tibble(x = 4.2, y = 3, xend = 5, yend = 2.35))
  diagrama(cx, setas, base = 4.2)
}

# ---- univariado × multivariado ------------------------------------------------

#' Matriz de dados ecológicos típica: espécies e variáveis ambientais em oito
#' áreas (o exemplo dos slides originais)
matrizes_exemplo <- function() {
  sp <- data.frame(Sp1 = c(2, 10, 18, 6, 0, 0, 0, 1), Sp2 = c(12, 0, 19, 21, 0, 0, 0, 0),
                   Sp3 = c(14, 11, 10, 9, 1, 2, 0, 1), Sp4 = c(0, 0, 1, 0, 18, 9, 19, 21),
                   Spn = c(2, 15, 19, 8, 0, 0, 0, 0), row.names = paste("Área", 1:8))
  amb <- data.frame(Temp = c(2, 10, 18, 6, 0, 0, 0, 1), pH = c(12, 0, 19, 21, 0, 0, 0, 0),
                    O2 = c(14, 11, 10, 9, 1, 2, 0, 1), CO2 = c(0, 0, 1, 0, 18, 9, 19, 21),
                    row.names = paste("Área", 1:8))
  list(sp = sp, amb = amb)
}

#' Tipos de análise conforme a estrutura dos dados (inspirado num slide de
#' Adriano S. Melo)
fig_tipos_analise <- function() {
  mat <- function(x0, y0, nc, nr = 5, cor = cores[["accent"]]) {
    expand_grid(i = seq_len(nr), j = seq_len(nc)) |>
      mutate(x = x0 + (j - 1) * .22, y = y0 - (i - 1) * .16, cor = cor)
  }
  tri <- function(x0, y0, n = 5, cor = cores[["accent"]]) {
    expand_grid(i = seq_len(n), j = seq_len(n)) |> filter(j <= i) |>
      mutate(x = x0 + (j - 1) * .22, y = y0 - (i - 1) * .16, cor = cor)
  }
  linhas <- tribble(
    ~y,   ~titulo,                                  ~exemplo,
    5,    "Univariadas: y ~ x",                     "regressão, ANOVA, regressão logística",
    3.9,  "Multivariadas exploratórias: Y",          "ordenação e classificação",
    2.8,  "Multivariadas (testes): DY ~ DX",         "teste de Mantel",
    1.7,  "Multivariadas (testes): Y ~ x",           "MANOVA, PERMANOVA",
    .6,   "Ordenações restritas: Y ~ X",             "RDA, CCA, db-RDA")
  vd <- cores[["verde"]]; az <- cores[["accent"]]
  cel <- bind_rows(
    mat(6.6, 5.3, 1, cor = vd), mat(7.4, 5.3, 1, cor = az),
    mat(6.6, 4.2, 6, cor = vd),
    tri(6.6, 3.1, cor = vd), tri(8.2, 3.1, cor = az),
    mat(6.6, 2.0, 6, cor = vd), mat(8.2, 2.0, 1, cor = az),
    mat(6.6, .9, 6, cor = vd), mat(8.2, .9, 3, cor = az))
  ggplot() +
    geom_tile(data = cel, aes(x, y), width = .2, height = .14, fill = cel$cor, alpha = .75) +
    geom_text(data = linhas, aes(0, y, label = titulo), hjust = 0, size = 5.2, fontface = "bold",
              colour = ne) +
    geom_text(data = linhas, aes(0, y - .35, label = exemplo), hjust = 0, size = 4.4,
              colour = cores[["texto"]], fontface = "italic") +
    coord_cartesian(xlim = c(0, 9.2), ylim = c(-.1, 5.6)) +
    theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---- história ------------------------------------------------------------------

#' Linha do tempo dos métodos multivariados (datas das publicações originais,
#' inspirado na figura de McCune & Grace 2002)
fig_linha_tempo <- function() {
  d <- tribble(
    ~ano, ~metodo,                                    ~lado,
    1901, "PCA (Pearson)",                             1,
    1933, "PCA (Hotelling)",                          -1,
    1935, "CA (Hirschfeld)",                           1,
    1957, "Ordenação polar (Bray & Curtis)",          -1,
    1959, "Análise de associação (Williams & Lambert)", 1,
    1964, "nMDS (Kruskal)",                           -1,
    1966, "PCoA (Gower)",                              1,
    1979, "DCA (Hill)",                               -1,
    1986, "CCA (ter Braak)",                           1,
    2001, "PERMANOVA (Anderson)",                     -1)
  d <- d |> mutate(y = c(1, -1, 2.2, -2.2, 1, -1, 2.2, -2.2, 1, -1))
  ggplot(d) +
    geom_hline(yintercept = 0, colour = ne, linewidth = 1.2) +
    geom_segment(aes(ano, 0, xend = ano, yend = y), colour = cores[["neutro"]]) +
    geom_point(aes(ano, 0), colour = ne, size = 3) +
    geom_label(aes(ano, y, label = paste0(ano, "\n", sub(" \\(", "\n(", metodo))), size = 3.9,
               lineheight = .9, fill = cores[["papel"]], colour = cores[["texto"]],
               linewidth = .2) +
    scale_x_continuous(limits = c(1890, 2012)) +
    coord_cartesian(ylim = c(-2.8, 2.8)) +
    theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---- espaço euclidiano ---------------------------------------------------------------

#' Duas áreas como pontos no espaço das espécies, com a distância euclidiana
fig_espaco <- function() {
  m <- matrizes_exemplo()$sp
  d <- tibble(area = rownames(m), sp1 = m$Sp1, sp2 = m$Sp2)
  a <- d[c(1, 2), ]
  ggplot(d, aes(sp1, sp2)) +
    annotate("segment", x = a$sp1[1], y = a$sp2[1], xend = a$sp1[2], yend = a$sp2[2],
             colour = "#c0392b", linewidth = 1.1) +
    annotate("segment", x = a$sp1[1], xend = a$sp1[2], y = a$sp2[1], yend = a$sp2[1],
             linetype = "22", colour = cores[["neutro"]]) +
    annotate("segment", x = a$sp1[2], xend = a$sp1[2], y = a$sp2[1], yend = a$sp2[2],
             linetype = "22", colour = cores[["neutro"]]) +
    annotate("text", x = mean(a$sp1) - 2.6, y = mean(a$sp2), colour = "#c0392b", size = 5,
             label = paste0("D = ", format(round(sqrt(sum((m[1, 1:2] - m[2, 1:2])^2)), 2),
                                            decimal.mark = ","))) +
    geom_point(shape = 21, fill = cores[["accent"]], colour = "white", size = 4) +
    geom_text(data = d[1:4, ], aes(label = area), vjust = -1, size = 4.4) +
    annotate("text", x = 1, y = -.9, label = "Áreas 5 a 8", size = 4.2, hjust = 0) +
    coord_equal(xlim = c(-1, 22), ylim = c(-1.5, 23)) +
    labs(x = "Abundância da espécie 1", y = "Abundância da espécie 2") +
    tema_aula(base = 15)
}

# ---- correlação -------------------------------------------------------------------

#' Diagramas de dispersão com correlações diferentes (dados simulados,
#' inspirado na Fig. 5.2 de Quinn & Keough 2002)
fig_correlacoes <- function() {
  set.seed(10)
  n <- 40
  x <- runif(n, 0, 10)
  casos <- list(
    "Positiva" = 2 + .8 * x + rnorm(n, 0, 1.6),
    "Negativa" = 10 - .8 * x + rnorm(n, 0, 1.6),
    "Nenhuma" = rnorm(n, 6, 2.5),
    "Não linear" = 12 - .45 * (x - 5)^2 + rnorm(n, 0, 1))
  d <- bind_rows(lapply(names(casos), \(k) tibble(x, y = casos[[k]], caso = k))) |>
    group_by(caso) |>
    mutate(titulo = paste0(caso, "\nr = ", format(round(cor(x, y), 2), nsmall = 2,
                                                  decimal.mark = ","))) |>
    ungroup() |>
    mutate(titulo = factor(titulo, unique(titulo)))
  ggplot(d, aes(x, y)) +
    geom_point(shape = 21, fill = cores[["accent"]], colour = "white", size = 2.6) +
    facet_wrap(~titulo, nrow = 1) +
    labs(x = "X", y = "Y") +
    tema_aula(base = 14) + theme(axis.text = element_blank())
}

# ---- vetores e autovetores ------------------------------------------------------------

#' Operações com vetores: multiplicação por escalar e soma
fig_vetores <- function() {
  u <- c(3, 1); v <- c(1, 2)
  seta <- arrow(length = unit(.2, "cm"), type = "closed")
  t <- tema_aula(base = 14) + theme(panel.grid.minor = element_blank())
  p1 <- ggplot() +
    annotate("segment", x = 0, y = 0, xend = 2 * v[1], yend = 2 * v[2], colour = cores[["ferrugem"]],
             linewidth = 1.3, arrow = seta) +
    annotate("segment", x = 0, y = 0, xend = v[1], yend = v[2], colour = cores[["accent"]],
             linewidth = 1.6, arrow = seta) +
    annotate("text", x = c(v[1], 2 * v[1]) + .35, y = c(v[2], 2 * v[2]), label = c("v", "2v"),
             size = 6, fontface = "bold.italic", colour = c(cores[["accent"]], cores[["ferrugem"]])) +
    coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
    labs(title = "Multiplicação por um escalar", x = NULL, y = NULL) + t
  p2 <- ggplot() +
    annotate("segment", x = 0, y = 0, xend = u[1], yend = u[2], colour = cores[["accent"]],
             linewidth = 1.3, arrow = seta) +
    annotate("segment", x = u[1], y = u[2], xend = u[1] + v[1], yend = u[2] + v[2],
             colour = cores[["accent"]], linewidth = 1.3, arrow = seta, linetype = "22") +
    annotate("segment", x = 0, y = 0, xend = u[1] + v[1], yend = u[2] + v[2],
             colour = cores[["verde"]], linewidth = 1.5, arrow = seta) +
    annotate("text", x = c(u[1] / 2, u[1] + v[1] / 2 + .3, (u[1] + v[1]) / 2 - .4),
             y = c(u[2] / 2 - .35, u[2] + v[2] / 2, (u[2] + v[2]) / 2 + .35),
             label = c("u", "v", "u + v"), size = 6, fontface = "bold.italic",
             colour = c(cores[["accent"]], cores[["accent"]], cores[["verde"]])) +
    coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
    labs(title = "Soma de vetores", x = NULL, y = NULL) + t
  p1 + p2
}

#' O que é um autovetor: multiplicada pela matriz A, a maioria dos vetores
#' muda de direção; o autovetor só muda de comprimento
fig_autovetor <- function() {
  A <- matrix(c(1, 2, 4, 3), 2)
  v <- c(2, 2); w <- c(2, -1)
  Av <- A %*% v; Aw <- A %*% w
  seta <- arrow(length = unit(.2, "cm"), type = "closed")
  painel <- function(x, Ax, titulo, cor) {
    lim <- max(abs(c(x, Ax))) * 1.15
    ggplot() +
      geom_hline(yintercept = 0, colour = cores[["neutro_cl"]]) +
      geom_vline(xintercept = 0, colour = cores[["neutro_cl"]]) +
      annotate("segment", x = 0, y = 0, xend = Ax[1], yend = Ax[2], colour = cor,
               linewidth = 1.2, arrow = seta, alpha = .7) +
      annotate("segment", x = 0, y = 0, xend = x[1], yend = x[2], colour = cores[["texto"]],
               linewidth = 1.6, arrow = seta) +
      annotate("text", x = x[1], y = x[2], label = paste0("v = (", x[1], "; ", x[2], ")"),
               vjust = 1.8, hjust = -.05, size = 5) +
      annotate("text", x = Ax[1], y = Ax[2], label = paste0("A·v = (", Ax[1], "; ", Ax[2], ")"),
               vjust = -.8, hjust = ifelse(Ax[1] > 0, 1, 0), size = 5, colour = cor) +
      coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim), clip = "off") +
      labs(title = titulo, x = NULL, y = NULL) + tema_aula(base = 14)
  }
  painel(v, Av, "Autovetor: mesma direção, 5 vezes maior", cores[["verde"]]) +
    painel(w, Aw, "Outro vetor: muda de direção", "#c0392b")
}

# ---- morfometria -------------------------------------------------------------

#' Morfometria tradicional (distâncias lineares) ou geométrica (marcos
#' anatômicos) sobre a silhueta dorsal de um anuro (PhyloPic, CC0)
fig_morfometria <- function(tipo = c("tradicional", "geometrica")) {
  tipo <- match.arg(tipo)
  sil <- png::readPNG(here::here("assets/phylopic/hylidae-dorsal.png"))
  asp <- dim(sil)[1] / dim(sil)[2]
  g <- ggplot() +
    annotation_raster(sil, 0, 1, 0, asp) +
    coord_equal(xlim = c(-.05, 1.75), ylim = c(0, asp * 1.02), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  Y <- function(f) asp * (1 - f)   # linha da imagem (0 = topo) → y do gráfico
  vm <- "#c0392b"
  if (tipo == "tradicional") {
    med <- tibble(x = c(.5, .33, .73), y = c(Y(0), Y(.15), Y(0)),
                  xend = c(.5, .66, .73), yend = c(Y(.70), Y(.15), Y(.30)),
                  r = c("CRC", "LC", "CC"), lx = c(.53, .495, .76), ly = c(Y(.5), Y(.10), Y(.15)))
    g + geom_segment(data = med, aes(x, y, xend = xend, yend = yend), colour = vm, linewidth = 1.3,
                     arrow = arrow(length = unit(.2, "cm"), ends = "both", type = "closed")) +
      geom_label(data = med, aes(lx, ly, label = r), colour = vm, fontface = "bold", size = 5.2,
                 hjust = c(0, .5, 0), fill = "white", linewidth = 0) +
      annotate("text", x = 1.08, y = asp * .55, hjust = 0, size = 5.2, lineheight = .95,
               colour = cores[["accent_dark"]],
               label = "CRC: comprimento\nrostro-cloacal\n\nLC: largura\nda cabeça\n\nCC: comprimento\nda cabeça")
  } else {
    lm <- tibble(x = c(.5, .33, .66, .34, .67, .35, .66, .40, .61, .5),
                 y = Y(c(0, .15, .15, .30, .30, .45, .45, .65, .65, .70)))
    lm$k <- seq_len(nrow(lm))
    g + geom_point(data = lm, aes(x, y), shape = 21, size = 4.5, fill = vm, colour = "white",
                   stroke = 1) +
      geom_text(data = lm, aes(x, y, label = k), size = 3.6, nudge_x = ifelse(lm$x < .5, -.05, .05),
                colour = vm, fontface = "bold") +
      annotate("text", x = 1.08, y = asp * .55, hjust = 0, size = 5.2, lineheight = .95,
               colour = cores[["accent_dark"]],
               label = "10 marcos anatômicos:\ncada indivíduo vira\n20 coordenadas (x, y)")
  }
}

# ---- história: ordenação polar e análise de gradientes --------------------------

#' Ordenação polar de Bray & Curtis (1957) nos prados de dunas (vegan::dune):
#' os dois locais mais diferentes são os extremos do eixo, e cada local é
#' posicionado pelas suas distâncias até eles
fig_ordenacao_polar <- function() {
  e <- new.env(); utils::data("dune", package = "vegan", envir = e)
  D <- as.matrix(vegan::vegdist(e$dune, "bray"))
  ab <- which(D == max(D), arr.ind = TRUE)[1, ]
  a <- ab[1]; b <- ab[2]; L <- D[a, b]
  x <- (L^2 + D[a, ]^2 - D[b, ]^2) / (2 * L)
  d <- tibble(local = rownames(D), x = x, extremo = seq_along(x) %in% c(a, b)) |>
    arrange(x) |> mutate(ny = rep(c(.18, -.18, .34, -.34), length.out = n()))
  ggplot(d, aes(x, 0)) +
    annotate("segment", x = 0, xend = L, y = 0, yend = 0, colour = cores[["accent_dark"]],
             linewidth = 1) +
    geom_point(aes(fill = extremo), shape = 21, size = 5, colour = "white") +
    geom_segment(aes(xend = x, y = 0, yend = ny * .8), colour = cores[["neutro_cl"]]) +
    geom_text(aes(y = ny, label = local), size = 4.4) +
    annotate("text", x = c(0, L), y = .55, label = c("extremo A", "extremo B"), size = 5,
             colour = "#c0392b", fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = "#c0392b", `FALSE` = cores[["accent"]]), guide = "none") +
    coord_cartesian(ylim = c(-.8, .8)) +
    labs(x = "Posição no eixo polar (distância de Bray-Curtis)", y = NULL) +
    tema_aula(base = 15) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
}

#' Análise de gradientes (Whittaker): curvas de resposta das espécies ao
#' longo de um gradiente de altitude (dados simulados)
fig_gradiente_whittaker <- function() {
  alt <- seq(500, 2000, by = 10)
  sp <- tibble(sp = paste("sp", 1:7), otimo = c(550, 800, 1000, 1250, 1450, 1700, 1950),
               larg = c(150, 220, 180, 260, 200, 230, 160), alt_max = c(30, 45, 35, 50, 40, 38, 28))
  d <- tidyr::crossing(sp, alt = alt) |>
    mutate(ab = alt_max * exp(-(alt - otimo)^2 / (2 * larg^2)))
  ggplot(d, aes(alt, ab, colour = sp)) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = grDevices::hcl.colors(7, "Dark 3"), guide = "none") +
    labs(x = "Altitude (m)", y = "Abundância",
         subtitle = "Cada espécie tem um ótimo e uma amplitude ao longo do gradiente") +
    tema_aula(base = 15)
}

# ---- espaço euclidiano --------------------------------------------------------

#' Distância euclidiana em 2D e em 3D (projeção oblíqua simples)
fig_distancia_2d3d <- function() {
  vm <- "#c0392b"; ne <- cores[["accent_dark"]]
  p1 <- ggplot() +
    annotate("segment", x = 1, y = 1, xend = 4, yend = 3.5, colour = vm, linewidth = 1.2) +
    annotate("segment", x = c(1, 4), y = c(1, 1), xend = c(4, 4), yend = c(1, 3.5),
             colour = cores[["neutro"]], linetype = "22") +
    annotate("point", x = c(1, 4), y = c(1, 3.5), size = 4, colour = ne) +
    annotate("text", x = c(1, 4), y = c(1, 3.5), label = c("A", "B"), vjust = -1, size = 5.5,
             fontface = "bold") +
    annotate("text", x = 2.5, y = .7, label = "Δy₁", size = 5, colour = cores[["neutro"]]) +
    annotate("text", x = 4.3, y = 2.25, label = "Δy₂", size = 5, colour = cores[["neutro"]]) +
    annotate("text", x = 2.2, y = 2.6, label = "D", size = 6, colour = vm, fontface = "bold") +
    coord_equal(xlim = c(0, 5), ylim = c(0, 4.2)) +
    labs(x = "Espécie 1", y = "Espécie 2", title = "2D: Pitágoras") + tema_aula(base = 15)
  pr <- function(x, y, z) c(x + .5 * z, y + .4 * z)   # projeção oblíqua
  eixo <- function(x, y, z) { o <- pr(0, 0, 0); f <- pr(x, y, z); c(o, f) }
  A <- pr(1, 1, 2.5); B <- pr(4, 3, .8); Cc <- pr(4, 1, 2.5); Dd <- pr(4, 3, 2.5)
  ex <- rbind(eixo(5, 0, 0), eixo(0, 4, 0), eixo(0, 0, 4))
  p2 <- ggplot() +
    annotate("segment", x = ex[, 1], y = ex[, 2], xend = ex[, 3], yend = ex[, 4], colour = ne,
             arrow = arrow(length = unit(.15, "cm"))) +
    annotate("text", x = ex[, 3], y = ex[, 4], label = c("sp 1", "sp 2", "sp 3"), hjust = c(-.2, -.2, -.2), vjust = c(.5, -.4, 1.4),
             size = 4.4, colour = ne) +
    annotate("segment", x = c(A[1], Cc[1], Dd[1]), y = c(A[2], Cc[2], Dd[2]),
             xend = c(Cc[1], Dd[1], B[1]), yend = c(Cc[2], Dd[2], B[2]),
             colour = cores[["neutro"]], linetype = "22") +
    annotate("segment", x = A[1], y = A[2], xend = B[1], yend = B[2], colour = vm, linewidth = 1.2) +
    annotate("point", x = c(A[1], B[1]), y = c(A[2], B[2]), size = 4, colour = ne) +
    annotate("text", x = c(A[1], B[1]), y = c(A[2], B[2]), label = c("A", "B"), vjust = -1,
             size = 5.5, fontface = "bold") +
    coord_equal(clip = "off") + labs(title = "3D: Pitágoras duas vezes") + theme_void(base_size = 15) +
    theme(plot.title = element_text(face = "bold", colour = ne),
          plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  p1 + p2
}

#' Espaço de formas curvo e o espaço tangente (esquema)
fig_espaco_tangente <- function() {
  t <- seq(0, pi, length.out = 200)
  arco <- tibble(x = cos(t), y = sin(t))
  pts <- tibble(t = c(1.15, 1.35, 1.6, 1.85, 2.0)) |> mutate(x = cos(t), y = sin(t),
                                                               px = x / y, py = 1)
  vm <- "#c0392b"; ne <- cores[["accent_dark"]]
  ggplot() +
    geom_path(data = arco, aes(x, y), colour = ne, linewidth = 1.3) +
    annotate("segment", x = -1.3, xend = 1.3, y = 1, yend = 1, colour = vm, linewidth = 1.3) +
    geom_segment(data = pts, aes(0, 0, xend = px, yend = py), colour = cores[["neutro"]],
                 linetype = "22") +
    geom_point(data = pts, aes(x, y), size = 3.5, colour = ne) +
    geom_point(data = pts, aes(px, py), size = 3.5, colour = vm) +
    annotate("point", x = 0, y = 1, shape = 8, size = 5, colour = vm) +
    annotate("text", x = -1.25, y = 1.12, label = "espaço tangente (plano)", hjust = 0, size = 5,
             colour = vm, fontface = "bold") +
    annotate("text", x = .75, y = .4, label = "espaço de formas\n(curvo)", size = 5, colour = ne,
             fontface = "bold", lineheight = .9) +
    annotate("text", x = .12, y = 1.12, label = "forma média", size = 4.4, colour = vm, hjust = 0) +
    coord_equal(xlim = c(-1.35, 1.35), ylim = c(0, 1.25)) + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' Comprimento de um vetor pelo teorema de Pitágoras
fig_comprimento_vetor <- function() {
  ne <- cores[["accent_dark"]]
  ggplot() +
    annotate("polygon", x = c(0, 4, 4), y = c(0, 0, 3), fill = cores[["neutro_cl"]], alpha = .6) +
    annotate("segment", x = 0, y = 0, xend = 4, yend = 3, colour = cores[["accent"]], linewidth = 1.5,
             arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("text", x = c(2, 4.3, 1.7), y = c(-.3, 1.5, 1.85), label = c("4", "3", "‖b‖ = √(4² + 3²) = 5"),
             size = c(5.5, 5.5, 5.5), colour = c(ne, ne, cores[["accent"]]), fontface = "bold", hjust = c(.5, .5, 1)) +
    coord_equal(xlim = c(-1.6, 4.8), ylim = c(-.6, 3.4)) + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

#' Dados brutos simulados de morfometria linear (mm) de 8 indivíduos de duas
#' espécies: todas as medidas crescem com o tamanho do corpo (alometria), e a
#' espécie B tem pernas proporcionalmente mais longas
morfo_brutos <- function() {
  set.seed(8)
  n <- 4
  crc <- c(rnorm(n, 52, 5), rnorm(n, 48, 4))
  sp <- rep(c("A", "B"), each = n)
  perna <- ifelse(sp == "A", .45, .58)
  d <- tibble(Indivíduo = paste0(sp, c(1:n, 1:n)), CRC = crc,
              LC = .34 * crc + rnorm(2 * n, 0, .6),
              F = perna * crc + rnorm(2 * n, 0, .8),
              T = (perna + .03) * crc + rnorm(2 * n, 0, .8),
              Pé = .42 * crc + rnorm(2 * n, 0, .7),
              Mão = .24 * crc + rnorm(2 * n, 0, .5))
  mutate(d, across(where(is.numeric), \(z) format(round(z, 1), nsmall = 1, decimal.mark = ",")))
}
