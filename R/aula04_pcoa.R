library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

az <- cores_familia[["Ordenação irrestrita"]]

#' Distâncias de Bray-Curtis entre os locais do Doubs (peixes, sem o
#' local 8)
dist_doubs <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  vegan::vegdist(e$doubs$fish[-8, ], "bray")
}

#' Os passos da PCoA: da matriz de dados às coordenadas principais
fig_pcoa_passos <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,    ~y,  ~w,  ~h,  ~rotulo,                                        ~cor,
    0,     0,   2.3, 1.3, "X\nobjetos × descritores",                    ne,
    3.4,   0,   2.3, 1.3, "D\ndistâncias entre\nos objetos",             ne,
    6.8,   0,   2.3, 1.3, "A = −½ D²",                                   ne,
    6.8,  -2.1, 2.3, 1.3, "Δ\nA com centralização\ndupla",              ne,
    3.4,  -2.1, 2.3, 1.3, "Autoanálise de Δ",                            az,
    0,    -2.1, 2.3, 1.3, "Coordenadas\nprincipais:\nautovetores × √λ",  az
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend,
    1.15,  0,     2.2,   0,
    4.55,  0,     5.6,   0,
    6.8,  -.65,   6.8,  -1.4,
    5.65, -2.1,   4.6,  -2.1,
    2.25, -2.1,   1.2,  -2.1
  )
  rot <- tribble(
    ~x,   ~y,    ~rotulo,
    1.7,   .3,   "qualquer\ncoeficiente",
    5.1,   .3,   "eleva ao\nquadrado",
    7.25, -1.0,  "centraliza"
  )
  diagrama(cx, setas, base = 4.6) +
    geom_text(data = rot, aes(x, y, label = rotulo), size = 3.8, lineheight = .9,
              colour = cores[["neutro"]], vjust = 0, hjust = c(.5, .5, 0))
}

#' Diagrama de Shepard: distâncias originais × distâncias no espaço
#' reduzido (2 eixos), na PCoA e no nMDS dos peixes do Doubs (Bray-Curtis)
fig_shepard <- function() {
  d <- dist_doubs()
  pco <- stats::cmdscale(d, k = 2, eig = TRUE)
  dp <- tibble(orig = as.vector(d), red = as.vector(dist(pco$points)))
  set.seed(2026)
  nm <- vegan::metaMDS(d, k = 2, trace = FALSE, autotransform = FALSE)
  # regressão monotônica (isotônica) das distâncias na ordenação sobre as
  # originais: a linha em degraus do diagrama de Shepard do nMDS
  dn <- tibble(orig = as.vector(d), red = as.vector(dist(nm$points))) |>
    arrange(orig)
  dn$ajuste <- stats::isoreg(dn$orig, dn$red)$yf
  t <- tema_aula(base = 14)
  p1 <- ggplot(dp, aes(orig, red)) +
    geom_abline(colour = cores[["ferrugem"]], linewidth = .8) +
    geom_point(colour = az, alpha = .5, size = 1.4) +
    labs(title = "PCoA: relação linear",
         x = "Distância original (Bray-Curtis)", y = "Distância em 2 eixos") + t
  p2 <- ggplot(dn, aes(orig, red)) +
    geom_point(colour = az, alpha = .5, size = 1.4) +
    geom_step(aes(y = ajuste), colour = cores[["ferrugem"]], linewidth = .9) +
    labs(title = "nMDS: relação monotônica",
         x = "Distância original (Bray-Curtis)", y = "Distância em 2 eixos") + t
  p1 + p2
}

#' Quatro pontos que não cabem no espaço euclidiano, e as correções
#' (redesenho da Fig. 9.17 de Legendre & Legendre 2012)
fig_nao_euclidiano <- function() {
  tri <- function(lado) {
    tibble(x = c(0, lado, lado / 2), y = c(0, 0, lado * sqrt(3) / 2),
           v = c("x1", "x2", "x3"))
  }
  centro <- function(tr) c(mean(tr$x), mean(tr$y))
  t <- theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          plot.title = element_text(face = "bold", colour = cores[["texto"]], size = 13))
  rot_v <- function(tr) geom_text(data = tr, aes(x, y, label = v),
                                  nudge_y = c(-.07, -.07, .07), fontface = "bold",
                                  colour = cores[["texto"]])
  # (a) lado 0,8 e D = 0,377 até x4: as três linhas não se encontram
  ta <- tri(.8); ca <- centro(ta)
  seg_a <- ta |> mutate(dx = ca[1] - x, dy = ca[2] - y, L = sqrt(dx^2 + dy^2),
                        xe = x + dx / L * .377, ye = y + dy / L * .377)
  pa <- ggplot() +
    geom_polygon(data = ta, aes(x, y), fill = NA, colour = cores[["texto"]]) +
    geom_segment(data = seg_a, aes(x, y, xend = xe, yend = ye), colour = cores[["ferrugem"]],
                 linewidth = .9) +
    geom_point(data = seg_a, aes(xe, ye), colour = cores[["ferrugem"]], size = 1.8) +
    annotate("text", x = ca[1] + .2, y = ca[2] + .17, label = "x4 ?",
             colour = cores[["ferrugem"]], fontface = "bold") +
    annotate("text", x = .4, y = -.08, label = "D = 0,8", size = 3.8) +
    annotate("text", x = .08, y = .17, label = "D = 0,377", size = 3.6, angle = 30,
             colour = cores[["ferrugem"]]) +
    rot_v(ta) + coord_equal(xlim = c(-.1, 1.3), ylim = c(-.15, 1.15)) +
    labs(title = "(a) Não euclidiano") + t
  # (b) + 0,2: lado 1,0 e D = 0,577, as linhas se encontram no centro
  tb <- tri(1); cb <- centro(tb)
  pb <- ggplot() +
    geom_polygon(data = tb, aes(x, y), fill = NA, colour = cores[["texto"]]) +
    geom_segment(data = tb, aes(x, y, xend = cb[1], yend = cb[2]), colour = az,
                 linewidth = .9) +
    annotate("point", x = cb[1], y = cb[2], colour = az, size = 2.4) +
    annotate("text", x = cb[1] + .06, y = cb[2] + .05, label = "x4", fontface = "bold",
             colour = az) +
    annotate("text", x = .5, y = -.08, label = "D = 1,0", size = 3.8) +
    annotate("text", x = .17, y = .2, label = "D = 0,577", size = 3.8, angle = 30,
             colour = az) +
    rot_v(tb) + coord_equal(xlim = c(-.1, 1.3), ylim = c(-.15, 1.15)) +
    labs(title = "(b) + 0,2: cabe em 2D") + t
  # (c) + 0,4: lado 1,2 e D = 0,777, x4 sai do plano (3 dimensões)
  lado <- 1.2
  h <- sqrt(.777^2 - (lado / sqrt(3))^2)
  base3 <- tibble(X = c(0, lado, lado / 2), Y = c(0, 0, lado * sqrt(3) / 2), Z = 0,
                  v = c("x1", "x2", "x3"))
  c3 <- c(mean(base3$X), mean(base3$Y))
  # vista de frente, levemente de cima: a profundidade (Y) achatada
  proj <- function(X, Y, Z) tibble(x = X, y = .3 * Y + 1.4 * Z)
  pbase <- bind_cols(proj(base3$X, base3$Y, base3$Z), v = base3$v)
  p4 <- proj(c3[1], c3[2], h)
  pc <- ggplot() +
    geom_polygon(data = pbase, aes(x, y), fill = cores[["neutro_cl"]], colour = cores[["texto"]]) +
    geom_segment(data = pbase, aes(x, y, xend = p4$x, yend = p4$y), colour = az,
                 linewidth = .9) +
    annotate("point", x = p4$x, y = p4$y, colour = az, size = 2.4) +
    annotate("text", x = p4$x, y = p4$y + .07, label = "x4", fontface = "bold",
             colour = az) +
    geom_text(data = pbase, aes(x, y, label = v), nudge_y = c(-.07, -.07, .07),
              fontface = "bold", colour = cores[["texto"]]) +
    annotate("text", x = .6, y = -.1, label = "D = 1,2", size = 3.8) +
    coord_equal(xlim = c(-.1, 1.5), ylim = c(-.15, 1.15)) +
    labs(title = "(c) + 0,4: precisa de 3D") + t
  pa + pb + pc
}

#' Autovalores da PCoA dos peixes do Doubs (Bray-Curtis): sem correção há
#' autovalores negativos; as correções de Lingoes e Cailliez os eliminam
fig_autovalores_negativos <- function() {
  d <- dist_doubs()
  um <- function(corr, nome) {
    p <- ape::pcoa(d, correction = corr)
    v <- if (corr == "none") p$values$Eigenvalues else p$values$Corr_eig
    tibble(metodo = nome, k = seq_along(v), lambda = v)
  }
  dd <- bind_rows(um("none", "Sem correção"), um("lingoes", "Lingoes"),
                  um("cailliez", "Cailliez")) |>
    mutate(metodo = factor(metodo, c("Sem correção", "Lingoes", "Cailliez")),
           neg = lambda < -1e-10)
  ggplot(dd, aes(k, lambda, fill = neg)) +
    geom_col(width = .8) +
    geom_hline(yintercept = 0, colour = cores[["texto"]], linewidth = .4) +
    facet_wrap(~metodo, scales = "free_y") +
    scale_fill_manual(values = c(`FALSE` = az, `TRUE` = cores[["ferrugem"]]),
                      labels = c("positivo", "negativo"), name = NULL) +
    labs(x = "Eixo", y = "Autovalor") +
    tema_aula(base = 14)
}
