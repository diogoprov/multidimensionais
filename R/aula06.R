library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))
source(here::here("R/ordena.R"))

oc <- cores_familia[["Testes"]]

# ---- dados -------------------------------------------------------------------

#' Ácaros de vegan::mite: distâncias de Bray-Curtis (composição, Hellinger),
#' ambientais (densidade do substrato e conteúdo de água, padronizados) e
#' geográficas
mite_dist <- function() {
  e <- new.env()
  utils::data("mite", "mite.env", "mite.xy", package = "vegan", envir = e)
  list(Dy  = vegan::vegdist(e$mite, "bray"),
       Dx  = stats::dist(scale(e$mite.env[, c("SubsDens", "WatrCont")])),
       Dg  = stats::dist(e$mite.xy))
}

#' Anuros em campo e em coleções (da Silva et al. 2017), dados da aula prática
anuros <- function() {
  a <- utils::read.csv(here::here("aula-06-matrizes-mantel-manova/anuros_permanova.csv"),
                       row.names = 1, fileEncoding = "latin1")
  list(sp = a[, -ncol(a)], grupo = factor(a[, ncol(a)], c("CAMPO", "COLECAO"),
                                          c("Campo", "Coleção")))
}

# ---- Mantel ------------------------------------------------------------------

#' Esquema do teste de Mantel: duas matrizes de dados viram duas matrizes de
#' distâncias, que são desdobradas em vetores e correlacionadas
fig_mantel_esquema <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                  ~cor,  ~face, ~tam,
    0,    1.6,   1.4, 1.1, "Y\ncomposição",                         ne,    "bold", 1.1,
    4,    1.6,   1.4, 1.1, "X\nambiente",                           ne,    "bold", 1.1,
    0,    -.3,   1.4, 1.1, "DY",                                    oc,    "bold", 1.4,
    4,    -.3,   1.4, 1.1, "DX",                                    oc,    "bold", 1.4,
    2,    -2,    3.4, .9,  "correlação de Pearson entre os\nvetores das duas matrizes: rM", oc, "plain", 1
  )
  setas <- tribble(~x, ~y, ~xend, ~yend, 0, 1.05, 0, .3, 4, 1.05, 4, .3, 0, -.85, 1.2, -1.5,
                   4, -.85, 2.8, -1.5)
  diagrama(cx, setas, base = 4.6) +
    annotate("text", x = c(.15, 4.15), y = .67, hjust = 0, size = 3.8, colour = ne,
             label = c('vegdist(Y, "bray")', 'dist(scale(X))'), family = "mono") +
    annotate("text", x = 2, y = 1.6, label = "~", size = 14, colour = "#c0392b", fontface = "bold")
}

#' Distâncias de composição contra distâncias ambientais (ácaros), com o rM
fig_mantel_dispersao <- function(d = mite_dist(), r = NULL, x = "Dx",
                                 xlab = "Distância ambiental (DX)") {
  dd <- tibble(x = as.vector(d[[x]]), y = as.vector(d$Dy))
  r <- r %||% cor(dd$x, dd$y)
  ggplot(dd, aes(x, y)) +
    geom_point(colour = oc, alpha = .25, size = 1) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["texto"]], linewidth = .9) +
    labs(x = xlab, y = "Distância de composição (DY)",
         subtitle = paste0("rM = ", format(round(r, 3), decimal.mark = ","))) +
    tema_aula(base = 14)
}

#' Distribuição nula do rM por permutação, com o valor observado
fig_mantel_nula <- function(m) {
  d <- tibble(r = m$perm)
  ggplot(d, aes(r)) +
    geom_histogram(bins = 40, fill = cores[["neutro_cl"]], colour = cores[["neutro"]]) +
    geom_vline(xintercept = m$statistic, colour = "#c0392b", linewidth = 1.2) +
    annotate("text", x = m$statistic, y = Inf, vjust = 1.5, hjust = 1.05, colour = "#c0392b",
             fontface = "bold", size = 4.6,
             label = paste0("observado: ", format(round(m$statistic, 3), decimal.mark = ","))) +
    labs(x = "rM com Y permutada", y = "Frequência",
         subtitle = paste(length(m$perm), "permutações das linhas e colunas de DY")) +
    tema_aula(base = 14)
}

# ---- regressão múltipla de matrizes ------------------------------------------

#' Regressão múltipla de matrizes de distâncias (MRM), com teste do R² por
#' permutação dos objetos da matriz resposta
mrm <- function(Dy, ..., nperm = 999, semente = 1) {
  X <- sapply(list(...), as.vector)
  y <- as.vector(Dy)
  fit <- lm(y ~ X)
  r2 <- summary(fit)$r.squared
  n <- attr(Dy, "Size"); M <- as.matrix(Dy)
  set.seed(semente)
  r2p <- replicate(nperm, {
    o <- sample(n); yp <- as.dist(M[o, o]); summary(lm(as.vector(yp) ~ X))$r.squared })
  list(coef = coef(fit)[-1], r2 = r2, p = (sum(r2p >= r2) + 1) / (nperm + 1))
}

#' Composição × ambiente quando a dissimilaridade satura: a reta da MRM e uma
#' curva não linear, como no GDM (coenoclina simulada)
#' Por que as dissimilaridades saturam: a amplitude de cada espécie no
#' gradiente (barras), a matriz de presença e ausência que sai dela e a
#' dissimilaridade de Jaccard do local A para os outros, que para em 1
saturacao_ex <- function() {
  sp <- tibble(sp = paste0("sp", 1:12), otimo = seq(5, 95, length.out = 12),
               larg = c(26, 18, 30, 22, 34, 20, 28, 24, 32, 18, 26, 22)) |>
    mutate(ini = pmax(otimo - larg / 2, 0), fim = pmin(otimo + larg / 2, 100))
  locais <- c(A = 8, B = 22, C = 38, D = 55, E = 75, F = 95)
  Y <- sapply(seq_len(nrow(sp)), \(k) as.integer(locais >= sp$ini[k] & locais <= sp$fim[k]))
  dimnames(Y) <- list(names(locais), sp$sp)
  list(sp = sp, locais = locais, Y = Y,
       jac = as.matrix(vegan::vegdist(Y, "jaccard", binary = TRUE))["A", ])
}

fig_saturacao <- function(ex = saturacao_ex()) {
  sp <- ex$sp; loc <- ex$locais; Y <- ex$Y
  sp$y <- rev(seq_len(nrow(sp)))
  p1 <- ggplot(sp) +
    geom_vline(xintercept = loc, colour = "#c0392b", linetype = "22", linewidth = .6) +
    geom_segment(aes(x = ini, xend = fim, y = y, yend = y), colour = oc, linewidth = 4.2,
                 lineend = "round") +
    annotate("label", x = loc, y = nrow(sp) + 1.1, label = names(loc), colour = "#c0392b",
             fontface = "bold", size = 5.4, linewidth = 0, fill = cores[["papel"]]) +
    scale_y_continuous(breaks = sp$y, labels = sp$sp) +
    coord_cartesian(xlim = c(0, 100), ylim = c(.5, nrow(sp) + 1.5)) +
    labs(x = "Gradiente ambiental", y = NULL, title = "Amplitude de cada espécie") +
    tema_aula(base = 14) + theme(panel.grid.major.y = element_blank())
  dm <- as_tibble(Y, rownames = "local") |>
    tidyr::pivot_longer(-local, names_to = "sp", values_to = "pres") |>
    mutate(sp = factor(sp, colnames(Y)), local = factor(local, rev(rownames(Y))))
  p2 <- ggplot(dm, aes(sp, local)) +
    geom_tile(aes(fill = factor(pres)), colour = "white", linewidth = .8) +
    geom_text(aes(label = pres), size = 4.2,
              colour = ifelse(dm$pres == 1, "white", cores[["neutro"]])) +
    scale_fill_manual(values = c(`0` = cores[["neutro_cl"]], `1` = oc), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Matriz de composição") +
    tema_aula(base = 14) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = .5),
          axis.text.y = element_text(face = "bold", colour = "#c0392b", size = 15))
  dj <- tibble(local = names(loc), dist = loc - loc["A"], jac = ex$jac[names(loc)])
  p3 <- ggplot(dj, aes(dist, jac)) +
    geom_hline(yintercept = 1, colour = cores[["neutro"]], linetype = "22") +
    geom_line(colour = oc, linewidth = 1.1) +
    geom_point(colour = "#c0392b", size = 3.4) +
    geom_text(aes(label = local), colour = "#c0392b", fontface = "bold", size = 5,
              vjust = -1) +
    coord_cartesian(ylim = c(0, 1.15)) +
    labs(x = "Distância ambiental até A", y = "Jaccard com A",
         title = "Dissimilaridade a partir de A") +
    tema_aula(base = 14)
  (p1 | p2 | p3) + plot_layout(widths = c(1.15, 1, 1))
}

fig_gdm <- function() {
  grad <- seq(0, 100, length.out = 40)
  otimo <- seq(-10, 110, length.out = 30)
  m <- round(outer(grad, otimo, \(g, o) 30 * exp(-(g - o)^2 / (2 * 9^2))))
  d <- vegan::vegdist(m, "bray")
  ge <- stats::dist(grad)
  dd <- tibble(x = as.vector(ge), y = as.vector(d))
  fit <- stats::nls(y ~ 1 - exp(-(b0 + b1 * x)), data = dd, start = list(b0 = .05, b1 = .03))
  cv <- tibble(x = seq(0, 100, length.out = 200)) |> mutate(y = predict(fit, newdata = pick(x)))
  ggplot(dd, aes(x, y)) +
    geom_point(colour = oc, alpha = .3, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["neutro"]], linetype = "22",
                linewidth = 1) +
    geom_line(data = cv, colour = "#c0392b", linewidth = 1.3) +
    annotate("text", x = 98, y = .55, hjust = 1, size = 4.6, colour = cores[["neutro"]],
             label = "regressão linear (MRM)") +
    annotate("text", x = 98, y = 1.08, hjust = 1, size = 4.6, colour = "#c0392b",
             label = "curva que satura (como no GDM)") +
    coord_cartesian(ylim = c(0, 1.12)) +
    labs(x = "Distância ambiental entre os locais", y = "Dissimilaridade (Bray-Curtis)") +
    tema_aula(base = 15)
}

# ---- Procrustes ------------------------------------------------------------------

#' Os três passos da superposição de Procrustes: translação, escalonamento e
#' rotação de um triângulo sobre outro
fig_procrustes_passos <- function(passo = NULL) {
  A <- rbind(c(0, 0), c(2, 3), c(3, .5))
  ang <- 50 * pi / 180
  R <- matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), 2)
  B <- (A %*% R) * 1.8 + matrix(c(3, -2), 3, 2, byrow = TRUE) +
    matrix(c(.15, -.1, -.2, .2, .1, .1), 3, byrow = TRUE)
  cen <- function(M) sweep(M, 2, colMeans(M))
  tamanho <- function(M) sqrt(sum(cen(M)^2))
  A1 <- cen(A); B1 <- cen(B)
  A2 <- A1 / tamanho(A1); B2 <- B1 / tamanho(B1)
  s <- svd(t(A2) %*% B2); H <- s$v %*% t(s$u)
  B3 <- B2 %*% H
  tri <- function(M, nome, ref) tibble(x = M[, 1], y = M[, 2], v = c("A", "B", "C"), conj = nome) |>
    mutate(v = if (nome == "Y2") tolower(v) else v)
  painel <- function(M1, M2, titulo) {
    d <- bind_rows(tri(M1, "Y1"), tri(M2, "Y2"))
    seg <- tibble(x = M1[, 1], y = M1[, 2], xend = M2[, 1], yend = M2[, 2])
    ggplot(d) +
      geom_segment(data = seg, aes(x, y, xend = xend, yend = yend), colour = cores[["neutro"]],
                   linetype = "22") +
      geom_polygon(aes(x, y, group = conj, colour = conj), fill = NA, linewidth = 1.1,
                   linetype = rep(c("solid", "42"), each = 3)) +
      geom_text(aes(x, y, label = v, colour = conj), fontface = "bold", size = 5,
                nudge_y = .08 * diff(range(d$y))) +
      geom_hline(yintercept = 0, colour = cores[["neutro_cl"]]) +
      geom_vline(xintercept = 0, colour = cores[["neutro_cl"]]) +
      scale_colour_manual(values = c(Y1 = cores[["texto"]], Y2 = oc), guide = "none") +
      coord_equal() + labs(title = titulo, x = NULL, y = NULL) +
      theme_void() +
      theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
            plot.title = element_text(face = "bold", hjust = .5, colour = cores[["texto"]]))
  }
  est <- list(list(A, B, "Dados originais"), list(A1, B1, "1. Translação"),
              list(A2, B2, "2. Escalonamento"), list(A2, B3, "3. Rotação"))
  if (is.null(passo))
    return(Reduce(`+`, lapply(est, \(e) painel(e[[1]], e[[2]], e[[3]]))) + plot_layout(nrow = 1))
  # um passo: antes → depois
  seta <- ggplot() +
    annotate("segment", x = 0, xend = 1, y = 0, yend = 0, linewidth = 2.5,
             colour = cores[["accent_dark"]], arrow = arrow(length = unit(.4, "cm"), type = "closed")) +
    annotate("text", x = .5, y = .25, label = sub("^\\d\\. ", "", est[[passo + 1]][[3]]),
             size = 6, fontface = "bold", colour = cores[["accent_dark"]]) +
    coord_cartesian(xlim = c(-.1, 1.1), ylim = c(-1, 1)) + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  antes <- est[[passo]]; depois <- est[[passo + 1]]
  painel(antes[[1]], antes[[2]], "Antes") + seta + painel(depois[[1]], depois[[2]], "Depois") +
    plot_layout(widths = c(1, .55, 1))
}

#' PROTEST: concordância entre a PCA ambiental e a PCA dos peixes do Doubs
doubs_protest <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  env <- e$doubs$env[-8, ]; fish <- e$doubs$fish[-8, ]
  pe <- vegan::rda(env, scale = TRUE)
  pf <- vegan::rda(vegan::decostand(fish, "hellinger"))
  set.seed(1)
  vegan::protest(vegan::scores(pe, display = "sites", choices = 1:2),
                 vegan::scores(pf, display = "sites", choices = 1:2), permutations = 999)
}

fig_protest <- function(p = doubs_protest()) {
  X <- p$X; Y <- p$Yrot
  d <- tibble(local = rownames(X), x = X[, 1], y = X[, 2], xe = Y[, 1], ye = Y[, 2])
  ggplot(d) +
    geom_segment(aes(x, y, xend = xe, yend = ye), colour = oc, linewidth = .8,
                 arrow = arrow(length = unit(.15, "cm"))) +
    geom_point(aes(x, y), colour = cores[["texto"]], size = 2) +
    geom_text(aes(x, y, label = local), size = 3.4, vjust = -.8, colour = cores[["texto"]]) +
    coord_equal() +
    labs(x = "Eixo 1", y = "Eixo 2",
         subtitle = "Ponto: PCA ambiental · seta: PCA dos peixes") +
    tema_aula(base = 13)
}

#' Procrustes como ferramenta geral: dos dados às coordenadas, à superposição e
#' aos resíduos (adaptado de Peres-Neto & Jackson 2001)
fig_procrustes_fluxo <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,    ~y,   ~w,   ~h,   ~rotulo,                                        ~cor,
    0,     1.9,  2.5,  1,    "Dados brutos\n(X e Y, separados)",             ne,
    4.6,   1.9,  2.5,  1,    "Matriz de\ndistâncias",                        ne,
    2.3,   -.4,  2.9,  1,    "Coordenadas\nde X e de Y",                     ne,
    6.4,   -.4,  2.5,  1,    "Superposição\nde Procrustes",                  oc,
    10.1,  -.4,  2.6,  1,    "Resíduos\n(objetos × dimensões)",              oc,
    14.2,  1.2,  2.7,  .8,   "Agrupamento",                                  ne,
    14.2,  -.4,  2.7,  .8,   "Ordenação",                                    ne,
    14.2,  -2,   2.7,  1,    "PROTEST, ANOVA,\nMANOVA, regressão",           ne
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend, ~tipo,
    1.25,  1.9,   3.35,  1.9,   "solid",
    .4,    1.4,   1.6,   .1,    "solid",
    4.2,   1.4,   3.0,   .1,    "solid",
    -1.25, 1.6,   .85,   -.4,   "22",
    3.75,  -.4,   5.15,  -.4,   "solid",
    7.65,  -.4,   8.8,   -.4,   "solid",
    11.4,  -.2,   12.85, 1.2,   "solid",
    11.4,  -.4,   12.85, -.4,   "solid",
    11.4,  -.6,   12.85, -2,    "solid"
  )
  setas$curv <- c(0, 0, 0, .35, 0, 0, 0, 0, 0)
  diagrama(cx, setas, base = 4.8) +
    annotate("text", x = 2.3, y = 2.25, size = 4.3, colour = ne, label = "distância\n(euclidiana, Jaccard...)",
             lineheight = .85, vjust = 0) +
    annotate("text", x = -.05, y = .75, size = 4.3, colour = ne, label = "ordenação\n(PCA, CA)",
             lineheight = .85, hjust = 1) +
    annotate("text", x = 4.75, y = .75, size = 4.3, colour = ne, label = "ordenação\n(PCoA, nMDS)",
             lineheight = .85, hjust = 0) +
    annotate("text", x = -1.5, y = -.6, size = 4.1, colour = cores[["neutro"]],
             label = "ou os próprios\ndados", lineheight = .85, fontface = "italic") +
    annotate("text", x = 12.1, y = -2.6, size = 4.1, colour = cores[["neutro"]],
             label = "testes de hipótese", fontface = "italic") +
    geom_blank(data = tibble(x = c(-2.6, 15.6), y = c(-2.8, 2.9)), aes(x, y))
}

#' PROTEST: distribuição dos m² das permutações e o m² observado
fig_protest_perm <- function(p = doubs_protest()) {
  m2 <- 1 - p$t^2
  obs <- p$ss
  d <- tibble(m2 = m2)
  topo <- max(hist(m2, breaks = 30, plot = FALSE)$counts)
  ggplot(d, aes(m2)) +
    geom_histogram(bins = 30, fill = cores[["neutro_cl"]], colour = cores[["neutro"]]) +
    annotate("segment", x = obs, xend = obs, y = topo * .55, yend = topo * .03,
             colour = "#c0392b", linewidth = 1.3, arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("text", x = obs, y = topo * .62, colour = "#c0392b", fontface = "bold", size = 5.2, hjust = 0,
             label = paste0("m² observado = ", format(round(obs, 3), decimal.mark = ","))) +
    labs(x = "m² das permutações", y = "Frequência",
         subtitle = paste(length(m2), "permutações das linhas de uma das matrizes")) +
    tema_aula(base = 16)
}

# ---- PERMANOVA ----------------------------------------------------------------------

#' A matriz de distâncias ordenada pelos grupos: as somas de quadrados vêm das
#' distâncias dentro dos grupos (blocos destacados) e de todas as distâncias
fig_permanova_matriz <- function(a = anuros()) {
  d <- as.matrix(vegan::vegdist(vegan::decostand(a$sp, "hellinger"), "euclidean"))
  o <- order(a$grupo); d <- d[o, o]; g <- a$grupo[o]
  n <- nrow(d)
  dd <- expand_grid(i = seq_len(n), j = seq_len(n)) |>
    filter(i > j) |>
    mutate(dist = d[cbind(i, j)], dentro = g[i] == g[j],
           grupo = ifelse(dentro, as.character(g[i]), "entre grupos"))
  rot <- tibble(i = seq_len(n), nome = rownames(d), g = g)
  ggplot(dd, aes(j, i)) +
    geom_tile(aes(fill = grupo, alpha = dist), colour = "white") +
    scale_fill_manual(values = c(Campo = cores[["verde"]], `Coleção` = cores[["accent"]],
                                 `entre grupos` = cores[["neutro"]]), name = NULL) +
    scale_alpha(range = c(.35, 1), guide = "none") +
    scale_y_reverse(breaks = rot$i[-1], labels = rot$nome[-1]) +
    scale_x_continuous(breaks = rot$i[-n], labels = rot$nome[-n]) +
    coord_equal(clip = "off") + labs(x = NULL, y = NULL) +
    tema_aula(base = 15) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          panel.grid = element_blank(), legend.position = "right")
}

#' PERMANOVA: distâncias dos objetos ao centroide do seu grupo (SS_W) e dos
#' centroides dos grupos ao centroide geral (SS_A); dados simulados
fig_permanova_centroides <- function() {
  set.seed(11)
  d <- bind_rows(tibble(x = rnorm(8, -2, .7), y = rnorm(8, 0, .7), g = "A"),
                 tibble(x = rnorm(8, 2, .7), y = rnorm(8, .3, .7), g = "B"))
  cen <- d |> group_by(g) |> summarise(cx = mean(x), cy = mean(y))
  ger <- tibble(x = mean(d$x), y = mean(d$y))
  d <- left_join(d, cen, by = "g")
  pal <- c(A = cores[["accent"]], B = cores[["verde"]])
  base <- function(titulo) ggplot() +
    geom_point(data = d, aes(x, y, fill = g, shape = g), size = 3.6, colour = "white") +
    scale_fill_manual(values = pal, guide = "none") +
    scale_shape_manual(values = c(A = 21, B = 24), guide = "none") +
    coord_equal(xlim = c(-4, 4), ylim = c(-2, 2.2)) +
    labs(title = titulo, x = NULL, y = NULL) +
    theme_void(base_size = 16) +
    theme(plot.title = element_text(hjust = .5, face = "bold", colour = cores[["accent_dark"]]),
          panel.border = element_rect(fill = NA, colour = cores[["neutro"]]),
          plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  p1 <- base("Objetos → centroide do grupo") +
    geom_segment(data = d, aes(x, y, xend = cx, yend = cy), colour = cores[["accent_dark"]],
                 linewidth = .6) +
    geom_point(data = cen, aes(cx, cy), shape = 4, size = 5, stroke = 2, colour = cores[["accent_dark"]]) +
    annotate("text", x = 0, y = -1.8, label = "SS[W]", parse = TRUE, size = 7, colour = "#c0392b")
  p2 <- base("Centroides dos grupos → centroide geral") +
    geom_segment(data = cen, aes(cx, cy, xend = ger$x, yend = ger$y), colour = cores[["accent_dark"]],
                 linewidth = 1.2) +
    geom_point(data = cen, aes(cx, cy), shape = 4, size = 5, stroke = 2, colour = cores[["accent_dark"]]) +
    geom_point(data = ger, aes(x, y), shape = 8, size = 6, stroke = 1.6, colour = "#c0392b") +
    annotate("text", x = 0, y = -1.8, label = "SS[A]", parse = TRUE, size = 7, colour = "#c0392b")
  p1 + p2
}

#' Posição × dispersão: quatro situações com dois grupos (dados simulados)
fig_posicao_dispersao <- function() {
  set.seed(3)
  ger <- function(mx, sd, g, n = 20) tibble(x = rnorm(n, mx, sd), y = rnorm(n, 0, sd), g = g)
  casos <- list(
    "Mesma posição,\nmesma dispersão" = bind_rows(ger(0, 1, "A"), ger(0, 1, "B")),
    "Posição\ndiferente" = bind_rows(ger(-2.5, 1, "A"), ger(2.5, 1, "B")),
    "Dispersão\ndiferente" = bind_rows(ger(0, .5, "A"), ger(0, 2, "B")),
    "Posição e dispersão\ndiferentes" = bind_rows(ger(-2.5, .5, "A"), ger(2.5, 2, "B")))
  d <- bind_rows(lapply(names(casos), \(n) mutate(casos[[n]], caso = n))) |>
    mutate(caso = factor(caso, names(casos)))
  cen <- d |> group_by(caso, g) |> summarise(x = mean(x), y = mean(y), .groups = "drop")
  ggplot(d, aes(x, y, colour = g)) +
    geom_segment(data = d |> left_join(cen, by = c("caso", "g"), suffix = c("", "c")),
                 aes(xend = xc, yend = yc), alpha = .35) +
    geom_point(size = 1.8) +
    geom_point(data = cen, shape = 4, size = 4, stroke = 1.6) +
    facet_wrap(~caso, nrow = 1) +
    scale_colour_manual(values = c(A = cores[["accent"]], B = cores[["ferrugem"]]), name = NULL) +
    coord_equal() + labs(x = NULL, y = NULL) +
    tema_aula(base = 12) + theme(axis.text = element_blank())
}

#' Distâncias dos locais ao centroide do seu grupo (betadisper), anuros
fig_betadisper <- function(bd) {
  d <- tibble(dist = bd$distances, grupo = bd$group)
  ggplot(d, aes(grupo, dist)) +
    geom_boxplot(fill = cores[["neutro_cl"]], colour = cores[["texto"]], width = .45,
                 outlier.shape = NA) +
    geom_jitter(width = .08, colour = oc, size = 3) +
    labs(x = NULL, y = "Distância ao centroide do grupo") +
    tema_aula(base = 15)
}

# ---- Mantel com a diferença simples (Somers & Jackson 2022) -------------------

#' Duas variáveis simuladas com correlação moderada e as três correlações:
#' Pearson entre as variáveis, Mantel com a distância euclidiana |xi − xj| e
#' Mantel com a diferença simples xi − xj (todos os pares ordenados)
mantel_diff_ex <- function(n = 40, rho = .4) {
  set.seed(7)
  x <- rnorm(n); y <- rho * x + sqrt(1 - rho^2) * rnorm(n)
  pares <- expand.grid(i = seq_len(n), j = seq_len(n)) |> subset(i != j)
  dx <- x[pares$i] - x[pares$j]; dy <- y[pares$i] - y[pares$j]
  low <- pares$i > pares$j
  list(x = x, y = y, dx = dx, dy = dy, low = low,
       r = cor(x, y), r_euc = cor(abs(dx[low]), abs(dy[low])), r_diff = cor(dx, dy))
}

fig_mantel_diff <- function(ex = mantel_diff_ex()) {
  f <- function(z) format(round(z, 2), nsmall = 2, decimal.mark = ",")
  pt <- function(d, titulo, xl, yl) ggplot(d, aes(a, b)) +
    geom_point(colour = cores[["accent"]], alpha = .35, size = 1.2) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#c0392b", linewidth = 1) +
    labs(title = titulo, x = xl, y = yl) + tema_aula(base = 14) +
    theme(plot.title = element_text(size = 15))
  p1 <- pt(tibble(a = ex$x, b = ex$y), paste0("Pearson: r = ", f(ex$r)), "x", "y") +
    geom_point(colour = cores[["accent"]], size = 2.2)
  p2 <- pt(tibble(a = abs(ex$dx[ex$low]), b = abs(ex$dy[ex$low])),
           paste0("Euclidiana: rM = ", f(ex$r_euc)), "|xi − xj|", "|yi − yj|")
  p3 <- pt(tibble(a = ex$dx, b = ex$dy), paste0("Diferença simples: rM = ", f(ex$r_diff)),
           "xi − xj", "yi − yj")
  p1 + p2 + p3
}
