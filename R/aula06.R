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
fig_procrustes_passos <- function() {
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
  painel(A, B, "Dados originais") + painel(A1, B1, "1. Translação") +
    painel(A2, B2, "2. Escalonamento") + painel(A2, B3, "3. Rotação") +
    plot_layout(nrow = 1)
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
    scale_y_reverse(breaks = rot$i, labels = rot$nome) +
    scale_x_continuous(breaks = rot$i, labels = rot$nome, position = "top") +
    coord_equal() + labs(x = NULL, y = NULL) +
    tema_aula(base = 17) +
    theme(axis.text.x = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom")
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
