library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/ordena.R"))

az <- cores_familia[["Ordenação irrestrita"]]

# ---- dados -------------------------------------------------------------

#' Rio Doubs (Verneaux 1973, via ade4): 29 locais, sem o local 8, que não
#' tem peixes. `env` = 11 variáveis ambientais; `fish` = 27 espécies.
dados_doubs <- function() {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  list(env = e$doubs$env[-8, ], fish = e$doubs$fish[-8, ])
}

#' Exemplo numérico de Legendre & Legendre (2012, seção 9.1): 5 objetos,
#' 2 descritores
exemplo_ll <- function() {
  matrix(c(2, 3, 5, 7, 9, 1, 4, 0, 6, 2), 5,
         dimnames = list(paste0("x", 1:5), c("y1", "y2")))
}

#' PCA do exemplo, passo a passo: centrar, dispersão, autoanálise e
#' escores. Os autovetores são orientados com y1 positivo no eixo 1 e y2
#' positivo no eixo 2, como no livro.
pca_ll <- function() {
  y  <- exemplo_ll()
  yc <- scale(y, scale = FALSE)
  s  <- crossprod(yc) / (nrow(y) - 1)
  e  <- eigen(s)
  u  <- e$vectors
  dimnames(u) <- list(colnames(y), c("PC1", "PC2"))
  u  <- sweep(u, 2, sinais_eixos(u, c("y1", "y2")), `*`)
  list(y = y, yc = yc, s = s, lambda = e$values, u = u, f = yc %*% u)
}

#' PCA das variáveis ambientais do Doubs (matriz de correlação)
pca_doubs <- function() vegan::rda(dados_doubs()$env, scale = TRUE)

# ---- esquemas ----------------------------------------------------------

#' Regressão simples: Y = a + bX (o ponto de partida da combinação linear)
fig_regressao <- function() {
  ggplot() +
    annotate("segment", x = .5, y = 1.2, xend = 4.5, yend = 3.4,
             colour = az, linewidth = 1.3) +
    annotate("text", x = 4.1, y = 1.1, label = "Y == a + b*X", parse = TRUE,
             size = 7, colour = cores[["texto"]]) +
    labs(x = "X", y = "Y") +
    coord_cartesian(xlim = c(0, 5), ylim = c(0, 4), expand = FALSE) +
    tema_aula(base = 18, conceitual = TRUE)
}

#' Nuvem de pontos simulada: dados brutos, centrados e eixos principais
#'
#' Cada ponto é uma localidade, com duas variáveis ambientais. Os três
#' painéis mostram o que a PCA faz: centrar e rotacionar.
fig_nuvem <- function() {
  set.seed(42)
  n <- 70
  temp <- rnorm(n, 24, 3)
  umid <- 40 + (temp - 24) + rnorm(n, 0, 1.6)
  d  <- tibble(temp, umid)
  dc <- d |> mutate(temp = temp - mean(temp), umid = umid - mean(umid))
  e  <- eigen(cov(dc))
  u  <- e$vectors
  u  <- sweep(u, 2, sign(u[1, ]), `*`)
  pc <- tibble(eixo = c("PC1", "PC2"),
               x = u[1, ] * c(8, 4.5), y = u[2, ] * c(8, 4.5))

  pontos <- function(dd) geom_point(data = dd, aes(temp, umid), shape = 21,
                                    fill = az, colour = "white", size = 2.6,
                                    stroke = .4)
  t <- tema_aula(base = 14, conceitual = TRUE) +
    theme(plot.title = element_text(size = rel(1)))
  p1 <- ggplot() + pontos(d) +
    coord_equal() +
    labs(title = "1. Dados brutos", x = "Temperatura", y = "Umidade") + t
  p2 <- ggplot() +
    geom_hline(yintercept = 0, linetype = "22", colour = cores[["neutro"]]) +
    geom_vline(xintercept = 0, linetype = "22", colour = cores[["neutro"]]) +
    pontos(dc) + coord_equal() +
    labs(title = "2. Centrados na média", x = "Temperatura", y = "Umidade") + t
  p3 <- ggplot() + pontos(dc) +
    geom_segment(data = pc, aes(-x, -y, xend = x, yend = y),
                 colour = cores[["ferrugem"]], linewidth = 1.1) +
    geom_text(data = pc, aes(x * 1.12, y * 1.12, label = eixo), size = 4.6,
              colour = cores[["ferrugem"]], fontface = "bold") +
    coord_equal() +
    labs(title = "3. Rotação: novos eixos",
         x = "Temperatura", y = "Umidade") + t
  p1 + p2 + p3
}

#' Exemplo de L&L: dados brutos (a) e centrados (b)
fig_exemplo_ll <- function() {
  p <- pca_ll()
  d  <- as_tibble(p$y, rownames = "obj")
  dc <- as_tibble(p$yc, rownames = "obj")
  m  <- colMeans(p$y)
  t  <- tema_aula(base = 15) + theme(panel.grid.major = element_line(colour = "#ebe5d8"))
  pa <- ggplot(d, aes(y1, y2)) +
    geom_hline(yintercept = m[2], linetype = "22", colour = cores[["neutro"]]) +
    geom_vline(xintercept = m[1], linetype = "22", colour = cores[["neutro"]]) +
    geom_point(size = 3.5, colour = az) +
    geom_text(aes(label = obj), hjust = -.45, size = 5, colour = cores[["texto"]]) +
    annotate("text", x = m[1] + .2, y = 6.7, hjust = 0, size = 4.4,
             label = paste0("médias: ", format(m[1], decimal.mark = ","),
                            " e ", format(m[2], decimal.mark = ","))) +
    coord_equal(xlim = c(0, 10), ylim = c(0, 7)) +
    labs(title = "(a) Brutos", x = expression(y[1]), y = expression(y[2])) + t
  pb <- ggplot(dc, aes(y1, y2)) +
    geom_hline(yintercept = 0, colour = cores[["texto"]]) +
    geom_vline(xintercept = 0, colour = cores[["texto"]]) +
    geom_point(size = 3.5, colour = az) +
    geom_text(aes(label = obj), hjust = -.45, size = 5, colour = cores[["texto"]]) +
    coord_equal(xlim = c(-5, 5), ylim = c(-3, 4)) +
    labs(title = "(b) Centrados",
         x = expression(y[1] - bar(y)[1]), y = expression(y[2] - bar(y)[2])) + t
  pa + pb
}

#' Exemplo de L&L: os eixos principais sobre os dados centrados e os
#' escores dos objetos no novo sistema de coordenadas
fig_exemplo_eixos <- function() {
  p <- pca_ll()
  dc <- as_tibble(p$yc, rownames = "obj")
  f  <- as_tibble(p$f, rownames = "obj")
  u  <- p$u
  ex <- tibble(eixo = c("PC1", "PC2"), x = u[1, ] * 4.8, y = u[2, ] * 4.8)
  t  <- tema_aula(base = 15) + theme(panel.grid.major = element_line(colour = "#ebe5d8"))
  pa <- ggplot(dc, aes(y1, y2)) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]]) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]]) +
    geom_segment(data = ex, aes(-x, -y, xend = x, yend = y),
                 colour = cores[["ferrugem"]], linewidth = 1) +
    geom_text(data = ex, aes(x * 1.1, y * 1.1, label = eixo),
              colour = cores[["ferrugem"]], fontface = "bold") +
    geom_point(size = 3.5, colour = az) +
    geom_text(aes(label = obj), hjust = -.45, size = 5, colour = cores[["texto"]]) +
    coord_equal(xlim = c(-5.5, 5.5), ylim = c(-4, 4.5)) +
    labs(title = "(c) Eixos principais",
         x = expression(y[1] - bar(y)[1]), y = expression(y[2] - bar(y)[2])) + t
  pb <- ggplot(f, aes(PC1, PC2)) +
    geom_hline(yintercept = 0, colour = cores[["ferrugem"]]) +
    geom_vline(xintercept = 0, colour = cores[["ferrugem"]]) +
    geom_point(size = 3.5, colour = az) +
    geom_text(aes(label = obj), hjust = -.45, size = 5, colour = cores[["texto"]]) +
    coord_equal(xlim = c(-5.5, 5.5), ylim = c(-4, 4.5)) +
    labs(title = "(d) Escores: F = Yc U", x = "PC1", y = "PC2") + t
  pa + pb
}

#' Projeção dos objetos no PC1: a PCA minimiza a soma dos quadrados das
#' distâncias perpendiculares ao eixo
fig_projecao <- function() {
  p <- pca_ll()
  dc <- as_tibble(p$yc, rownames = "obj")
  u1 <- p$u[, 1]
  pr <- dc |> mutate(t = y1 * u1[1] + y2 * u1[2], px = t * u1[1], py = t * u1[2])
  ggplot(pr) +
    annotate("segment", x = -5.5 * u1[1], y = -5.5 * u1[2],
             xend = 5.5 * u1[1], yend = 5.5 * u1[2],
             colour = cores[["ferrugem"]], linewidth = 1.1) +
    annotate("text", x = 5.9 * u1[1], y = 5.9 * u1[2], label = "PC1",
             colour = cores[["ferrugem"]], fontface = "bold", size = 5) +
    geom_segment(aes(y1, y2, xend = px, yend = py), colour = cores[["ocre"]],
                 linewidth = .9, linetype = "22") +
    geom_point(aes(px, py), shape = 21, fill = "white", colour = cores[["ferrugem"]],
               size = 3) +
    geom_point(aes(y1, y2), size = 3.5, colour = az) +
    geom_text(aes(y1, y2, label = obj), nudge_x = -.4, nudge_y = .3) +
    annotate("text", x = -5.3, y = -3.3, hjust = 0, size = 4.8, colour = cores[["ocre"]],
             label = "resíduos: perpendiculares ao eixo") +
    coord_equal(xlim = c(-5.5, 5.5), ylim = c(-3.5, 4)) +
    labs(x = expression(y[1] - bar(y)[1]), y = expression(y[2] - bar(y)[2])) +
    tema_aula(base = 15)
}

#' Mínimos quadrados × eixo maior: o que cada um minimiza
fig_mqo_eixo_maior <- function() {
  set.seed(7)
  n <- 14
  x <- runif(n, 1, 9)
  y <- 1 + .8 * x + rnorm(n, 0, 1.1)
  d <- tibble(x, y)
  # MQO
  b <- coef(lm(y ~ x))
  dm <- d |> mutate(yhat = b[1] + b[2] * x)
  # eixo maior: primeiro autovetor da matriz de dispersão
  u <- eigen(cov(cbind(x, y)))$vectors[, 1]
  m <- colMeans(cbind(x, y))
  de <- d |> mutate(t = (x - m[1]) * u[1] + (y - m[2]) * u[2],
                    px = m[1] + t * u[1], py = m[2] + t * u[2])
  t <- tema_aula(base = 15)
  base_pts <- geom_point(shape = 21, fill = az, colour = "white", size = 3.2)
  p1 <- ggplot(dm, aes(x, y)) +
    geom_abline(intercept = b[1], slope = b[2], colour = cores[["ferrugem"]],
                linewidth = 1) +
    geom_segment(aes(xend = x, yend = yhat), colour = cores[["ocre"]], linetype = "22") +
    base_pts + coord_equal() +
    labs(title = "Mínimos quadrados (regressão)",
         subtitle = "minimiza as distâncias verticais") + t
  p2 <- ggplot(de, aes(x, y)) +
    geom_abline(intercept = m[2] - u[2] / u[1] * m[1], slope = u[2] / u[1],
                colour = cores[["ferrugem"]], linewidth = 1) +
    geom_segment(aes(xend = px, yend = py), colour = cores[["ocre"]], linetype = "22") +
    base_pts + coord_equal() +
    labs(title = "Eixo maior (= PC1)",
         subtitle = "minimiza as distâncias perpendiculares") + t
  p1 + p2
}

# ---- PCA do Doubs ------------------------------------------------------

#' Autovalores da PCA do Doubs, com o modelo de broken stick e a média
#' dos autovalores (Kaiser-Guttman). Recalculado com os dados da Fig. 5.1
#' de Borcard et al. (2018).
fig_autovalores <- function() {
  pca <- pca_doubs()
  ev <- pca$CA$eig
  p  <- length(ev)
  bs <- rev(cumsum(1 / (p:1)) / p) * sum(ev)
  d  <- tibble(eixo = factor(names(ev), names(ev)), ev = unname(ev), bs = bs)
  ggplot(d, aes(eixo)) +
    geom_col(aes(y = ev), fill = az, width = .7) +
    geom_hline(yintercept = mean(ev), linetype = "22", colour = cores[["ocre"]],
               linewidth = .9) +
    annotate("text", x = p, y = mean(ev) + .25, hjust = 1, colour = cores[["ocre"]],
             label = "média dos autovalores (Kaiser-Guttman)", size = 4.4) +
    geom_line(aes(y = bs, group = 1), colour = cores[["ferrugem"]], linewidth = 1) +
    geom_point(aes(y = bs), colour = cores[["ferrugem"]], size = 2.4) +
    annotate("text", x = 3.3, y = bs[2] + .35, hjust = 0, colour = cores[["ferrugem"]],
             label = "broken stick", size = 4.6) +
    labs(x = NULL, y = "Autovalor") +
    tema_aula(base = 15)
}

#' Loadings: correlação de Pearson entre as variáveis e os escores dos
#' locais nos três primeiros eixos
tab_loadings <- function() {
  pca <- pca_doubs()
  env <- dados_doubs()$env
  sc <- escores(pca, ref = "dfs", eixos = 1:3)$sites
  r <- cor(env, as.matrix(sc[, -1]))
  ev <- pca$CA$eig[1:3]
  out <- rbind(r, "% da variação" = 100 * ev / pca$tot.chi)
  out <- as.data.frame(out)
  out[] <- lapply(out, \(v) format(round(v, 2), nsmall = 2, decimal.mark = ","))
  out
}

#' Arco entre dois ângulos (em graus), para marcar ângulos nos biplots
arco <- function(de, ate, raio, n = 60) {
  a <- seq(de, ate, length.out = n) * pi / 180
  tibble(x = raio * cos(a), y = raio * sin(a))
}

#' Ângulo de cada seta, em graus, a partir do lado positivo do PC1
angulo_seta <- function(sp, var) {
  v <- unlist(sp[sp$rotulo == var, c("PC1", "PC2")])
  atan2(v[[2]], v[[1]]) * 180 / pi
}

#' Biplot da PCA do Doubs, montado em camadas para a leitura passo a passo
#'
#' Todas as camadas de um mesmo slide usam o mesmo `quadro`, para que as
#' figuras empilhadas (.r-stack) fiquem exatamente sobrepostas.
#'
#' @param scaling 1 (distâncias entre locais) ou 2 (correlações entre
#'   variáveis)
#' @param camadas o que desenhar: "quadrantes", "locais", "setas"
#' @param destaque anotações: "eixos" (os dois PCs), "escore" (o local
#'   `local` com as suas coordenadas), "distancia" (entre os locais `par`),
#'   "comprimento" (a seta mais longa e a mais curta), "angulo_eixo"
#'   (nitrato e pH em relação ao PC1), "angulo_vars" (ângulos entre pares de
#'   setas), "centro" (círculo em volta da origem) e "projecao" (o local
#'   `local` projetado na seta de `variavel`)
#' @param quadro "locais" ou "setas": o que define os limites dos eixos
fig_biplot <- function(scaling = 1, camadas = c("locais", "setas"),
                       destaque = NULL, local = "15", variavel = "nit",
                       par = c("1", "30"),
                       quadro = if ("setas" %in% camadas) "setas" else "locais") {
  e <- escores(pca_doubs(), ref = "dfs", scaling = scaling)
  sp <- e$species; si <- e$sites
  # setas com o mesmo alcance dos locais, como o biplot do vegan
  k <- 0.9 * max(abs(as.matrix(si[, 2:3]))) / max(abs(as.matrix(sp[, 2:3])))
  sp <- sp |> mutate(PC1 = PC1 * k, PC2 = PC2 * k)
  amp <- diff(range(c(si$PC2, sp$PC2)))
  sp <- sp |> mutate(lx = PC1 * 1.13, ly = separa_rotulos(lx, PC2 * 1.13, .25 * amp, .07 * amp))
  lim <- max(abs(c(si$PC1, si$PC2, sp$lx, sp$ly)))
  if (quadro == "locais") {
    qx <- max(abs(si$PC1)); qy <- max(abs(si$PC2)) * 1.35
    xl <- c(-qx, qx) * 1.05; yl <- c(-qy, qy) * 1.05
  } else {
    xl <- range(c(si$PC1, sp$lx)) + c(-.1, .1)
    yl <- range(c(si$PC2, sp$ly)) + c(-.1, .1)
  }
  oc <- cores[["ocre"]]
  rotulo_oc <- function(x, y, txt, ...)
    annotate("label", x = x, y = y, label = txt, colour = oc, size = 4.4,
             fontface = "bold", label.size = 0, fill = cores[["papel"]], ...)
  ponto <- function(id) unlist(si[si$rotulo == id, c("PC1", "PC2")])

  sp$cor <- cores[["ferrugem"]]
  if ("comprimento" %in% destaque) {
    comp <- sqrt(sp$PC1^2 + sp$PC2^2)
    sp$cor[-c(which.max(comp), which.min(comp))] <- cores[["neutro"]]
  }
  g <- ggplot() +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4)
  if ("eixos" %in% destaque) {
    g <- g +
      annotate("segment", x = xl[1], xend = xl[2], y = 0, yend = 0, colour = oc,
               linewidth = 1.4, arrow = arrow(length = unit(.3, "cm"))) +
      annotate("segment", x = 0, xend = 0, y = yl[1], yend = yl[2], colour = oc,
               linewidth = 1.4, arrow = arrow(length = unit(.3, "cm"))) +
      rotulo_oc(.97 * xl[2], .1 * yl[2], "1º componente\nprincipal (PC1)", hjust = 1,
                lineheight = .9) +
      rotulo_oc(.04 * xl[2], .95 * yl[2], "2º componente principal (PC2)", hjust = 0)
  }
  if ("quadrantes" %in% camadas) {
    qx <- max(abs(si$PC1)); qy <- max(abs(si$PC2)) * 1.35
    quad <- tibble(x = c(qx, -qx, -qx, qx), y = c(qy, qy, -qy, -qy),
                   h = c(1, 0, 0, 1), v = c(1, 1, 0, 0),
                   txt = c("PC1 +\nPC2 +", "PC1 \u2212\nPC2 +",
                           "PC1 \u2212\nPC2 \u2212", "PC1 +\nPC2 \u2212"))
    g <- g + geom_text(data = quad, aes(x, y, label = txt, hjust = h, vjust = v),
                       colour = oc, size = 4.6, fontface = "bold", lineheight = .9)
  }
  if ("setas" %in% camadas) {
    g <- g +
      geom_segment(data = sp, aes(0, 0, xend = PC1, yend = PC2, colour = cor),
                   linewidth = .8, arrow = arrow(length = unit(.17, "cm"))) +
      geom_text(data = sp, aes(lx, ly, label = rotulo, colour = cor),
                size = 4.4, fontface = "bold") +
      scale_colour_identity()
  }
  if ("locais" %in% camadas) {
    g <- g + geom_text(data = si, aes(PC1, PC2, label = rotulo), colour = az, size = 3.6)
  }
  if ("escore" %in% destaque) {
    s <- ponto(local)
    g <- g +
      annotate("segment", x = 0, xend = s[1], y = s[2], yend = s[2],
               colour = oc, linetype = "22", linewidth = .9) +
      annotate("segment", x = s[1], xend = s[1], y = 0, yend = s[2],
               colour = oc, linetype = "22", linewidth = .9) +
      annotate("point", x = s[1], y = s[2], shape = 21, size = 8, stroke = 1.1,
               colour = oc) +
      rotulo_oc(s[1], -.12 * yl[2], format(round(s[1], 2), nsmall = 2)) +
      rotulo_oc(.03 * xl[2], s[2], format(round(s[2], 2), nsmall = 2), hjust = 0)
  }
  if ("distancia" %in% destaque) {
    a <- ponto(par[1]); b <- ponto(par[2])
    g <- g +
      annotate("segment", x = a[1], xend = b[1], y = a[2], yend = b[2],
               colour = oc, linewidth = 1.1) +
      annotate("point", x = c(a[1], b[1]), y = c(a[2], b[2]), shape = 21,
               size = 7, stroke = 1.1, colour = oc) +
      rotulo_oc((a[1] + b[1]) / 2, (a[2] + b[2]) / 2 + .08 * yl[2], "d")
  }
  if ("angulo_eixo" %in% destaque) {
    marca <- function(var, raio) {
      a <- angulo_seta(sp, var); m <- a / 2 * pi / 180
      list(geom_path(data = arco(0, a, raio), aes(x, y), colour = oc, linewidth = 1.1),
           rotulo_oc((raio + .1 * lim) * cos(m), (raio + .1 * lim) * sin(m),
                     paste0(round(abs(a)), "\u00b0")))
    }
    g <- g + marca("nit", .62 * lim) + marca("pH", .38 * lim)
  }
  if ("angulo_vars" %in% destaque) {
    par_ang <- function(v1, v2, raio, volta = FALSE) {
      a1 <- angulo_seta(sp, v1); a2 <- angulo_seta(sp, v2)
      if (volta) a2 <- a2 + 360
      m <- (a1 + a2) / 2 * pi / 180
      list(geom_path(data = arco(a1, a2, raio), aes(x, y), colour = oc, linewidth = 1.1),
           rotulo_oc(1.25 * raio * cos(m), 1.25 * raio * sin(m),
                     paste0(round(abs(a2 - a1)), "\u00b0")))
    }
    g <- g + par_ang("pho", "nit", .55 * lim) + par_ang("nit", "pH", .3 * lim) +
      par_ang("oxy", "bdo", .16 * lim, volta = TRUE)
  }
  if ("centro" %in% destaque) {
    g <- g + geom_path(data = arco(0, 360, raio_centro * lim), aes(x, y),
                       colour = oc, linewidth = 1.1)
  }
  if ("projecao" %in% destaque) {
    v <- unlist(sp[sp$rotulo == variavel, c("PC1", "PC2")]); v <- v / sqrt(sum(v^2))
    s <- ponto(local)
    t <- sum(s * v)
    g <- g +
      annotate("segment", x = s[1], y = s[2], xend = t * v[1], yend = t * v[2],
               colour = oc, linewidth = 1, linetype = "22") +
      annotate("point", x = t * v[1], y = t * v[2], colour = oc, size = 3) +
      annotate("point", x = s[1], y = s[2], shape = 21, size = 6, stroke = 1,
               colour = oc)
  }
  g + coord_equal(xlim = xl, ylim = yl, expand = FALSE) +
    labs(x = "PC1", y = "PC2", subtitle = paste("Scaling", scaling)) +
    tema_aula(base = 14)
}

#' Raio do círculo do centro do biplot, como fração do alcance do gráfico
raio_centro <- 0.2

#' Locais dentro do círculo do centro no biplot em scaling 2
locais_centro <- function() {
  e <- escores(pca_doubs(), ref = "dfs", scaling = 2)
  si <- e$sites; sp <- e$species
  k <- 0.9 * max(abs(as.matrix(si[, 2:3]))) / max(abs(as.matrix(sp[, 2:3])))
  lim <- max(abs(c(si$PC1, si$PC2, 1.13 * k * sp$PC1, 1.13 * k * sp$PC2)))
  dentro <- sqrt(si$PC1^2 + si$PC2^2) < raio_centro * lim
  sort(as.integer(si$rotulo[dentro]))
}

#' Correlação das variáveis do Doubs com os dois primeiros PCs
cor_eixos <- function() {
  si <- escores(pca_doubs(), ref = "dfs", scaling = 1)$sites
  stats::cor(dados_doubs()$env, si[, c("PC1", "PC2")])
}

#' Só as setas do scaling 2, em unidades de correlação com os eixos (os
#' escores do vegan no scaling 2 são proporcionais a elas), com a ponta da
#' seta de `variavel` projetada nos dois eixos
fig_biplot_setas <- function(variavel = "pho") {
  r <- cor_eixos()
  sp <- as_tibble(r, rownames = "rotulo") |>
    mutate(lx = PC1 * 1.13, ly = separa_rotulos(lx, PC2 * 1.13, .2, .075))
  v <- r[variavel, ]
  oc <- cores[["ocre"]]
  ggplot(sp) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    annotate("segment", x = v[1], xend = v[1], y = 0, yend = v[2], colour = oc,
             linetype = "22", linewidth = .9) +
    annotate("segment", x = 0, xend = v[1], y = v[2], yend = v[2], colour = oc,
             linetype = "22", linewidth = .9) +
    annotate("label", x = v[1], y = .08, label = format(round(v[1], 2), nsmall = 2), colour = oc,
             size = 4.4, fontface = "bold", label.size = 0, fill = cores[["papel"]]) +
    annotate("label", x = -.1, y = v[2], label = format(round(v[2], 2), nsmall = 2), colour = oc,
             size = 4.4, fontface = "bold", label.size = 0, fill = cores[["papel"]]) +
    geom_segment(aes(0, 0, xend = PC1, yend = PC2), colour = cores[["ferrugem"]],
                 linewidth = .8, arrow = arrow(length = unit(.17, "cm"))) +
    geom_text(aes(lx, ly, label = rotulo), colour = cores[["ferrugem"]],
              size = 4.4, fontface = "bold") +
    annotate("point", x = v[1], y = v[2], shape = 21, size = 6, stroke = 1.1, colour = oc) +
    coord_equal(xlim = c(-1, 1.1), ylim = c(-.75, .65)) +
    labs(x = "Correlação com o PC1", y = "Correlação com o PC2",
         subtitle = "Scaling 2, só os descritores") +
    tema_aula(base = 14)
}

#' PCA sobre a matriz de covariância × de correlação (Doubs)
fig_cov_cor <- function() {
  env <- dados_doubs()$env
  um <- function(scale, titulo) {
    pca <- vegan::rda(env, scale = scale)
    sp <- escores(pca, ref = "dfs", scaling = 2)$species
    ggplot(sp) +
      geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
      geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
      geom_segment(aes(0, 0, xend = PC1, yend = PC2), colour = cores[["ferrugem"]],
                   linewidth = .8, arrow = arrow(length = unit(.17, "cm"))) +
      geom_text(aes(PC1 * 1.1, PC2 * 1.1, label = rotulo), size = 4.2,
                colour = cores[["ferrugem"]], fontface = "bold") +
      coord_equal() +
      labs(title = titulo, x = "PC1", y = "PC2") +
      tema_aula(base = 14)
  }
  um(FALSE, "Covariância (brutas)") + um(TRUE, "Correlação (padronizadas)") +
    plot_layout(widths = c(1.5, 1))
}

#' Círculo de equilíbrio: PCA dos peixes do Doubs transformados por
#' Hellinger, escalonamento 1. Setas além do círculo de raio sqrt(d/p)
#' contribuem mais do que a média para o plano dos eixos 1 e 2.
fig_circulo_equilibrio <- function() {
  hel <- vegan::decostand(dados_doubs()$fish, "hellinger")
  pca <- stats::prcomp(hel)
  u <- pca$rotation[, 1:2]
  u <- sweep(u, 2, sinais_eixos(u, "Satr"), `*`)
  p <- nrow(u)
  raio <- sqrt(2 / p)
  d <- as_tibble(u, rownames = "esp") |>
    mutate(fora = sqrt(PC1^2 + PC2^2) > raio,
           lx = PC1 * 1.12, ly = separa_rotulos(lx, PC2 * 1.12, .06, .028))
  circ <- tibble(a = seq(0, 2 * pi, length.out = 200),
                 x = raio * cos(a), y = raio * sin(a))
  ggplot(d) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_vline(xintercept = 0, colour = cores[["neutro"]], linewidth = .4) +
    geom_path(data = circ, aes(x, y), colour = cores[["ocre"]], linewidth = .9) +
    geom_segment(aes(0, 0, xend = PC1, yend = PC2,
                     colour = fora), linewidth = .7,
                 arrow = arrow(length = unit(.15, "cm"))) +
    geom_text(aes(lx, ly, label = esp, colour = fora), size = 3.8) +
    scale_colour_manual(values = c(`TRUE` = cores[["ferrugem"]], `FALSE` = cores[["neutro"]]),
                        guide = "none") +
    coord_equal() +
    labs(x = "PC1", y = "PC2") +
    tema_aula(base = 14)
}

#' Normal bivariada simulada, com as distribuições marginais
fig_normal_bivariada <- function() {
  set.seed(3)
  n <- 3000
  x <- rnorm(n)
  y <- .6 * x + rnorm(n, 0, .8)
  d <- tibble(x, y)
  t <- tema_aula(base = 14)
  centro <- ggplot(d, aes(x, y)) +
    geom_point(colour = az, alpha = .25, size = .8) +
    stat_ellipse(level = .95, colour = cores[["verde"]], linewidth = 1.1) +
    labs(x = "X", y = "Y") + t
  topo <- ggplot(d, aes(x)) + geom_density(fill = cores[["ferrugem"]], alpha = .5,
                                            colour = cores[["ferrugem"]]) +
    theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  lado <- ggplot(d, aes(y)) + geom_density(fill = cores[["accent"]], alpha = .5,
                                            colour = cores[["accent"]]) +
    coord_flip() + theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  topo + plot_spacer() + centro + lado +
    plot_layout(ncol = 2, widths = c(4, 1), heights = c(1, 4))
}
