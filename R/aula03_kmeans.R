library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
source(here::here("R/diagrama.R"))
source(here::here("R/aula03_hierarquico.R"))   # doubs_chord()

lar <- cores_familia[["Classificação"]]
pal_grupos <- c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]], cores[["ocre"]])

#' Três nuvens de pontos simuladas em dois descritores
dados_kmeans <- function() {
  set.seed(21)
  centros <- rbind(c(2, 2), c(6, 3), c(4, 7))
  do.call(rbind, lapply(1:3, \(k) cbind(rnorm(25, centros[k, 1], .8),
                                         rnorm(25, centros[k, 2], .8))))
}

#' O algoritmo do k-means em quatro quadros: centroides iniciais,
#' atribuição, atualização dos centroides e solução final (dados simulados)
fig_kmeans_passos <- function() {
  pts <- dados_kmeans()
  c0 <- rbind(c(1, 6), c(3, 5.5), c(7, 6))          # começo ruim de propósito
  atribui <- function(cc) apply(pts, 1, \(p) which.min(colSums((t(cc) - p)^2)))
  atualiza <- function(g) t(sapply(1:3, \(k) colMeans(pts[g == k, , drop = FALSE])))
  g1 <- atribui(c0); c1 <- atualiza(g1)
  g2 <- atribui(c1); c2 <- atualiza(g2)
  repeat { g3 <- atribui(c2); c3 <- atualiza(g3); if (all(abs(c3 - c2) < 1e-9)) break; c2 <- c3 }
  quadro <- function(g, cc, titulo, cc_ant = NULL) {
    d <- tibble(x = pts[, 1], y = pts[, 2], g = if (is.null(g)) NA else factor(g))
    p <- ggplot(d, aes(x, y)) +
      geom_point(aes(fill = g), shape = 21, colour = "white", size = 2.6, stroke = .3) +
      scale_fill_manual(values = pal_grupos[1:3], na.value = cores[["neutro"]], guide = "none")
    if (!is.null(cc_ant)) {
      p <- p + annotate("segment", x = cc_ant[, 1], y = cc_ant[, 2], xend = cc[, 1], yend = cc[, 2],
                        arrow = arrow(length = unit(.18, "cm")), colour = cores[["texto"]])
    }
    p + annotate("point", x = cc[, 1], y = cc[, 2], shape = 4, size = 5, stroke = 2.2,
                 colour = cores[["texto"]]) +
      coord_equal(xlim = c(-.5, 8.5), ylim = c(-.5, 9.5)) +
      labs(title = titulo, x = NULL, y = NULL) +
      tema_aula(base = 12) + theme(axis.text = element_blank(),
                                   plot.title = element_text(size = 12))
  }
  quadro(NULL, c0, "1. k centroides iniciais") +
    quadro(g1, c0, "2. Cada objeto vai para\no centroide mais próximo") +
    quadro(g1, c1, "3. Os centroides vão para\na média do grupo", c0) +
    quadro(g3, c3, "4. Repete até nada mudar") +
    plot_layout(nrow = 1)
}

#' Fluxograma do k-means
fig_kmeans_fluxo <- function() {
  ne <- cores[["accent_dark"]]
  cx <- tribble(
    ~x,   ~y,    ~w,  ~h,  ~rotulo,                                    ~cor,
    0,    1.5,   2.4, .9,  "Dados e\nnúmero de grupos k",              ne,
    3.4,  1.5,   2.4, .9,  "Centroides\niniciais",                     ne,
    6.8,  1.5,   2.6, .9,  "Atribui cada objeto\nao centroide mais\npróximo", lar,
    6.8,  -.3,   2.6, .9,  "Recalcula os\ncentroides",                 lar,
    3.4,  -.3,   2.4, .9,  "Os centroides\nmudaram?",                  ne,
    0,    -.3,   2.4, .9,  "Grupos finais",                            ne
  )
  setas <- tribble(
    ~x,    ~y,    ~xend, ~yend, ~curv,
    1.2,   1.5,   2.15,  1.5,   0,
    4.6,   1.5,   5.45,  1.5,   0,
    6.8,   1.05,  6.8,   .2,    0,
    5.5,   -.3,   4.65,  -.3,   0,
    3.4,   .15,   5.45,  1.3,   -.25,
    2.2,   -.3,   1.25,  -.3,   0
  )
  diagrama(cx, setas, base = 4.4) +
    annotate("text", x = 4.05, y = .5, label = "sim", size = 4.2, colour = ne, fontface = "bold") +
    annotate("text", x = 1.7, y = -.05, label = "não", size = 4.2, colour = ne, fontface = "bold")
}

#' Soma de quadrados dentro dos grupos (TESS) contra k, nos peixes do Doubs
#' transformados por chord
fig_kmeans_scree <- function(kmax = 10) {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  x <- vegan::decostand(e$doubs$fish[-8, ], "normalize")
  set.seed(1)
  d <- tibble(k = 1:kmax,
              tess = sapply(1:kmax, \(k) stats::kmeans(x, k, nstart = 100)$tot.withinss))
  ggplot(d, aes(k, tess)) +
    geom_line(colour = lar, linewidth = 1) + geom_point(colour = lar, size = 2.8) +
    scale_x_continuous(breaks = 1:kmax) +
    labs(x = "Número de grupos (k)", y = "TESS (soma de quadrados\ndentro dos grupos)") +
    tema_aula(base = 15)
}

# ---- IndVal ----------------------------------------------------------------

#' IndVal de Dufrêne & Legendre (1997)
#'
#' @param m matriz locais × espécies
#' @param g vetor de grupos dos locais
#' @param nperm permutações para o teste
#' @return tibble com o grupo de maior IndVal de cada espécie, A, B, IndVal e p
indval <- function(m, g, nperm = 999, semente = 1) {
  g <- factor(g)
  calc <- function(gg) {
    medias <- apply(m, 2, \(x) tapply(x, gg, mean))
    A <- sweep(medias, 2, colSums(medias), "/")
    B <- apply(m > 0, 2, \(x) tapply(x, gg, mean))
    iv <- A * B
    list(A = A, B = B, iv = iv, max = apply(iv, 2, max))
  }
  obs <- calc(g)
  set.seed(semente)
  perm <- replicate(nperm, calc(sample(g))$max)
  p <- (rowSums(perm >= obs$max - 1e-12) + 1) / (nperm + 1)
  k <- apply(obs$iv, 2, which.max)
  tibble(especie = colnames(m), grupo = levels(g)[k],
         A = obs$A[cbind(k, seq_along(k))], B = obs$B[cbind(k, seq_along(k))],
         IndVal = obs$max, p = p)
}

#' Exemplo numérico de Legendre & Legendre (2012), Tabela 8.10, modificado
#' de Dufrêne & Legendre (1997): 3 espécies em 25 locais e 5 grupos
indval_ll <- function() {
  m <- cbind(
    `Espécie 1` = rep(c(4, 5, 6, 3, 2), each = 5),
    `Espécie 2` = c(rep(8, 5), rep(4, 5), rep(6, 5), 4, 4, 2, 0, 0, rep(0, 5)),
    `Espécie 3` = c(rep(18, 5), rep(2, 5), rep(0, 15)))
  list(m = m, g = rep(1:5, each = 5))
}

#' Tabela do exemplo: A, B e IndVal de cada espécie em cada grupo
tab_indval_ll <- function() {
  ex <- indval_ll()
  g <- factor(ex$g)
  medias <- apply(ex$m, 2, \(x) tapply(x, g, sum))
  A <- sweep(medias, 2, colSums(medias), "/")
  B <- apply(ex$m > 0, 2, \(x) tapply(x, g, mean))
  f <- \(v) format(round(v, 2), nsmall = 2, decimal.mark = ",")
  out <- lapply(colnames(ex$m), \(s) {
    rbind(setNames(f(A[, s]), paste("Grupo", 1:5)),
          setNames(f(B[, s]), paste("Grupo", 1:5)),
          setNames(f(A[, s] * B[, s]), paste("Grupo", 1:5)))
  })
  tab <- do.call(rbind, out)
  rownames(tab) <- paste(rep(colnames(ex$m), each = 3), c("A", "B", "IndVal"))
  # o maior IndVal de cada espécie em negrito
  for (s in seq_along(colnames(ex$m))) {
    r <- 3 * s; k <- which.max(A[, s] * B[, s])
    tab[r, k] <- paste0("**", tab[r, k], "**")
  }
  as.data.frame(tab)
}

#' Especificidade: a abundância de uma espécie nos grupos de locais, com o
#' grupo de interesse (j) por último (inspirado em Podani & Csányi 2010)
fig_especificidade <- function() {
  casos <- list(a = c(4, 4, 4, 4, 4), b = c(0, 0, 0, 0, 20),
                c = c(20, 0, 0, 0, 0), d = c(5, 5, 5, 5, 0))
  d <- lapply(names(casos), \(n) tibble(caso = n, grupo = 1:5, ab = casos[[n]],
                                        foco = 1:5 == 5)) |> bind_rows()
  rot <- d |> group_by(caso) |>
    summarise(A = paste0("A = ", ab[5], "/", sum(ab), " = ",
                         format(round(ab[5] / sum(ab), 2), decimal.mark = ",")))
  ggplot(d, aes(grupo, ab)) +
    geom_col(aes(fill = foco), width = .7) +
    geom_text(aes(label = ab), vjust = -.4, size = 4) +
    geom_text(data = rot, aes(3, 25, label = A), size = 4.6, fontface = "bold",
              colour = cores[["texto"]]) +
    facet_wrap(~caso, nrow = 1, labeller = labeller(caso = \(x) paste0("(", x, ")"))) +
    scale_fill_manual(values = c(`FALSE` = cores[["neutro"]], `TRUE` = lar), guide = "none") +
    scale_x_continuous(breaks = 1:5, labels = c(rep("", 4), "j")) +
    scale_y_continuous(limits = c(0, 27)) +
    labs(x = "Grupos de locais (o grupo j em destaque)", y = "Abundância") +
    tema_aula(base = 14)
}

#' IndVal das espécies de peixes do Doubs nos quatro grupos do Ward
indval_doubs <- function(k = 4) {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  fish <- e$doubs$fish[-8, ]
  g <- stats::cutree(doubs_hc()$Ward, k)
  indval(as.matrix(fish), g)
}

#' Abundância das espécies indicadoras nos locais do Doubs, com os locais
#' ordenados pelos grupos do Ward
fig_indicadoras <- function(n = 8) {
  e <- new.env()
  utils::data("doubs", package = "ade4", envir = e)
  fish <- e$doubs$fish[-8, ]
  g <- stats::cutree(doubs_hc()$Ward, 4)
  iv <- indval(as.matrix(fish), g)
  top <- iv |> dplyr::filter(p <= 0.05) |> dplyr::arrange(grupo, dplyr::desc(IndVal)) |>
    dplyr::group_by(grupo) |> dplyr::slice_head(n = 3) |> dplyr::ungroup()
  ord <- order(g, as.integer(rownames(fish)))
  d <- tibble::as_tibble(fish[ord, top$especie]) |>
    dplyr::mutate(local = factor(rownames(fish)[ord], rownames(fish)[ord]), grupo = g[ord]) |>
    tidyr::pivot_longer(-c(local, grupo), names_to = "especie", values_to = "ab") |>
    dplyr::mutate(especie = factor(especie, rev(top$especie)))
  ggplot(d, aes(local, especie, fill = ab)) +
    geom_tile(colour = "white") +
    facet_grid(~grupo, scales = "free_x", space = "free_x",
               labeller = labeller(grupo = \(x) paste("Grupo", x))) +
    scale_fill_gradient(low = cores[["papel"]], high = lar, name = "Abundância") +
    labs(x = "Locais", y = NULL) +
    tema_aula(base = 12) +
    theme(axis.text.x = element_text(size = 8), panel.grid = element_blank(),
          legend.position = "right")
}
