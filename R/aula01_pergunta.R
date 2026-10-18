# Aula 01: da pergunta à predição e estudos observacionais × experimentais.
# Copiado da Aula 2 da Bioestatística (~/GitHub/bioestatistica/R/aula02_pergunta.R
# e aula02_delineamento.R), com as setas do fluxograma corrigidas: as voltas
# ("não") são linhas cheias vermelhas, para a ponta da seta sair inteira, e os
# rótulos "sim" ficam acima das setas, sem cobri-las.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

cores_niveis <- c("#e6c35c", "#d98a3d", "#a15e3f")
niveis_n <- c("Controle", "Pouco N", "Muito N")

fig_diagrama <- function(nos, arestas, larg = 1.25, alt = .42, base = 5.2,
                         xlim = NULL, ylim = NULL) {
  if (!"tracejada" %in% names(arestas)) arestas$tracejada <- FALSE
  if (!"curva" %in% names(arestas)) arestas$curva <- 0
  if (!"lado" %in% names(arestas)) arestas$lado <- "baixo"
  if (!"lx" %in% names(arestas)) arestas$lx <- NA_real_
  if (!"ly" %in% names(arestas)) arestas$ly <- NA_real_
  pos <- select(nos, id, x, y)
  ar <- arestas |>
    left_join(rename(pos, de = id, x0 = x, y0 = y), by = "de") |>
    left_join(rename(pos, para = id, x1 = x, y1 = y), by = "para") |>
    mutate(
      dx = x1 - x0, dy = y1 - y0,
      # recua a seta até a borda da caixa (larg x alt), pelo lado que ela cruza
      s0 = pmin(abs((larg / 2 + .06) / dx), abs((alt / 2 + .06) / dy)),
      s1 = pmin(abs((larg / 2 + .10) / dx), abs((alt / 2 + .10) / dy)),
      xa = x0 + dx * s0, ya = y0 + dy * s0,
      xb = x1 - dx * s1, yb = y1 - dy * s1,
      xm = coalesce(lx, (xa + xb) / 2), ym = coalesce(ly, (ya + yb) / 2)
    )
  # aresta curva sai e chega pelo mesmo lado (cima ou baixo) das caixas
  curvas <- filter(ar, curva != 0) |>
    mutate(s = ifelse(lado == "cima", 1, -1),
           xa = x0, ya = y0 + s * (alt / 2 + .04),
           xb = x1, yb = y1 + s * (alt / 2 + .08), tracejada = FALSE)
  camadas_curvas <- lapply(seq_len(nrow(curvas)), function(i)
    geom_curve(data = curvas[i, ],
               aes(x = xa, y = ya, xend = xb, yend = yb, linetype = tracejada),
               curvature = curvas$curva[i], linewidth = 1.1,
               colour = "#c0392b",
               arrow = arrow(length = unit(.3, "cm"), type = "closed")))
  g <- ggplot() +
    geom_segment(data = filter(ar, curva == 0),
                 aes(x = xa, y = ya, xend = xb, yend = yb,
                     linetype = tracejada),
                 linewidth = 1.3, colour = cores[["neutro"]],
                 arrow = arrow(length = unit(.32, "cm"), type = "closed")) +
    camadas_curvas +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"),
                          guide = "none") +
    geom_label(data = filter(ar, !is.na(rotulo)),
               aes(xm, ym, label = rotulo), size = base * .9,
               fontface = "bold", fill = cores[["papel"]],
               colour = cores[["texto"]], linewidth = 0) +
    geom_tile(data = nos, aes(x, y), width = larg, height = alt,
              fill = nos$cor, colour = NA) +
    geom_text(data = nos, aes(x, y, label = rotulo), colour = "white",
              size = base, lineheight = .95, fontface = "bold") +
    coord_equal(clip = "off", xlim = xlim, ylim = ylim) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  g
}

# Da pergunta à predição, com as voltas do fluxo (Ruxton & Colegrave 2016,
# cap. 2).
fig_fluxo_pergunta <- function() {
  passos <- c(P = "Pergunta\nclara", H = "Hipóteses", R = "Predições",
              L = "Seguem da\nhipótese?", T = "São\ntestáveis?",
              C = "Planejar\na coleta")
  nos <- tibble::tibble(id = names(passos), rotulo = unname(passos),
                        x = (seq_along(passos) - 1) * 2.75, y = 0,
                        cor = c(rep(cores[["accent"]], 3),
                                rep(cores[["ocre"]], 2), cores[["verde"]]))
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo, ~tracejada, ~curva, ~lado,   ~lx,  ~ly,
    "P", "H",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "H", "R",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "R", "L",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "L", "T",   "sim",   FALSE,      0,      "baixo", 9.625, .38,
    "T", "C",   "sim",   FALSE,      0,      "baixo", 12.375, .38,
    # indo para a esquerda, curvatura positiva arqueia para cima
    "L", "H",   "não",   TRUE,       .4,     "cima",  5.5,  1.45,
    "T", "R",   "não",   TRUE,       -.4,    "baixo", 8.25, -1.45
  )
  fig_diagrama(nos, arestas, larg = 1.85, alt = .8, base = 5,
               ylim = c(-1.6, 1.6))
}

fig_hipotese_n <- function() {
  nos <- tibble::tribble(
    ~id, ~rotulo,                              ~x,  ~y, ~cor,
    "N", "Nitrogênio\nno solo",                 2.6, 0,  cores[["verde"]],
    "C", "Taxa de crescimento\nper capita",      0,   0,  cores[["accent"]]
  )
  arestas <- tibble::tibble(de = "N", para = "C", rotulo = "+")
  fig_diagrama(nos, arestas, larg = 1.45, alt = .6)
}

# ---------------------------------------------------------------------------
# Expressão gráfica das hipóteses: nula e três alternativas.
# ---------------------------------------------------------------------------
dados_hipotese <- function(tipo, n = 9, seed = 2) {
  set.seed(seed)
  x <- sort(runif(n, .5, 9.5))
  f <- switch(tipo,
    nula      = function(x) rep(5, length(x)),
    linear    = function(x) 1.2 + .75 * x,
    saturante = function(x) 1 + 6.5 * x / (1.3 + x),
    unimodal  = function(x) 1.5 + 1.9 * x - .19 * x^2
  )
  list(pontos = tibble::tibble(x = x, y = f(x) + rnorm(n, 0, .75)),
       curva  = tibble::tibble(x = seq(.2, 9.8, length.out = 200),
                               y = f(seq(.2, 9.8, length.out = 200))))
}

fig_hipotese <- function(tipo = "nula", titulo = NULL, base = 15) {
  d <- dados_hipotese(tipo)
  ggplot(d$pontos, aes(x, y)) +
    geom_line(data = d$curva, colour = cores[["verde"]], linewidth = 1.4) +
    geom_point(size = 3.2, colour = cores[["accent"]]) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
    labs(x = "Nitrogênio no solo (mg/g)",
         y = "Crescimento per capita", title = titulo) +
    tema_aula(base, conceitual = TRUE)
}

fig_hipoteses_alternativas <- function() {
  fig_hipotese("linear",    "H1: linear",    base = 13) +
  fig_hipotese("saturante", "H2: saturante", base = 13) +
  fig_hipotese("unimodal",  "H3: unimodal",  base = 13)
}

fig_confundimento_dag <- function(mostrar_agua = TRUE) {
  nos <- tibble::tribble(
    ~id, ~rotulo,                          ~x,   ~y,   ~cor,
    "C", "Taxa de crescimento\nper capita", 0,    0,    cores[["accent"]],
    "N", "Nitrogênio\nno solo",             2.8,  0,    cores[["verde"]],
    "A", "Água\nno solo",                   1.4, -1.25, cores[["ferrugem"]]
  )
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo, ~tracejada,
    "N", "C",   "?",     FALSE,
    "A", "C",   "+",     FALSE,
    "A", "N",   "+",     FALSE
  )
  if (!mostrar_agua) {
    nos <- filter(nos, id != "A")
    arestas <- filter(arestas, de != "A") |> mutate(rotulo = "+")
  }
  fig_diagrama(nos, arestas, larg = 1.5, alt = .62, ylim = c(-1.6, .4))
}

dados_observacional <- function(n = 60, seed = 12) {
  set.seed(seed)
  tibble::tibble(agua = runif(n, 5, 35)) |>
    mutate(nitrogenio = 1 + .08 * agua + rnorm(n, 0, .35),
           # o nitrogênio NÃO tem efeito; só a água
           crescimento = .5 + .05 * agua + rnorm(n, 0, .18),
           classe_agua = cut(agua, c(5, 15, 25, 35), include.lowest = TRUE,
                             labels = c("seco", "intermediário", "úmido")))
}

fig_confundimento_dados <- function(d = dados_observacional()) {
  cor_agua <- c(seco = "#d98a3d", intermediário = "#8a8172",
                úmido = cores[["accent"]])
  a <- ggplot(d, aes(nitrogenio, crescimento)) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["texto"]],
                linewidth = 1) +
    geom_point(size = 2.8, colour = cores[["neutro"]]) +
    labs(x = "Nitrogênio no solo (mg/g)", y = "Crescimento per capita",
         title = "Todas as parcelas juntas") +
    tema_aula(14)
  b <- ggplot(d, aes(nitrogenio, crescimento, colour = classe_agua)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    geom_point(size = 2.8) +
    scale_colour_manual(values = cor_agua, name = "Água no solo") +
    labs(x = "Nitrogênio no solo (mg/g)", y = NULL,
         title = "Separando por umidade") +
    tema_aula(14)
  a + b
}

# ---------------------------------------------------------------------------
# O mesmo problema como experimento: 30 parcelas sorteadas entre 3 níveis.
# ---------------------------------------------------------------------------
dados_experimento <- function(seed = 15) {
  set.seed(seed)
  expand.grid(col = 1:10, lin = 1:3) |>
    mutate(agua = 8 + 2.6 * col + rnorm(n(), 0, 1.5),
           trat = factor(sample(rep(niveis_n, 10)), levels = niveis_n),
           efeito = c(0, .12, .35)[as.integer(trat)],
           crescimento = .5 + .02 * agua + efeito + rnorm(n(), 0, .12))
}

fig_experimento_mapa <- function(d = dados_experimento()) {
  fundo <- tibble::tibble(x = seq(.4, 10.6, by = .05))
  a <- ggplot() +
    geom_raster(data = tidyr::crossing(fundo, y = c(.4, 3.6)),
                aes(x, y, fill = x), interpolate = TRUE, alpha = .6) +
    scale_fill_gradient(low = "#e8c99a", high = "#8fb3c4", guide = "none") +
    geom_tile(data = d, aes(col, lin, colour = trat), fill = "white",
              width = .72, height = .72, linewidth = 2.2) +
    geom_text(data = d, aes(col, lin, label = substr(trat, 1, 1)),
              size = 4.6, fontface = "bold", colour = cores[["texto"]]) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n),
                        name = NULL) +
    annotate("text", x = c(.5, 10.5), y = 3.85, hjust = c(0, 1),
             label = c("solo mais seco", "solo mais úmido"), size = 4.4) +
    coord_equal(clip = "off") +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          legend.position = "bottom")
  b <- ggplot(d, aes(trat, agua, colour = trat)) +
    geom_boxplot(width = .5, outlier.shape = NA, fill = NA, linewidth = .8) +
    geom_jitter(width = .1, height = 0, size = 2.2) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n),
                        guide = "none") +
    labs(x = NULL, y = "Água no solo (%)",
         title = "Água por tratamento") +
    tema_aula(14)
  a + b + plot_layout(widths = c(2.2, 1))
}

fig_experimento_resultado <- function(d = dados_experimento()) {
  r <- d |>
    group_by(trat) |>
    summarise(m = mean(crescimento), n = n(),
              ep = sd(crescimento) / sqrt(n), .groups = "drop") |>
    mutate(li = m - qt(.975, n - 1) * ep, ls = m + qt(.975, n - 1) * ep)
  ggplot(d, aes(trat, crescimento)) +
    geom_jitter(width = .1, height = 0, size = 3.4, shape = 21,
                fill = cores[["accent"]], colour = "white", stroke = .8) +
    geom_pointrange(data = r, aes(y = m, ymin = li, ymax = ls),
                    colour = cores[["accent_dark"]], size = .9,
                    linewidth = 1.4, position = position_nudge(x = .25)) +
    labs(x = NULL, y = "Crescimento per capita") +
    tema_aula(15)
}

