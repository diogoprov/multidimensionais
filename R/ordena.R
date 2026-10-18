library(tibble)

# O sinal de um eixo de ordenação é arbitrário: a mesma PCA pode sair com o
# eixo 1 invertido em outra versão do vegan, do BLAS ou do sistema. Para o
# texto do slide e a figura não brigarem, cada eixo é orientado de modo que
# um descritor de referência fique sempre do lado positivo.

#' Sinal que deixa o descritor de referência positivo em cada eixo
#'
#' @param desc matriz de escores dos descritores (espécies ou variáveis nas
#'   linhas, eixos nas colunas)
#' @param ref nome do descritor de referência; um só (vale para todos os
#'   eixos) ou um por coluna de `desc`. Escolha um descritor com carga
#'   alta no eixo, para o sinal não depender de um valor perto de zero.
sinais_eixos <- function(desc, ref) {
  ref <- rep_len(ref, ncol(desc))
  s <- sign(desc[cbind(match(ref, rownames(desc)), seq_len(ncol(desc)))])
  if (anyNA(s)) stop("descritor de referência ausente: ",
                     paste(ref[is.na(s)], collapse = ", "))
  s[s == 0] <- 1
  s
}

#' Escores de uma ordenação do vegan, com os eixos orientados
#'
#' Funciona com rda(), cca(), capscale()/dbrda() e decorana(). Devolve uma
#' lista de tibbles (`sites`, `species` e, nas restritas, `biplot`) com a
#' coluna `rotulo` e uma coluna por eixo, com os nomes do vegan (PC1, CA1,
#' RDA1...), prontas para o ggplot.
#'
#' @param ord objeto de ordenação
#' @param ref descritor de referência (ver sinais_eixos())
#' @param eixos eixos a extrair
#' @param scaling escalonamento do vegan (1 = distâncias entre objetos,
#'   2 = correlações entre descritores)
escores <- function(ord, ref, eixos = 1:2, scaling = 2) {
  disp <- c("sites", "species")
  if (!is.null(ord$CCA)) disp <- c(disp, "bp")
  sc <- vegan::scores(ord, choices = eixos, scaling = scaling, display = disp)
  s <- sinais_eixos(sc$species, ref)
  lapply(sc, function(m) {
    m <- sweep(as.matrix(m), 2, s, `*`)
    tibble::as_tibble(m, rownames = "rotulo")
  })
}

#' Orienta os eixos de uma matriz de coordenadas qualquer
#'
#' Para PCoA (cmdscale(), ape::pcoa()) e nMDS, que não têm escores de
#' descritores: o objeto de referência (uma linha de `coord`) fica do lado
#' positivo de cada eixo.
#'
#' @param coord matriz de coordenadas (objetos nas linhas)
#' @param ref nome (ou nomes, um por eixo) da linha de referência
orienta_coord <- function(coord, ref) {
  sweep(as.matrix(coord), 2, sinais_eixos(as.matrix(coord), ref), `*`)
}
