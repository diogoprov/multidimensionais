# Análises Multidimensionais: conversão dos slides para Quarto/revealjs

Repositório da disciplina de pós-graduação **Análises Multidimensionais**
(201521415, 45 h), Mestrado em Biologia Animal, UFMS. Site publicado em
<https://provetelab.org/multidimensionais/>. Responda em português.

O plano completo, com o inventário dos `.pptx`, a estimativa de tempo e o
raciocínio de cada decisão, está em `PLANO-conversao-qmd.md`. Este arquivo
guarda as regras e a situação atual. As regras herdadas da Bioestatística
estão em `~/GitHub/bioestatistica/CLAUDE.md` e
`~/GitHub/bioestatistica/WORKFLOW-conversao-qmd.md`; na dúvida, valem as de lá,
salvo quando este arquivo disser outra coisa.

## Objetivo

Converter as 7 aulas, hoje em PowerPoint/PDF, em slides **Quarto/revealjs**
(`slides/aNN.qmd`), com as figuras geradas em R a partir dos conjuntos de
dados dos pacotes (`ade4`, `vegan`) ou de dados simulados. Os scripts de
prática e os walkthroughs continuam nas pastas das aulas.

## Situação das aulas

| Aula | Pasta | Slides hoje | Original (.pptx) | Lote |
|-----:|-------|-------------|------------------|-----:|
| 1 | `aula-01-intro-multivariada` | `slides/aula-01.pdf` | `Aula 1.pptx` (104 slides) | 6 |
| 2 | `aula-02-coeficientes-associacao` | `slides/aula-02.pdf` | `Aula 2.pptx` (84) | 2 |
| 3 | `aula-03-agrupamento` | `slides/aula-03.pdf` | `Aula 3.pptx` (60) | 3 |
| 4 | `aula-04-ordenacoes-irrestritas` | `slides/aula-04.pdf` | `Aula 4.pptx` (97) | **1 (modelo)** |
| 5 | `aula-05-ordenacoes-restritas` | `slides/aula-05.pdf` | `Aula 5.pptx` (138) | 4 |
| 6 | `aula-06-matrizes-mantel-manova` | `slides/aula-06.pdf` | `Aula 6.pptx` (63) | 5 |
| 7 | `aula-07-metodos-baseados-modelos` | `slides/aula-07.pdf` | `Aula 7.pptx` (62) | 7 |

Lote 0 (infraestrutura e correções) feito em out/2026. Próximo: a Aula 4.

## Como trabalhar com o Diogo

- **Commit e push só com aprovação explícita de cada lote.** Mostre o
  resultado antes. Aprovação de um lote não vale para o próximo.
- Combine antes de cortar conteúdo, criar "Para saber mais" ou mudar escopo.
- Aponte os erros do original e relate as correções no fim do lote.
- Aplique a skill **karpathy-guidelines**: mudanças cirúrgicas, critério de
  sucesso verificável.

## Decisões da disciplina

- **Fonte**: os `Aula N.pptx` de
  `~/Library/CloudStorage/OneDrive-Pessoal/Aulas/2019 - Multivariada BioAnimal/`.
  Os outros `.pptx`/`.ppt` da pasta são versões antigas. Os arquivos ficam
  só na nuvem: baixe com `cat "Aula N.pptx" > /dev/null` em segundo plano (o
  macOS não tem `timeout`). Extraia num diretório próprio do scratchpad.
- **Fio condutor**: o mapa dos métodos (`mapa_plot(destaque = ...)`, em
  `R/mapa.R`), no lugar do ciclo PPDAC. Da matriz de dados saem a
  associação (Aula 2), e dela classificação (3), ordenação irrestrita (4),
  ordenação restrita (5) e testes (6); e, sem matriz de distância, os
  métodos baseados em modelos (7).
- **Selos das famílias**: `[Ordenação irrestrita]{.familia .irrestrita}`.
  Classes: `.associacao`, `.classificacao`, `.irrestrita`, `.restrita`,
  `.testes`, `.modelos`. Cores em `cores_familia` (`R/tema.R`) e em
  `$familia-*` (`slides/theme.scss`): mude nos dois lugares.
- **Pacotes**: `vegan`, `ade4` e `cluster`, além de `ggplot2`, `dplyr`,
  `tidyr`, `purrr`, `tibble`, `patchwork`, `ape`, `here` e `knitr`.
  Pergunte antes de adicionar outro.
- **Dados**: os conjuntos dos pacotes (`doubs` do `ade4`, sem CSV; `dune`,
  `varespec`, `mite` etc. do `vegan`) ou dados simulados.
- **Não entram**: Kahoot (não faz sentido na pós), "Brinque com o
  código"/webR (os alunos já sabem R) e "Teste-se".
- **Atividades**: as do original ficam em `::: {.atividade}`. Onde couber,
  "Pergunta para a turma" (*peer instruction*) ou "Leia o biplot".

## Material do Legendre e de outros livros

Quase 60 slides citam Legendre. Tratamento:

1. **Capturas dos slides do curso do Legendre** (exemplos numéricos
   pequenos): recalcular em R com os mesmos números e conferir contra os
   autovalores e coordenadas do original. Na `.ref`: "Exemplo numérico de P.
   Legendre (material do curso de Ecologia Numérica)".
2. **Figuras de livros** (Legendre & Legendre 2012, Borcard et al. 2018,
   McCune & Grace 2002): redesenhar com dados dos pacotes ou simulados.
   "Inspirado na Figura X de ..." só com o número conferido no PDF do
   capítulo (os PDFs estão na pasta do OneDrive).
3. **Quadros-resumo e tabelas**: reescrever como tabela própria em
   português, com "Adaptado de ...".
4. **Artigos**: licença conferida no Crossref; CC BY → figura original com
   crédito.
5. Figura insubstituível: só com exceção autorizada pelo Diogo, registrada
   em `assets/terceiros/FONTES.md`.

## Arquivos de apoio

- `R/tema.R`: `cores`, `cores_familia`, `tema_aula(base = 15, conceitual = FALSE)`.
- `R/mapa.R`: `mapa_plot(destaque = NULL, perguntas = TRUE)`.
- `R/ordena.R`: `escores(ord, ref, eixos = 1:2, scaling = 2)` devolve os
  escores do vegan como tibbles, **com o sinal dos eixos travado**: o
  descritor `ref` fica sempre positivo (no Doubs, `ref = "dfs"`, a distância
  da nascente). `orienta_coord(coord, ref)` faz o mesmo para PCoA e nMDS,
  usando um objeto como referência. Use sempre essas funções nas figuras.
- `slides/theme.scss`, `slides/_metadata.yml`, `slides/_selo-familia.html`:
  tema e selo no canto, copiados da Bioestatística.

## Estrutura de um deck

- YAML igual ao de `~/GitHub/bioestatistica/slides/a02.qmd`, com o footer
  `"Análises Multidimensionais · PPGBA/UFMS"` e o subtítulo
  `"Aula N · Análises Multidimensionais · PPGBA/UFMS"`.
- Chunk `setup` com `source(here::here("R/..."))` → `## Hoje` (+ `.nota`
  sobre as figuras em R) → `## Onde estamos` com `mapa_plot(destaque = ...)`
  (chunk com `fig-height: 5.6` e `out-height: 590px`: no 10 × 4,4 padrão
  o `coord_equal` encolhe o mapa) → blocos `# N. Título {.secao}` com o selo da família.
- **Não ponha `---` antes de um `# `** (gera slide em branco).
- Toda figura com `fig-alt` descritivo em português.
- Números citados no texto (autovalores, % de variação, *stress*, R², *p*)
  saem de `` `r ...` ``, nunca digitados.
- `metaMDS`: `set.seed()` antes e `trace = FALSE`. Testes por permutação:
  semente e número de permutações fixos. *Scaling* sempre explícito no slide.
- Um arquivo `R/aulaNN_bloco.R` por bloco, só com funções `fig_*()`.

## Build e publicação

- O GitHub Actions roda **só o Quarto** (com knitr, sem os pacotes de
  ecologia numérica). Os chunks dos slides funcionam graças ao `freeze:
  auto`: `_freeze/` **é versionado**. Depois de mudar um `.qmd` com código ou
  uma função em `R/`, renderize localmente e commite `_freeze/` junto.
- Se acrescentou ou removeu um chunk, apague `_freeze/slides/aNN` antes.
- Os walkthroughs têm `eval: false` e não executam nada.
- `docs/` não é versionado. Os PDFs antigos de `slides/` continuam em
  `resources`, para não quebrar links.
- `README.md`, `CLAUDE.md` e `PLANO-conversao-qmd.md` ficam fora do render.
- Teste de transbordamento: viewport 1250×750 no navegador embutido, script
  da seção 3.7 do workflow da Bioestatística.

## Scripts de prática: pendências conhecidas

- O `quinta.R` (Aula 5) quebra na seção de LDA: mistura os nomes das
  variáveis do CSV de Borcard (`pen`, `dbo`) com os do `ade4` (`slo`,
  `bdo`). A versão do OneDrive está no meio dessa troca. Resolver no lote da
  Aula 5.
- `exercicio-indval-sem-resposta.R` (Aula 3): o vetor `fitofis` tem 20
  rótulos, mas `bocaina.txt` tem 13 locais (as espécies estão nas linhas).
- As versões de `Material aula prática/` no OneDrive são mais novas que as
  do repositório (usam `ade4::doubs`, `adonis2`, `scores()`), mas estão
  incompletas: o `Quarta.R` de lá lê os CSV e o `doubs` ao mesmo tempo.
- Pacotes que os scripts usam e que não estão na máquina do Diogo:
  `vegan3d`, `pvclust`, `labdsv`, `FD` e `RgoogleMaps`.

## Cursos irmãos

- `~/GitHub/bioestatistica`: Bioestatística (graduação), o modelo desta conversão
- `~/GitHub/modelos-lineares-2026-2`: Análise de Dados Univariados
- `~/GitHub/intro-r`: Introdução ao R
