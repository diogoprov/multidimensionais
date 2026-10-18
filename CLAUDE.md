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
| 1 | `aula-01-intro-multivariada` | **`slides/a01.qmd` ✔** (81 slides; revisada duas vezes em 18/out: conteúdo do original restaurado slide a slide, início igual à Aula 2 da Bioestatística) | `Aula 1.pptx` (104 slides) | 6 |
| 2 | `aula-02-coeficientes-associacao` | **`slides/a02.qmd` ✔** (62 slides; revisada pelo Diogo em 18/out) | `Aula 2.pptx` (84) | 2 |
| 3 | `aula-03-agrupamento` | **`slides/a03.qmd` ✔** (44 slides; aguarda revisão do Diogo) | `Aula 3.pptx` (60) | 3 |
| 4 | `aula-04-ordenacoes-irrestritas` | **`slides/a04.qmd` ✔** (73 slides) | `Aula 4.pptx` (97) | **1 (modelo)** |
| 5 | `aula-05-ordenacoes-restritas` | **`slides/a05.qmd` ✔** (57 slides; aguarda revisão do Diogo) | `Aula 5.pptx` (138) | 4 |
| 6 | `aula-06-matrizes-mantel-manova` | **`slides/a06.qmd` ✔** (29 slides; aguarda revisão do Diogo) | `Aula 6.pptx` (63) | 5 |
| 7 | `aula-07-metodos-baseados-modelos` | `slides/aula-07.pdf` | `Aula 7.pptx` (62) | 7 |

Lote 0 (infraestrutura e correções) e lote 1 (Aula 4, a aula-modelo) feitos
em out/2026. O inventário de cada aula (ação por slide, erros do original)
fica em `inventarios/aNN.md`. A Aula 2 foi convertida em out/2026 sem parar
para aprovação, a pedido do Diogo, que revisa depois. As Aulas 1, 3, 5 e 6 também.
Próximo: a Aula 7.

**Depois de todas as aulas** (pedido do Diogo, 17/out/2026): revisar as
páginas de cada aula para retirar links e arquivos desnecessários, revisar
os walkthroughs e propor melhorias para a parte prática da disciplina,
incluindo desafios para os alunos. Mostrar a proposta antes de mexer.

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
- **Pacotes**: `vegan`, `ade4`, `cluster` e `pvclust` (este a pedido do Diogo, na Aula 3), além de `ggplot2`, `dplyr`,
  `tidyr`, `purrr`, `tibble`, `patchwork`, `ape`, `here` e `knitr`.
  Também em uso: `ggrepel` (rótulos do triplot com variável categórica, Aula
  5) e `spdep` (redes dos MEMs, Aula 5); `rphylopic` só para baixar as
  silhuetas, fora do render. Pergunte antes de adicionar outro.
- **Dados**: os conjuntos dos pacotes (`doubs` do `ade4`, sem CSV; `dune`,
  `varespec`, `mite` etc. do `vegan`) ou dados simulados.
- **Não entram**: Kahoot (não faz sentido na pós), "Brinque com o
  código"/webR (os alunos já sabem R), "Teste-se" e "Pergunta para a
  turma": as atividades ficam por conta dos exercícios práticos.
- **Atividades**: as do original ficam em `::: {.atividade}`. A única que
  se acrescenta é **"Leia o biplot"**: um biplot real, três perguntas e a
  resposta em `::: {.fragment .definicao}` (modelo na Aula 4).
- **Slides de Thiago Gonçalves-Souza** (crédito no rodapé do original):
  redesenhar e creditar na `.ref` como "Baseado em slides de Thiago
  Gonçalves-Souza".
- **Memes**: mantidos, no espírito do original, com pinturas em domínio
  público do Commons e legenda própria (classe `.meme`; imagens e licenças
  em `assets/memes/FONTES.md`). Fotos de pessoas, fotomontagens, GIFs e
  imagens de banco saem.
- **"Para saber mais"**: tabelas que o Diogo deixa só para consulta (como a
  Tabela 9.4 de L&L) vão para uma seção final `# Para saber mais {.secao}`.

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

- `R/tema.R`: `cores`, `cores_familia`, `tema_aula(base = 15, conceitual = FALSE)`
  e `separa_rotulos(x, y, dx, dy)`, que afasta os nomes das setas quase
  paralelas nos biplots (sem `ggrepel`).
- `R/diagrama.R`: `diagrama(caixas, setas)`, para fluxogramas e árvores de
  decisão (caixas arredondadas e setas).
- `R/dendro.R`: `fig_dendro(hc, ate, corte, grupos)`, dendrograma horizontal
  em ggplot2 (como em L&L), com fusões até `ate` destacadas (para mostrar o
  agrupamento passo a passo).
- `lda_manual()` (em `R/aula05_lda.R`): LDA sem `MASS`, conferida com
  `MASS::lda`. `mite_varpart()` (em `R/aula05_parcial.R`): partição de
  variância dos ácaros com PCNMs selecionados.
- `ordiR2step()` e `ordistep()` reavaliam a chamada `rda()`: o arquivo que
  as usa precisa de `library(vegan)`, não basta `vegan::`.
- `indval()` (em `R/aula03_kmeans.R`): IndVal de Dufrêne & Legendre com teste
  de permutação, sem pacote extra.
- Memes já usados (não repetir sem motivo): *Melencolia I* (Aulas 2 e 4),
  *O Pesadelo* (4), *O Grito*, Caravaggio e Wright of Derby (2), Inocêncio X
  (Velázquez), *O desesperado* (Courbet) e *No café* (Degas) (3), *Sísifo*
  (Ticiano), *A lição de anatomia* (Rembrandt), *A cartomante* (La Tour) e
  *O sono da razão* (Goya) (5), *O homem com a enxada* (Millet) (6), *A parábola dos cegos* (Bruegel),
  *O comedor de feijão* (Carracci), *Washington cruzando o Delaware* (Leutze),
  *A infância de Raleigh* (Millais), *Tédio* (Sickert), *Filósofo em meditação*
  (Rembrandt), *A escola da aldeia* (Steen) e *O almoço dos barqueiros* (Renoir) (1).
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
- Um arquivo `R/aulaNN_bloco.R` por bloco, só com funções `fig_*()` (na
  Aula 4: `aula04_intro.R`, `_pca.R`, `_ca.R`, `_pcoa.R`, `_nmds.R`).
- **Tamanho do texto** (pedido do Diogo): corpo em 32 px; nada abaixo de
  ~26 px fora das `.ref`. Evite o `{.smaller}` do reveal (22 px). Para slide
  denso, use `{.medio}` (~27 px, em `slides/theme.scss`); se ainda não
  couber, divida o slide. Tabelas pequenas vão em `.tabela-grande`; tabela
  alta e estreita pode ser transposta (como os loadings da Aula 4). Figuras
  devem ocupar o espaço livre (`out-height`).
- **Passo a passo de contas**: quando o original mostra de onde vem cada
  número (centralizar, montar **S**, resolver a equação característica),
  preserve esse detalhe, com um passo por fragmento.
- **Referências com link** (pedido do Diogo): toda referência verificada na
  `.ref` leva link para o DOI, `[Autor (ano)](https://doi.org/...)`.
  Legendre & Legendre (2012) aponta para o capítulo citado, pelo DOI do
  capítulo (o ScienceDirect bloqueia acesso automatizado; o DOI resolve
  para a página certa): cap. 1 `10.1016/B978-0-444-53868-0.50001-0`;
  cap. 2 `...50002-2`; cap. 7 `...50007-1`; cap. 8 `...50008-3`; cap. 9
  `...50009-5`; cap. 10 `...50010-1`; cap. 11 `...50011-3`. Borcard et al.
  (2018): livro `10.1007/978-3-319-71404-2`, capítulo N com sufixo `_N`
  (cap. 5 = `_5`). McCune & Grace e Gotelli & Ellison ficam sem link.
- **Espaço**: slide de uma frase só vai para o slide vizinho; lista curta
  ganha uma tabela ou figura que acrescente informação (como a tabela de
  propriedades em "Três tipos de coeficientes", na Aula 2). Meça a altura
  útil e mire em ~500–740 px.
- **Preserve os esquemas e alterne visual e texto** (regra do Diogo, Aula 5):
  todo esquema do original (matrizes, fluxos, triplots montados em camadas,
  passos de um cálculo) vira figura em R; não resuma vários slides de
  esquema num slide de texto. Evite sequências de slides só de texto.
  Sequências construídas aos poucos no original (o triplot, o dbMEM, a
  partição de variância) viram vários slides com a mesma figura crescendo.
- **Silhuetas de organismos**: do PhyloPic (`rphylopic`), salvas em
  `assets/phylopic/` com autor e licença em `assets/phylopic/FONTES.md`.
- **Conteúdo comum com a Bioestatística** (pergunta, hipóteses,
  confundimento, experimento): reaproveitar os slides e as funções de
  `~/GitHub/bioestatistica` (cópia em `R/aula01_pergunta.R`), não reescrever.
- **Esquemas já feitos em outros cursos** (como as origens do zero, do
  curso de Análise de Dados Univariados) são reaproveitados de
  `assets/esquemas/`, com a origem em `assets/esquemas/FONTES.md`.
- **Números com vírgula dentro de LaTeX**: no modo matemático a vírgula
  vira pontuação e ganha espaço ("64, 3"). Escreva `8{,}2` e, para números
  calculados, use `numm()` (definida no `setup` da Aula 4) em vez de `num()`.

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
- Teste de transbordamento: viewport 1250×750 no navegador embutido. Se o
  painel for menor, o reveal escala o palco (`Reveal.getScale()` < 1) e o
  teste em coordenadas absolutas do workflow da Bioestatística falha calado.
  Meça em unidades de slide: `(rect.bottom - topo de .reveal .slides) /
  Reveal.getScale()` > 745 é transbordamento. Antes de medir, recarregue as
  imagens (`img.src += '?t=' + Date.now()`): o navegador guarda os PNGs
  antigos, que têm o mesmo nome.

## Armadilhas encontradas

- `Shepard()` é do `MASS`, não do `vegan`. Para a linha em degraus do
  diagrama de Shepard do nMDS, use `stats::isoreg()`.
- O `convert` do ImageMagick instalado em `/usr/local/bin` é um binário
  Intel e não roda. Para miniaturas dos slides: `gs -sDEVICE=png16m` no PDF
  e o pacote `magick` do R para montar folhas de contato.
- Para ver um `.pptx` inteiro, o PDF de `slides/` serve quando tem o mesmo
  número de páginas e o mesmo texto (conferir com `gs -sDEVICE=txtwrite`).
  Na Aula 4 era o caso.
- No R, `cca()` e `eigen()` devolvem eixos com o sinal trocado em relação
  aos exemplos do Legendre. `escores()`/`orienta_coord()` resolvem; escolha
  a referência que reproduz a orientação do original.

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
