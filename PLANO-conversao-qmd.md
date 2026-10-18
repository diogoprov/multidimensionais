# Plano: converter as aulas de Análises Multidimensionais para Quarto/revealjs

Documento orientador da conversão dos slides desta disciplina (PowerPoint)
para Quarto/revealjs, no mesmo padrão já aplicado à Bioestatística. Ele
adapta `~/GitHub/bioestatistica/WORKFLOW-conversao-qmd.md` e o `CLAUDE.md`
daquele repositório, que continuam sendo a referência completa das regras.
No lote 0, este plano vira o `CLAUDE.md` do repositório (com a tabela de
situação das aulas).

---

## 1. Ponto de partida

### 1.1 O repositório hoje

| Aula | Pasta | Slides no repo | Código e dados na pasta |
|-----:|-------|----------------|-------------------------|
| 1 | `aula-01-intro-multivariada` | `slides/aula-01.pdf` | nenhum (teórica) |
| 2 | `aula-02-coeficientes-associacao` | `slides/aula-02.pdf` | `chap2.R`, `chap3.R`, `coldiss.R`, `panelutils.R`, `Doubs.RData` + 4 walkthroughs |
| 3 | `aula-03-agrupamento` | `slides/aula-03.pdf` | 3 scripts (cluster/k-means/IndVal, pvclust, exercício IndVal), `DoubsSpe.csv`, `DoubsEnv.csv`, `ambiente.txt`, `bocaina.txt` + 3 walkthroughs |
| 4 | `aula-04-ordenacoes-irrestritas` | `slides/aula-04.pdf` | `Quarta.R` + walkthrough |
| 5 | `aula-05-ordenacoes-restritas` | `slides/aula-05.pdf` | `quinta.R` + walkthrough |
| 6 | `aula-06-matrizes-mantel-manova` | `slides/aula-06.pdf` | `sexta.R`, 8 arquivos de dados (incl. `anuros_permanova.csv`, `repteis_permanova.csv`) + walkthrough |
| 7 | `aula-07-metodos-baseados-modelos` | `slides/aula-07.pdf` | nenhum (aula extra, teórica) |

O site é Quarto (`_quarto.yml`, tema cosmo, `styles.css`, extensão
fontawesome), publicado pelo GitHub Actions. Hoje **nada é executado** no
render (todos os walkthroughs têm `eval: false`) e `_freeze/` não existe.

### 1.2 Fonte: os `.pptx` do OneDrive

Pasta: `~/Library/CloudStorage/OneDrive-Pessoal/Aulas/2019 - Multivariada BioAnimal/`.

Os arquivos ficam só na nuvem até serem lidos (aparecem com 0 B). Para
baixar, faça `cat "Aula N.pptx" > /dev/null` em segundo plano. O macOS não
tem `timeout`. Use os `Aula N.pptx`, que são a versão mais recente; os
outros (`Aula_PCA_PCoA.pptx`, `Análises canônicas.pptx`, `Aula 7 2022.pptx`,
`Multivariada*.ppt`) são versões antigas, úteis só para consulta.

| Aula | Arquivo | Modificado | Slides | Slides com imagem | Notas do apresentador | Slides que citam Legendre |
|-----:|---------|-----------|------:|------------------:|------:|------:|
| 1 | `Aula 1.pptx` (68 MB) | 2025-10 | 104 | 66 | 5 | 8 |
| 2 | `Aula 2.pptx` (23 MB) | 2020-09 | 84 | 45 | 1 | 18 |
| 3 | `Aula 3.pptx` (4 MB) | 2025-10 | 60 | 46 | 0 | 3 |
| 4 | `Aula 4.pptx` (9 MB) | 2023-04 | 97 | 60 | 1 | 10 |
| 5 | `Aula 5.pptx` (21 MB) | 2023-04 | 138 | 76 | 5 | 10 |
| 6 | `Aula 6.pptx` (8 MB) | 2023-04 | 63 | 39 | 1 | 10 |
| 7 | `Aula 7.pptx` (8 MB) | 2022-09 | 62 | 46 | 15 | 0 (2 citam Warton) |
| | **Total** | | **608** | **378** | | **59** |

Contagem automática (texto dos slides com "Legendre"). Slides que mostram uma
figura do livro sem citar o nome no texto não entram nela, então o número
real deve ser maior.

Material de apoio na mesma pasta:

- capítulos em PDF de Legendre & Legendre, Borcard et al. e McCune & Grace
  (as leituras da disciplina);
- `Mapa mental resumo Analise_multivariada.pdf`, base possível para o mapa
  do fio condutor;
- `Material aula prática/`, com versões dos scripts mais novas (out/2025)
  que as do repositório: `Quarta.R`, `quinta.R`, `sexta.R`, `exemplo.Rmd` e
  pastas por dia;
- o backup do Moodle de 2023 (`.mbz`).

### 1.3 Problemas encontrados no repositório (corrigir no lote 0)

1. `aula-04-ordenacoes-irrestritas/index.qmd` aponta para `quarta.R`, mas o
   arquivo é `Quarta.R`. No macOS funciona; no GitHub Pages (Linux, sensível
   a maiúsculas) o link quebra.
2. As páginas das Aulas 4 e 5 dizem "Sem datasets associados", mas
   `Quarta.R` e `quinta.R` leem `DoubsSpe.csv` e `DoubsEnv.csv`, que só
   existem na pasta da Aula 3.
3. `exercicio-indval-sem-resposta.R` lê `exemploIndVal.txt`, que não está no
   repositório (o comentário manda importar `bocaina.txt`).
4. Os comentários dos scripts têm acentos corrompidos (`vari?veis`,
   `diret?rio`): foram salvos em Latin-1 e lidos como UTF-8.
5. Os scripts do repositório são mais antigos que os de
   `Material aula prática/` (seção 1.2). Comparar e decidir qual vale.
6. O `README.md` (e este arquivo) podem estar sendo renderizados como
   páginas do site. Excluir com `"!README.md"` e `"!PLANO-*.md"` em
   `project: render`, como na Bioestatística.

---

## 2. Decisões tomadas (17/out/2026)

| Tema | Decisão |
|------|---------|
| Fonte | `Aula N.pptx` da pasta do OneDrive (seção 1.2) |
| Fio condutor | Mapa de decisão dos métodos (seção 2.1), no lugar do PPDAC |
| Pacotes | `vegan`, `ade4` e `cluster` aprovados, além dos da Bioestatística (`ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`, `patchwork`, `ape`, `here`, `knitr`) |
| Dados das figuras | Conjuntos disponíveis nos pacotes: `doubs` (`ade4`), `dune`/`dune.env`, `varespec`/`varechem`, `mite` (`vegan`) e outros. O Doubs vem do `ade4`, sem CSV. Dados simulados quando o conceito pedir |
| Kahoot | Não entra (não faz sentido na pós) |
| "Brinque com o código" (webR) | Não entra: os alunos já sabem R, e os scripts de prática cumprem esse papel |
| Aula-modelo | Aula 4 (ordenações irrestritas) |
| Sinal dos eixos | Travado por código (seção 6) |
| `metaMDS` | Só `set.seed()`: o que importa é a configuração |

### 2.1 Fio condutor: o mapa dos métodos

Um diagrama desenhado em R (`R/mapa.R`, no papel do `ppdac.R`) aparece no
início de cada aula com a família do dia destacada. Cada bloco da aula
recebe o selo da família, com uma cor fixa por família, a mesma no diagrama
e no `theme.scss`.

| Família | Aulas | Pergunta que responde |
|---|---|---|
| Associação | 2 | Quão parecidos são dois objetos ou descritores? |
| Classificação | 3 | Os objetos formam grupos discretos? |
| Ordenação irrestrita | 4 | Quais são os principais gradientes nos dados? |
| Ordenação restrita | 5 | Quanto da variação de Y é explicada por X? |
| Testes entre matrizes e grupos | 6 | Duas matrizes covariam? Os grupos diferem? |
| Baseados em modelos | 7 | Como modelar a abundância diretamente? |

A Aula 1 apresenta o mapa inteiro. O `Mapa mental resumo` da pasta pode ser
o ponto de partida do desenho.

### 2.2 Ainda em aberto (não bloqueia o lote 0)

- Atividades de sala por aula: quais existem nos `.pptx`. Na pós, sem
  Kahoot, proponho dois formatos: **"Pergunta para a turma"** (*peer
  instruction*, como na Bioestatística) e **"Leia o biplot"** (a turma
  interpreta um resultado antes de ver a resposta em fragmento).
- A Aula 7 (extra) entra na conversão? A estimativa da seção 5 considera
  que sim.
- Os scripts de prática: manter como estão (só com as correções da 1.3) ou
  trocar a leitura dos CSV do Doubs por `data(doubs, package = "ade4")`?

---

## 3. Arquitetura

```
_quarto.yml          # + render list, resources (PDFs antigos), freeze, knitr bg, lang: pt
slides/
  _metadata.yml      # opções comuns dos decks (copiar da Bioestatística)
  _selo-familia.html # selo da família no canto do slide (adaptado de _selo-etapa.html)
  theme.scss         # copiado da Bioestatística; cores das famílias no lugar das do PPDAC
  aNN.qmd            # um deck por aula
  aula-0N.pdf        # ficam, em resources, para não quebrar links antigos
R/
  tema.R             # paletas, tema_aula(), fundo #faf7f0, OutDec = ","
  mapa.R             # mapa_plot(destaque = ...)
  ordena.R           # helpers comuns: orienta_eixos(), escores em tibble para ggplot
  aulaNN_bloco.R     # só funções fig_*(), cada uma com sua semente
data/FONTES.md       # só se entrar algum dado fora dos pacotes
assets/              # figuras externas licenciadas, cada pasta com FONTES.md
_freeze/             # versionado: o CI continua rodando só o Quarto
aula-0N-*/           # páginas das aulas, scripts e walkthroughs (ficam onde estão)
```

- **Copiar, não abstrair**: tema, `_metadata.yml` e `R/tema.R` vêm da
  Bioestatística e são adaptados aqui (footer
  `"Análises Multidimensionais · PPGBA/UFMS"`).
- **CI**: com `freeze: auto` e `_freeze/` versionado, o workflow atual
  basta. `vegan`, `ade4` e `cluster` só são necessários na máquina do Diogo.
  Atualizar o comentário do workflow que diz que nada é executado.
- **YAML de cada deck**: igual ao de `bioestatistica/slides/a02.qmd`
  (`theme: [default, theme.scss]`, 1250×750, `auto-stretch: false`,
  `slide-number: c/t`, `echo: false`, `fig-width: 10`, `fig-height: 4.4`,
  `dev.args: bg: "#faf7f0"`, `lang: pt`).
- **Números no texto saem dos chunks** (autovalores, % de variação,
  *stress*, R² ajustado, *p*), com `` `r ...` `` inline, nunca digitados.

---

## 4. Como lidar com o material do Legendre

É o principal desafio desta disciplina: ~60 slides citam Legendre
explicitamente, sem contar as figuras do livro sem citação no texto. O
material aparece de três formas, e cada uma tem um tratamento.

### 4.1 Capturas dos slides do curso do Legendre ("Slide P. Legendre")

Exemplo: Aula 4, slides de CA e de *scaling* (7 capturas seguidas). São
tabelas pequenas de dados artificiais (6 objetos × 3 espécies, 6 locais × 7
espécies) e as ordenações calculadas a partir delas.

**Tratamento: recalcular em R com os mesmos números.** A tabela vira
`tibble` no `R/aulaNN_*.R`, a ordenação é refeita com `vegan::cca()` ou
`rda()` e o gráfico é redesenhado em `ggplot2`. O texto é traduzido e
reescrito. Na `.ref`: "Exemplo numérico de P. Legendre (material do curso
de Ecologia Numérica)". Assim o slide mostra o mesmo exemplo e o aluno ainda
pode rodar o código. Os autovalores do original (λ₁ = 0,2295 no exemplo da
CA) servem de teste: se o recálculo não bater, há erro na transcrição ou no
*scaling*.

### 4.2 Figuras do livro (Legendre & Legendre 2012, Elsevier; Borcard et al. 2018, Springer)

Sem licença aberta: **redesenhar** com `doubs`, `dune`, `varespec` ou dados
simulados. Na `.ref`: "Inspirado na Figura X de Legendre & Legendre (2012)",
só com o número da figura conferido no PDF do capítulo (que está na pasta do
OneDrive). Esquemas conceituais (o "efeito ferradura", a geometria da PCA, o
fluxograma de escolha de coeficientes) são redesenhados como diagrama em R.

### 4.3 Quadros-resumo e texto (`Legendre quadro resumo*.pdf`, tabelas de coeficientes)

**Reescrever como tabela própria em português** (`.tabela-grande`), citando
a fonte ("Adaptado de Legendre & Legendre 2012, Tabela X"). Ideias e
classificações não têm dono; a diagramação e a redação do livro, sim.

### 4.4 O que não dá para resolver assim

Se alguma figura for insubstituível (por exemplo, uma ilustração que só
funciona como está), há duas saídas:

- **Exceção autorizada pelo Diogo**, com crédito no slide, registrada em
  `assets/terceiros/FONTES.md` (como as exceções da Bioestatística).
- **Pedir permissão ao Legendre** para usar os slides do curso dele. É uma
  decisão sua: não verifiquei a licença do material que ele publica, e
  nenhum contato será feito por mim.

### 4.5 Outras fontes frequentes

- **McCune & Grace 2002** (MjM, sem licença aberta): mesmo tratamento das
  figuras de livro (4.2).
- **Warton et al. e outros artigos** (Aula 7): conferir a licença de cada
  artigo no Crossref (`api.crossref.org/works/<DOI>`). CC BY → usar a
  figura original com crédito; caso contrário, redesenhar.
- **Retratos de autores** (Pearson, Hotelling, Gower, ter Braak, Legendre):
  só domínio público/CC0 do Wikimedia Commons, em `assets/historia/FONTES.md`.
  Retratos de pessoas vivas raramente estão em CC0; na falta, só o nome.

---

## 5. Lotes e estimativa de tempo

### 5.1 Base da estimativa

Na Bioestatística, as 11 aulas (52 a 66 slides por deck, sem Legendre e sem
pacotes de ecologia numérica) foram convertidas em **~15 h de sessão**
(git: 15/out/2026, das 00h às 15h), ou seja, **~1h20 por aula**. A
modelo (Aula 2) levou mais. Depois vieram **~1,5 dia** de acabamento:
fidelidade aos originais, imagens em domínio público, site e extras.

Esta disciplina é mais pesada:

- **608 slides** (87 por aula em média, contra ~60 na Bioestatística);
- 62% dos slides têm imagem, a maioria de livros sem licença aberta, e quase
  tudo precisa ser recalculado ou redesenhado;
- as figuras dependem de ordenações reais (`vegan`), com sinais, *scalings*
  e permutações a controlar;
- em compensação, não há Kahoot, webR nem "Teste-se" a produzir.

### 5.2 Estimativa por lote

| Lote | Conteúdo | Tempo de sessão estimado |
|-----:|----------|-------------------------:|
| 0 | Correções da 1.3, `CLAUDE.md`, `_quarto.yml`, tema, `R/tema.R`, `R/mapa.R`, `R/ordena.R` | 1–1,5 h |
| 1 | **Aula 4 (modelo)**, 97 slides, com as idas e vindas de tema e tom | 3–4 h |
| 2 | Aula 2 (84 slides, muito Legendre: coeficientes) | 2,5–3 h |
| 3 | Aula 3 (60 slides) | 1,5–2 h |
| 4 | Aula 5 (138 slides; candidata a "Para saber mais") | 3,5–4,5 h |
| 5 | Aula 6 (63 slides) | 2–2,5 h |
| 6 | Aula 1 (104 slides, álgebra de matrizes e figuras geométricas) | 2,5–3,5 h |
| 7 | Aula 7 (62 slides, 15 notas, figuras de artigos) | 2–2,5 h |
| 8 | Acabamento: fidelidade aos originais, licenças, página inicial e páginas das aulas | 3–5 h |
| 9 | Revisão da parte prática (pedido do Diogo, 17/out/2026): limpar as páginas das aulas (links e arquivos desnecessários), revisar os walkthroughs e propor melhorias para as aulas práticas, com desafios para os alunos | a estimar |
| | **Total** | **~21–28 h de sessão** |

Isso equivale a **3 a 4 dias de trabalho concentrado**, ou cerca de duas
semanas no ritmo de um lote por dia. O gargalo provavelmente será a sua
revisão de cada lote, não a conversão. Se a Aula 7 ficar em PDF, tire
~2 h.

A estimativa é inferência minha a partir do ritmo da Bioestatística. A
maior incerteza é o número real de figuras do Legendre (seção 1.2): a
Aula 4 vai calibrar o resto.

### 5.3 Ordem e critérios

| Lote | Critério de sucesso |
|-----:|---------------------|
| 0 | `quarto render` sem erros; links das Aulas 4 e 5 funcionando; site publicado igual ao atual |
| 1 | Diogo aprova tema, estrutura, tom, mapa dos métodos e o tratamento do Legendre (4.1–4.3) |
| 2–7 | Cada deck passa no teste de transbordamento e nas conferências da seção 6 |
| 8 | Combinar o escopo antes (remodelação do site em PR separado, como na Bioestatística) |
| 9 | Proposta mostrada e aprovada antes de mexer nos scripts e walkthroughs |

Ordem 4 → 2 → 3 → 5 → 6 → 1 → 7: depois da modelo, segue as dependências de
conteúdo. A Aula 1 fica para depois, quando o estilo das figuras
geométricas já estiver fixado na 4.

Cada lote: converter → renderizar → conferir → **mostrar ao Diogo** →
commit e push só com aprovação → atualizar o `CLAUDE.md` e a memória.
Aprovação de um lote não vale para o próximo.

### 5.4 Pipeline de uma aula

1. **Extrair** o `.pptx` num diretório próprio do scratchpad (dado não
   confiável): texto, notas do apresentador (→ `::: {.nota}`) e
   `ppt/media/`.
2. **Inventariar as figuras**: para cada uma, decidir entre recalcular
   (4.1), redesenhar (4.2), reescrever como tabela (4.3), usar a original
   com licença aberta (4.5) ou retirar. Mostrar o inventário ao Diogo na
   aula-modelo.
3. **Escrever `R/aulaNN_bloco.R`**: só funções `fig_*()`, cada uma com
   `set.seed()` próprio. Ordenações com `vegan`/`ade4`, gráficos com
   `ggplot2` (escores via `scores()`, sem `ggvegan`).
4. **Escrever `slides/aNN.qmd`**: chunk `setup` → `## Hoje` (+ `.nota` sobre
   as figuras em R) → mapa dos métodos → blocos `# N. Título {.secao}` com
   selo. Nunca `---` antes de `# `. Toda figura com `fig-alt` em português.
   Preservar o texto do original; corrigir erros e relatá-los.
5. **Atividades**: tudo que o original tiver vira `::: {.atividade}`; onde
   couber, "Pergunta para a turma" ou "Leia o biplot".
6. **Referências**: só as do original, da página da aula ou verificadas no
   Crossref/Open Library. Na dúvida, `<!-- TODO: referência -->` e aviso.
7. **Página da aula**: botão "Abrir slides" (`../slides/aNN.html`) no lugar
   do PDF, resumo com a família de métodos e `## Conteúdo`. Manter as
   seções de código, dados e leituras.

---

## 6. Sinal dos eixos, sementes e permutações

- **Sinal dos eixos (PCA, CA, PCoA, RDA, CCA)**: o sinal de um autovetor é
  arbitrário e pode mudar entre versões do `vegan`, do BLAS ou da
  plataforma. `orienta_eixos()`, em `R/ordena.R`, fixa a orientação: para
  cada eixo, multiplica escores de locais e de espécies/variáveis por
  `sign()` da carga de uma variável de referência (por exemplo, `dfs`, a
  distância da nascente no Doubs, sempre positiva no eixo 1). Texto e figura
  nunca brigam. *Não achei nas sessões salvas o truque que usamos antes;
  confirmar se é este.*
- **`metaMDS`**: `set.seed()` antes da chamada e `trace = FALSE`. Guardar o
  *stress* para citar no texto via inline.
- **Escalonamento de biplots** (`scaling = 1` × `2`): sempre explícito no
  slide.
- **Testes por permutação** (`anova.cca`, `adonis2`, `mantel`, `protest`):
  semente e número de permutações fixos.
- Funções auxiliares de Borcard et al. (`coldiss.R`, `panelutils.R`) usam
  gráficos base. Nos slides, redesenhar em `ggplot2`.

---

## 7. Conferência antes de mostrar cada lote

- `quarto render slides/aNN.qmd` sem avisos. Se um chunk foi acrescentado ou
  removido, apagar `_freeze/slides/aNN` antes.
- **Teste de transbordamento** no navegador embutido (viewport 1250×750,
  script JS do workflow da Bioestatística, seção 3.7); voltar o viewport
  para `desktop` no fim.
- Cada figura nova salva em PNG no scratchpad e inspecionada: rótulos
  sobrepostos (comum em biplots: selecionar espécies ou deslocar rótulos à
  mão), contraste, pontos com contorno branco, vírgula decimal.
- Recálculos de exemplos do Legendre conferidos contra os valores do
  original (autovalores, coordenadas).
- Nenhum `slides/aNN.rmarkdown` esquecido.
- Antes do push: `git log origin/main..HEAD`; depois, `gh run watch` e
  `curl` na página publicada.

As demais armadilhas da seção 5 do workflow da Bioestatística valem aqui
(subscritos Unicode nas figuras, `sprintf()` e vírgula decimal,
sombreamento em `mutate()`, Mermaid no revealjs, OneDrive travando leitura).

---

## 8. Próximo passo

Lote 0: transformar este plano no `CLAUDE.md`, corrigir os problemas da
seção 1.3 e montar a infraestrutura (tema, `R/tema.R`, `R/mapa.R`,
`R/ordena.R`). Em seguida, o inventário das figuras da Aula 4 para validar o
tratamento do Legendre antes de escrever o deck.
