# Workflow: converter uma disciplina de PowerPoint para Quarto/revealjs

Relato da conversão de **Análises Multidimensionais** (7 aulas, out/2026),
escrito para a próxima: a reformulação de **Métodos Filogenéticos
Comparativos**. Parte do workflow herdado da Bioestatística
(`~/GitHub/bioestatistica/WORKFLOW-conversao-qmd.md`) e registra o que mudou,
o que deu certo e os erros, para não repeti-los.

Resultado: 7 decks (`slides/a01.qmd` a `a07.qmd`, de 45 a 82 slides cada),
figuras geradas em R a partir de dados de pacotes ou simulados, inventário
por aula (`inventarios/aNN.md`), parte prática com walkthroughs e desafios,
e o site no visual da Bioestatística (mapa dos métodos, Comece aqui,
Programa, Roteiros práticos, Glossário).

---

## 1. Como trabalhar com o Diogo

O que ficou claro ao longo da conversão (vale para a próxima disciplina):

- **Português** na conversa; o conteúdo dos slides é em português, e a
  terminologia técnica pode ficar em inglês.
- **Autorização**: começou "commit e push só com aprovação de cada lote".
  Depois da aula-modelo, virou "converta sem esperar, eu reviso no site".
  Esse segundo modo funcionou muito melhor: converter → renderizar → medir →
  commit → push na `main` → acompanhar o deploy → conferir no ar → seguir.
- **A revisão chega slide a slide**, por URL (`.../a05.html#/titulo-do-slide`),
  muitas vezes no meio do trabalho. Trate cada uma na hora, publique e volte
  ao que estava fazendo.
- **Ele conhece o próprio original de cor.** Quando algo some, ele percebe.
  "Cortou muito conteúdo", "faltou o esquema", "no original havia um slide
  que explicava visualmente cada parte da fórmula": essas foram as críticas
  mais frequentes (ver seção 5).
- Regras explícitas que ele deu, em ordem de importância:
  1. **Preserve ao máximo os esquemas do original e alterne visual e texto.**
     Nada de sequências de slides só de texto. Sequências montadas aos poucos
     no original (o triplot, o biplot, o dbMEM, a partição de variância, a
     ilustração da poça) viram vários slides com a mesma figura crescendo.
  2. **Não corte conteúdo.** Slide de uma frase pode ir para o vizinho, mas
     um bloco do original não pode desaparecer. Na dúvida, mantenha.
  3. **Passo a passo das contas**: quando o original mostra de onde vem cada
     número, preserve, com um passo por fragmento.
  4. **Texto grande** (corpo ≥ ~26 px) e uso do espaço do slide.
  5. **Referências com link** para o DOI; Legendre & Legendre pelo DOI do
     capítulo; livros com capítulos online (Ruxton & Colegrave, Oxford) pelo
     DOI de cada capítulo.
  6. **Reaproveite** esquemas e slides de cursos irmãos (a Aula 1 reaproveitou
     a Aula 2 da Bioestatística; as origens do zero vieram do curso de
     univariada).
  7. **Pergunte antes de adicionar um pacote** novo.
- Ele gosta de **figuras dos próprios trabalhos** (o capítulo 2 do *Análises
  ecológicas no R*, a figura da RLQ do material suplementar de
  Franco-Belussi et al. 2017, a ilustração da poça de Provete et al. 2011) e
  de material **brasileiro**.

---

## 2. Fases do projeto (na ordem em que funcionou)

| Fase | O que foi feito | Observação |
|---|---|---|
| 0. Plano | `PLANO-conversao-qmd.md`: inventário dos `.pptx`, estimativa, decisões (fio condutor, pacotes, dados, o que não entra) | Discutir as decisões **antes** de escrever qualquer slide |
| 1. Infraestrutura | `CLAUDE.md`, tema, `R/tema.R`, `R/mapa.R`, `R/diagrama.R`, `R/ordena.R`, selos das famílias | Copiar da Bioestatística e adaptar |
| 2. Aula-modelo | Aula 4 inteira, revisada pelo Diogo duas vezes | Fixa o padrão de texto, figura e referência para as outras |
| 3. Demais aulas | Inventário → funções R → deck → render → medição → publicação | A partir da Aula 2, sem esperar aprovação |
| 4. Revisões | Rodadas de feedback por URL; duas revisões completas da Aula 1 | Ver seção 5: quase todas por conteúdo cortado |
| 5. Prática | Sessões remotas em paralelo, um PR por aula (walkthrough, script, desafios) | Funcionou bem; o `_freeze` precisa ser gerado no Mac |
| 6. Site | Visual da Bioestatística (PR remoto) + Comece aqui, Roteiros práticos, Glossário | Ver seção 4 |

**O que eu mudaria**: fazer o **mapeamento slide a slide** (seção 3.2)
*antes* de escrever o deck, em todas as aulas. Na Aula 1 eu resumi demais
e precisei de duas revisões completas para devolver o que faltava.

---

## 3. Pipeline de uma aula

### 3.1 Fonte

- Os `.pptx` mais recentes estavam no OneDrive
  (`.../Aulas/2019 - Multivariada BioAnimal/Aula N.pptx`), só na nuvem.
  Baixe com `cat "Aula N.pptx" > destino.pptx` (o macOS não tem `timeout`).
- Extraia num diretório do scratchpad e gere:
  - **texto por slide e notas** (`ppt/slides/slideN.xml`, `ppt/notesSlides/`),
    com um script Python que lê `<a:t>`;
  - **mídia por slide**: os `ppt/slides/_rels/slideN.xml.rels` dizem que
    imagem (`ppt/media/imageNN`) está em cada slide. Foi assim que achei as
    figuras dos artigos e a figura do próprio Diogo;
  - **folhas de contato** do PDF (`gs -sDEVICE=png16m -r40`) montadas com o
    pacote `magick` do R. O `convert` do ImageMagick instalado é um binário
    Intel e não roda.
- Os slides que são **só imagem** não aparecem no texto extraído. Só as
  folhas de contato mostram o que eles tinham. Olhe todas.

### 3.2 Inventário (`inventarios/aNN.md`)

O formato que funcionou:

1. **Resumo por bloco**: origem do material → ação.
2. **Tabela slide a slide** (slide do original → o que vira no deck:
   figura em R, figura original licenciada, texto, sai). É a garantia de que
   nada some. Use-a também para conferir o deck pronto.
3. **Erros do original** (cada aula teve vários; ver seção 5).
4. **Pendências** para o Diogo (licenças a confirmar, referências não
   achadas, decisões de conteúdo).
5. Ao fim de cada revisão, um parágrafo do que mudou e por quê.

### 3.3 Código R

- Um arquivo `R/aulaNN_bloco.R` por bloco, só com funções `fig_*()` e de
  dados. Helpers compartilhados em `R/tema.R` (cores, `tema_aula()`),
  `R/diagrama.R` (caixas e setas), `R/ordena.R` (escores com o sinal dos
  eixos travado), `R/mapa.R` (o mapa dos métodos), `R/dendro.R`.
- **Trave o sinal dos eixos** (um descritor de referência sempre positivo):
  o R inverte eixos em relação ao Legendre e entre versões.
- **Recalcule os exemplos numéricos do original** e confira contra os
  números dele. Foi assim que apareceram os erros de conta e de dados.
- Sementes fixas em tudo que é aleatório; `metaMDS` com `set.seed()`.
- Teste cada figura num PNG antes de pôr no deck (montagens com `magick`
  para ver várias de uma vez). Rótulos sobrepostos aparecem aí.
- Para esquemas: `diagrama()` com caixas e setas; matrizes como tabelas
  desenhadas; chaves (`{`) como `annotate("path")`; LaTeX com `\underbrace`
  e `\color` para fórmulas anotadas (Procrustes, r de Pearson, GLLVM).
- Silhuetas de organismos: **PhyloPic** via `rphylopic` (fora do render),
  salvas em `assets/phylopic/` com autor e licença.

### 3.4 O `.qmd` dos slides

- YAML igual ao da Bioestatística; `## Hoje`, `## Onde estamos` com o mapa
  destacado, seções `# N. Título {.secao}` com o selo da família.
- Classes: `.medio` (~27 px), `.tabela-grande`, `.definicao`, `.ref`,
  `.nota`, `.meme` (`.meme-topo`, `.meme-base`), `.codigo-compacto`.
- **Todo meme precisa de um título `##` próprio.** Sem título, o bloco cai
  dentro do slide anterior.
- Números citados no texto saem de `` `r ...` ``, nunca digitados.
- Figuras de terceiros: só com licença conferida (seção 3.5), com a licença
  escrita na `.ref` e registrada em `assets/terceiros/FONTES.md`.

### 3.5 Referências e licenças (regra rígida)

- **Nunca invente** referência, DOI, ano ou número de figura. Confira tudo no
  Crossref (`curl -A "mailto:..." https://api.crossref.org/works/DOI`).
  O `urllib` do Python falhou por certificado SSL; use `curl`.
- **Número de figura**: só escreva "Fig. N de ..." se conferiu no artigo.
  Sem isso, "adaptado de um esquema de ...".
- **Licença para reusar figura**: o Crossref traz o campo `license`; confirme
  no Unpaywall (`api.unpaywall.org/v2/DOI?email=...`, com um e-mail genérico)
  se a **versão publicada** é CC BY. Lições:
  - *Trends* (Elsevier) quase nunca é CC BY, mesmo quando é gratuito para ler
    ("bronze" não dá direito de reuso);
  - na *MEE*, *Ecology & Evolution*, *PLoS ONE*, *Sci. Rep.*, *Ecol. Lett.*
    (alguns), *Microbiome* e *Ecography* (alguns) há CC BY;
  - preprint CC BY não autoriza a figura da versão publicada;
  - CC BY-NC serve para curso sem fins lucrativos; CC BY-NC-ND só sem
    modificar a figura.
- Imagens do Wikimedia Commons: confira `LicenseShortName` pela API, com
  intervalos de alguns segundos entre as chamadas (há limite de taxa).
- O ScienceDirect bloqueia acesso automatizado: use os DOIs dos capítulos.

### 3.6 Memes

- Mantidos "no mesmo espírito" do original, com pinturas em domínio público
  e legenda própria. Fotos de pessoas, montagens e GIFs saem.
- **Prefira pintura brasileira** (pedido do Diogo). No Brasil a obra entra em
  domínio público 70 anos após a morte do autor: Portinari (2033), Anita
  Malfatti (2035), Tarsila (2044) e Di Cavalcanti (2047) ainda não podem.
  Livres: Almeida Júnior, Belmiro de Almeida, Rodolfo Amoedo, Henrique
  Bernardelli, Pedro Américo, Eliseu Visconti, os Timótheo da Costa e, dos
  modernistas, Ismael Nery.
- **Nada de símbolos patrióticos ou militares dos EUA** (nem de outras
  potências): a disciplina é dada num país que sofreu um golpe apoiado pelos
  EUA em 1964. *Washington cruzando o Delaware* foi um erro meu.
- A pintura favorita do Diogo é *A Liberdade guiando o povo*.

### 3.7 Renderizar e conferir

- `freeze: auto` com `_freeze/` versionado; o CI só tem Quarto e knitr.
  **Mudou uma função em `R/`? Renderize o `.qmd` explicitamente**: o freeze
  não percebe. Acrescentou ou removeu chunk? Apague `_freeze/slides/aNN`
  antes.
- Widgets (`plotly`) deixam bibliotecas em `_freeze/site_libs/`: versione
  essas pastas também, ou o CI não monta a página.
- **Teste de transbordamento** no navegador embutido, viewport 1250 × 750,
  em unidades de slide:
  `(rect.bottom − topo de .reveal .slides) / Reveal.getScale() > 745`,
  com todos os fragmentos visíveis. Recarregue as imagens antes
  (`img.src += '?t=' + Date.now()`): o navegador guarda os PNGs antigos.
- Depois de cada mudança: medir o deck inteiro, olhar os slides novos em
  captura de tela, só então commitar.

### 3.8 Publicação

- Commit com mensagem em português e o crédito ao Claude; `git push origin
  HEAD:main` (fast-forward) e na branch da sessão; `gh run watch` no deploy;
  `curl` da página publicada para confirmar que a mudança está no ar.
- Branches de outras sessões: atualize com a `main`, gere o `_freeze` no Mac,
  confira, envie para a branch do PR e só faça o merge com a aprovação do
  Diogo.

---

## 4. O que deu certo

- **Aula-modelo primeiro**: a Aula 4, revisada duas vezes, definiu o padrão.
- **Figuras em R a partir de dados reais dos pacotes** (Doubs, ácaros,
  dunas, aranhas) e de simulações quando o conceito pede; recalcular os
  exemplos do Legendre achou erros nos originais.
- **Montagem em camadas** (triplot, biplot, dbMEM, partição de variância,
  poça): o slide fica fiel ao original e mais didático.
- **Esquemas com fórmula anotada** (`\underbrace` + cor) no lugar de caixa
  de texto.
- **O CLAUDE.md como memória do projeto**: regras do Diogo, situação das
  aulas, armadilhas, memes já usados, pacotes aprovados. Atualize a cada
  regra nova.
- **Sessões remotas em paralelo** para a parte prática (um PR por aula) e o
  visual do site, enquanto a sessão principal convertia slides.
- **Conferir o site no ar** depois de cada deploy, inclusive os links
  internos (59 links testados com `fetch` no fim).

---

## 5. Erros, e como evitá-los

### De conteúdo (os mais caros)

| Erro | Consequência | Como evitar |
|---|---|---|
| Resumir a Aula 1 (104 slides → 45) | Duas revisões completas para restaurar | Tabela slide a slide antes de escrever; conferir o deck contra ela |
| Juntar sequências de esquemas num slide de texto (triplot, passo 2 da RDA, MEMs, partição de variância, Procrustes) | O Diogo pediu cada uma de volta | Regra 1 da seção 1 |
| Trocar esquemas por texto ("faltou a figura do k-means", "a cofenética usava esquemas") | Retrabalho | Se o original tem desenho, o deck tem figura |
| Conteúdo que já existia num curso irmão reescrito do zero | O Diogo apontou que era a Aula 2 da Bioestatística | Procurar nos cursos irmãos antes |
| Escolher uma pintura com carga política ruim | Constrangimento | Seção 3.6 |
| Usar imagem sem conferir a licença (o Diogo achava que a do Understanding Race era CC0; a página diz "All Rights Reserved") | Teria de sair | Conferir sempre na fonte |
| Atribuir uma gravura a um artigo sem confirmação (Tiedemann 1836) | Atribuição falsa evitada a tempo | Creditar "fonte original não identificada" |
| Linkar o mapa mental errado (o Diogo mandou o de métodos comparativos) | Retrabalho pequeno | Confirmar o título do recurso antes de linkar |

### Técnicos

- Regex com `DOTALL` engoliu um arquivo inteiro; uma edição em Python
  substituiu o resto de um arquivo R e apagou duas funções. Recuperado com
  `git show HEAD:arquivo`. **Edições grandes: script salvo num arquivo, com
  `assert` antes de cada substituição.**
- `vegan` 2.7: `adonis()` e `summary()` do `decorana` são *defunct*; use
  `adonis2()`. `Shepard()` é do `MASS`.
- `ordiR2step()` reavalia a chamada `rda()`: o arquivo precisa de
  `library(vegan)`.
- `ggplot2`: `label.size` em `geom_label` virou `linewidth`.
- `ade4::doubs`: a declividade (`slo`) já é ln(x + 1), e `oxy`/`bdo` vêm
  × 10. A LDA tirava o log duas vezes (corrigido no PR #3, com a altitude,
  como em Borcard et al. 2018).
- `gllvm`: com preditores, a segunda variável latente pode ficar com
  variância ~0 e o ajuste avisa de não convergência. Use `n.init` alto e
  menos variáveis latentes; leia o aviso.
- Rótulos sobrepostos em quase toda figura nova: teste em PNG antes.
- O teste de transbordamento precisa dos fragmentos visíveis e da escala do
  reveal; a figura de dois painéis com títulos longos se sobrepôs no meio.

### Erros dos originais (exemplos do que procurar)

- "Escalar = número inteiro"; "−1 < r > 1"; vetor à esquerda da matriz.
- "CCorA = CVA" (o Diogo pediu para manter, como sinônimo usado no
  `geomorph`).
- Título "Problemas de abordagens baseadas em modelos" com os problemas dos
  métodos baseados em **distância**.
- Seções de livro citadas pela numeração de outra edição (Ruxton &
  Colegrave, "11.2.1").

---

## 6. Para Métodos Filogenéticos Comparativos

O Diogo quer uma reformulação **mais profunda**, não só uma conversão.
Sugestões a partir desta experiência:

1. **Comece pelo desenho pedagógico, antes dos slides.** Objetivos por aula,
   um fio condutor visual (o equivalente ao mapa dos métodos: por exemplo,
   uma árvore de decisão "que pergunta sobre a evolução dos atributos eu
   tenho?" → sinal filogenético, correlação evolutiva (PGLS, contrastes),
   modelos de evolução (BM, OU, multi-regime), estado ancestral,
   diversificação, atributos multivariados) e a ordem das aulas. Mostre ao
   Diogo e só então inventarie.
2. **O mapa mental do Coggle** da disciplina
   (<https://coggle.it/diagram/WhbkkxE2BAAB0R0m>) é o ponto de partida desse
   fio condutor.
3. **Inventário slide a slide desde o início** (seção 3.2), com a coluna
   "o que entra de novo" para a reformulação.
4. **Dados de pacotes** para as figuras e a prática. Conferir o que existe
   antes de planejar: por exemplo, `phytools` (`anoletree`, `mammal.tree` /
   `mammal.data`), `geiger` (`geospiza`), `caper` (`shorebird`), `ape`
   (`bird.orders`). Simulações com `ape::rTraitCont`, `phytools::fastBM`,
   `geiger::sim.char`.
5. **Esquemas próprios em R** para o que costuma vir de figura de livro:
   árvores com `ape`/`ggtree` (`ggtree` é Bioconductor: perguntar antes),
   o movimento browniano no tempo, contrastes independentes passo a passo,
   o OU puxando para o ótimo, a matriz de covariância filogenética **C**
   montada a partir da árvore.
6. **Armadilhas prováveis**: árvore não ultramétrica ou sem comprimentos de
   ramo; nomes das pontas que não batem com os dados; λ e K com poucas
   espécies; OU mal identificado (α e σ² correlacionados); `nlme::gls` com
   `corBrownian`/`corPagel` e a ordem das linhas.
7. Reaproveite: a Aula 6 desta disciplina (Procrustes, PROTEST,
   `betadisper`) e o slide de PCA desta disciplina para a aula de atributos
   multivariados; a Aula 2 da Bioestatística para pergunta e hipótese.
8. Mantenha: memes brasileiros em domínio público, figuras CC BY dos
   artigos com licença conferida, publicação direta na `main` depois da
   aula-modelo, e o CLAUDE.md como memória.

### Prompt sugerido para a nova sessão

> Leia `~/GitHub/multidimensionais/WORKFLOW-conversao-qmd.md` e
> `~/GitHub/bioestatistica/WORKFLOW-conversao-qmd.md`. Quero reformular a
> disciplina de Métodos Filogenéticos Comparativos (repo `...`, pptx em
> `...`), não só converter. Antes de qualquer slide: (1) proponha o desenho
> pedagógico e o fio condutor visual, partindo do mapa mental do Coggle;
> (2) faça o inventário slide a slide de uma aula-modelo, com o que entra de
> novo; (3) escreva o `PLANO-conversao-qmd.md` e o `CLAUDE.md`. Me mostre
> antes de escrever o deck da aula-modelo.
