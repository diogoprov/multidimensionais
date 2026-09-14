# Análises Multidimensionais

Materiais da disciplina de pós-graduação **Análises Multidimensionais**
(201521415, 45 h) do [Mestrado em Biologia Animal](https://ppgba.ufms.br/)
da Universidade Federal de Mato Grosso do Sul.

**Site publicado: <https://provetelab.org/multidimensionais/>**

## O que há aqui

Sete aulas, cada uma em sua própria pasta, contendo a página da aula
(`index.qmd`), os scripts em R usados em sala, os conjuntos de dados e os
*walkthroughs* — versões HTML comentadas do código, para o aluno acompanhar
linha a linha.

| Aula | Tópico |
|-----:|--------|
| 1 | Introdução à estatística multivariada |
| 2 | Coeficientes de associação |
| 3 | Agrupamento: cluster hierárquico, k-means e IndVal |
| 4 | Ordenações irrestritas: PCA, PCoA, nMDS e CA |
| 5 | Ordenações restritas: CCA, RDA, pRDA, db-RDA, LDA |
| 6 | Regressão de matrizes, Mantel, PROCRUSTES e (PER)MANOVA |
| 7 | Métodos multidimensionais baseados em modelos |

Os slides em PDF ficam em `slides/`.

## Como o site é construído

Site [Quarto](https://quarto.org) publicado no GitHub Pages por GitHub
Actions (`.github/workflows/publish.yml`). **A pasta `docs/` não é
versionada** — ela é gerada a cada push. Para publicar uma alteração, basta
editar o `.qmd`, commitar e dar push.

Para trabalhar localmente:

```bash
quarto preview      # pré-visualiza com recarga automática
quarto render       # gera docs/
```

O código R das aulas é exibido mas **não executado** durante o render (todos
os walkthroughs trazem `eval: false`), então não é preciso ter os pacotes
instalados para construir o site.

## Origem

Estes materiais viviam em `teaching/multidimensionais/` no repositório do
site do laboratório ([diogoprov.github.io](https://github.com/diogoprov/diogoprov.github.io)),
de onde foram extraídos com o histórico preservado. O histórico anterior à
migração continua disponível lá.

---

[Biodiversity Synthesis Lab](https://provetelab.org/) · Diogo B. Provete ·
Instituto de Biociências, UFMS
