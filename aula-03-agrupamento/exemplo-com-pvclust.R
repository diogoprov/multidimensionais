#Exemplo da análise com o pvclust
library(vegan)
library(pvclust)

exemplo <- read.table("bocaina.txt", h=TRUE)#Importem o conjunto de dados Bocaina.txt

bocaina <- t(exemplo)#Se quisermos agrupar as espécies, temos de transpôr a matriz
boc.norm <- decostand(bocaina, "normalize")

analise <- pvclust(boc.norm, method.hclust="average", method.dist="euc") 
plot(analise, hang=-1)
pvrect(analise)
distBocaina <- vegdist(bocaina, method="horn")#produz uma matriz de similaridade com o coeficiente de Morisita-Horn
dendro <- hclust(distBocaina, method="average")#produz um agrupamento com a função hclust e o método UPGMA
cophenetic(analise)->cofresult
cor(cofresult, distBocaina)
