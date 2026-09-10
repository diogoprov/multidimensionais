

install.packages(c("vegan","labdsv"))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------



#### Aula 1 - Cluster analysis


# 1.1 - Load required libraries 
library (vegan) 
library(labdsv)



# 1.2 - Import the data from CSV files
spe <- read.csv("DoubsSpe.csv", row.names=1)# nomeei a planilha e abro no R (tudo o que est? na direita ser? realizado)
env <- read.csv("DoubsEnv.csv", row.names=1)
spe <- spe[-8,] # Remove empty site 8 # retiro a linha 8, pois n?o h? dados
env <- env[-8,] # Remove empty site 8


# 1.3 - Single linkage agglomerative clustering
library(vegan)
spe.norm <- decostand(spe, "normalize") # fun??o do pacore vegan, ela faz v?rias padroniza??es, aqui ele normaliza  
spe.ch <- vegdist(spe.norm, "euc") # criamos uma matriz de dist?ncia, se n?o determinamos o tipo de matris ele far? o bray curtis
spe.ch.single <- hclust(spe.ch, method="single") # an?lise de cluster, aqui escolhemos qual m?todo de liga??o queremos 
plot(spe.ch.single)


# 1.4 - Complete-linkage agglomerative clustering
spe.ch.complete <- hclust(spe.ch, method="complete")
plot(spe.ch.complete)


# 1.5 - UPGMA agglomerative clustering
#UPGMA = Unweighted Pair-Group Method using arithmetic Averages
spe.ch.UPGMA <- hclust(spe.ch, method="average")
plot(spe.ch.UPGMA)


# 1.6 - Ward's minimum variance clustering
spe.ch.ward <- hclust(spe.ch, method="ward")
plot(spe.ch.ward)


# 1.7 - Cophenetic correlation
spe.ch.single.coph <- cophenetic(spe.ch.single) # fun??o para a matriz cofen?tica
cor(spe.ch, spe.ch.single.coph) # correla??o entre os conjuntos de dados
spe.ch.comp.coph <- cophenetic(spe.ch.complete)
cor(spe.ch, spe.ch.comp.coph)
spe.ch.UPGMA.coph <- cophenetic(spe.ch.UPGMA)
cor(spe.ch, spe.ch.UPGMA.coph)
spe.ch.ward.coph <- cophenetic(spe.ch.ward)
cor(spe.ch, spe.ch.ward.coph)


# 1.8 - Shepard-like diagrams
#Para real?ar a rela??o entre uma matriz de dist?ncia e uma matriz cofen?tica, podemos
#utilizar o diagrama de Shepard plotando as dist?ncias originais com as cofen?ticas.
plot(spe.ch, spe.ch.UPGMA.coph, xlab="Chord distance", 
	ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
  main=c("UPGMA", paste("Cophenetic correlation ",
  round(cor(spe.ch, spe.ch.UPGMA.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.UPGMA.coph), col="red")


# 1.9 - Gower (1983) distance
#O m?todo de agrupamento que produzir a mais baixa dist?ncia de Gower pode ser 
#visto como aquele que gera o melhor modelo de agrupamento da matriz de dist?ncia.
gow.dist.single <- sum((spe.ch-spe.ch.single.coph)^2)
gow.dist.comp <- sum((spe.ch-spe.ch.comp.coph)^2)
gow.dist.UPGMA <- sum((spe.ch-spe.ch.UPGMA.coph)^2)
gow.dist.ward <- sum((spe.ch-spe.ch.ward.coph)^2)
gow.dist.single
gow.dist.comp
gow.dist.UPGMA
gow.dist.ward

# 1.8 - Shepard-like diagrams
#Para real?ar a rela??o entre uma matriz de dist?ncia e uma matriz cofen?tica, podemos
#utilizar o diagrama de Shepard plotando as dist?ncias originais com as cofen?ticas.
plot(spe.ch, spe.ch.single.coph, xlab="Chord distance", 
	ylab="Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
  main=c("single", paste("Cophenetic correlation ",
  round(cor(spe.ch, spe.ch.single.coph),3))))
abline(0,1)
lines(lowess(spe.ch, spe.ch.single.coph), col="red")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------


#### Aula 2 - k-means partitioning


# 2.1 - Pre-transformed species data
spe.kmeans <- kmeans(spe.norm, centers=4, nstart=100) # aqui ? o local da linha de comando que eu digo quantos grupos eu quero (centers=4)
spe.kmeans


# 2.2 - k-means partitioning, 2 to 10 groups
#criterion="ssi": A melhor parti??o ? indicada pelo mais elevado valor de ssi.
spe.KM.cascade <- cascadeKM(spe.norm, inf.gr=2, sup.gr=10, iter=100, criterion="ssi") #quanto maior o valor de ssi melhor, da? eu escolho o n?mero de parti??es de acordo com o maior n?mero de ssi
plot(spe.KM.cascade, sortg=TRUE)


# 2.3 - summary
summary(spe.KM.cascade)
spe.KM.cascade$results
spe.KM.cascade$partition
#SSE: crit?rio utilizado pelo algor?timo para achar o agrupamento ?timo dos objetos.
#calinski or ssi: s?o bons crit?rios para encontrar o n?mero ideal de grupos (?cascadeKM)

#If the geographic coordinates of the objects are available, they can be used
#to plot a map of the objects, with symbols or colours representing the groups 
#specified by one of the columns of this table

write.table(spe.KM.cascade$partition[,3],"teste.csv") # salvo um arquivo na pasta (diret?rio) em excell

#------------------------------------------------------------------------------------------------------------------------------------------------------------------


#### Aula 3 - Species indicator values (Dufrene and Legendre)


#3.1 - Divide the sites into 4 groups depending on the distance to the source of the river
das.D1 <- dist(data.frame(das=env[,1], row.names=rownames(env)))
dasD1.kmeans <- kmeans(das.D1, centers=4, nstart=100)
dasD1.kmeans$cluster  # antes da an?lise dividimos as ?reas em 4 grupos de acordo com as caracter?sticas ambientais


#3.2 - Indicator species for this typology of the sites
iva <- indval(spe, dasD1.kmeans$cluster) # indval (dados de esp?cie, vetor de agrupamento)
iva


#3.3 - Table of the significant indicator species
gr <- iva$maxcls[iva$pval<=0.05] # maxcls (para obtermos o grupo)
iv <- iva$indcls[iva$pval<=0.05] # valor de indica??o
pv <- iva$pval[iva$pval<=0.05]   # valor de p
fr <- apply(spe>0, 2, sum)[iva$pval<=0.05]
fidg <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr) # fun??o para criar a planilha
fidg <- fidg[order(fidg$group, -fidg$indval),]
fidg
#Export the result to a CSV file (to be opened in a spreadsheet)
write.csv(fidg, "IndVal-das.csv") #para criar o arquivo excell na minha pasta - diret?rio



#Exerc?cio

va<-read.table("ambiente.txt") #abri o arquivo .txt
#cluster analysis
# 1.3 - Single linkage agglomerative clustering
library(vegan)

# 1.4 - Complete-linkage agglomerative clustering

# 1.5 - UPGMA agglomerative clustering
#UPGMA = Unweighted Pair-Group Method using arithmetic Averages


# 1.6 - Ward's minimum variance clustering

# 1.7 - Cophenetic correlation


# 1.8 - Shepard-like diagrams
#Para real?ar a rela??o entre uma matriz de dist?ncia e uma matriz cofen?tica, podemos
#utilizar o diagrama de Shepard plotando as dist?ncias originais com as cofen?ticas.


# 1.9 - Gower (1983) distance
#O m?todo de agrupamento que produzir a mais baixa dist?ncia de Gower pode ser 
#visto como aquele que gera o melhor modelo de agrupamento da matriz de dist?ncia.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

