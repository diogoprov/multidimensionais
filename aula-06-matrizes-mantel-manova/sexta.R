# Wed May  8 22:22:47 2019 ------------------------------

#---Aula sexta

#### Aula 16 - Mantel e Mantel Parcial

#16.1 - Mantel #preciso criar duas matrizes de dist?ncias que ser?o correlacionadas
library(vegan)
data(varespec)
data(varechem)
veg.dist <- vegdist(varespec) # Bray-Curtis
varstand <- decostand(varechem,method="standardize")
env.dist <- vegdist(varstand, "euclid")
mantel(veg.dist,env.dist)     #correla??o de pearson param?trico e correla??o de spearman n?o param?trico
mantel(veg.dist, env.dist, method="spear")

#16.2 - Mantel Parcial # a ?ltima matriz que entra na an?lise ser? a que eu quero controlar o efeito
dist<-read.table ("dist.txt", header=TRUE)
amb<-read.table ("amb.txt", header=TRUE)
gen<-read.table ("gen.txt", header=TRUE)
amb<-decostand(amb,"standardize")
distdist<-vegdist(dist, method="euclidean")
ambdist<-vegdist(amb, method="euclidean")
gendist<-vegdist(gen, method="euclidean")
mantel.partial(gendist, ambdist, distdist, method = "pearson", permutations = 10000)#aqui controlei o efeito geogr?fico

#--------------------------------------------------------------------------------

#### Aula 17 - Protest
clad<-read.table("clad.txt", header = TRUE)
cope<-read.table("cope.txt", header = TRUE)
dcaclad<-decorana(clad) #m?todo de ordena??o (DCA)
dcacope<-decorana(cope)
summary(dcaclad)
summary(dcacope)
scordcaclad<-scores(dcaclad, display=c("sites"), choices =1:2) #salvando os escores da DCA e salvando os dois primeiros eixos
scordcacope<-scores(dcacope, display=c("sites"), choices =1:2)
cladcope<-protest(scordcaclad, scordcacope, scores = "sites", permutations = 10000)

#### Aula 10 - BETADISPER - Multivariate homogeneity of groups dispersions 

#10.1 - Pacote e dados
varespec
dis <- vegdist(varespec) #Bray-Curtis distances between samples
groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed")) #First 16 sites grazed, remaining 8 sites ungrazed
groups

#10.2 - Calculate multivariate dispersions
mod <- betadisper(dis, groups)
mod

#10.3 - Significance and Pairwise comparisons
anova(mod) #Perform test (if the distances between group members and group centroids is the Euclidean distance)
permutest(mod, pairwise = TRUE) #Permutation test for F
(mod.HSD <- TukeyHSD(mod)) #Tukey's Honest Significant Differences

#10.4 - Plots
plot(mod) #Groups and distances to centroids on the first two PCoA axes
plot(mod, axes = c(3,1)) #Can also specify which axes to plot, ordering respected
boxplot(mod) #Draw a boxplot of the distances to centroid for each group

#Comparing species composition between field and collections with PERMANOVA
#Data from da Silva et al. 2017 SAJH What do data from fieldwork and scientific collections tell us about species richness and composition of amphibians and reptiles?

anuros_permanova <- read.csv("anuros_permanova.csv", header = T, row.names = 1)
head(anuros_permanova)
repteis_permanova <- read.csv("repteis_permanova.csv", header = T, row.names = 1)
head(repteis_permanova)

adonis(anuros_permanova[,-28] ~ anuros_permanova[,28], method = "bray")#considering abundance
adonis(anuros_permanova[,-28] ~ anuros_permanova[,28], method = "bray", binary=TRUE)#using Sørensen index for incidence

#Both tests are significant, showing that the diference in abundance of museum species did not affect our conclusions. 
