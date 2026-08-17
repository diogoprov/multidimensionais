# Tue May  7 20:08:24 2019 ------------------------------

#---Aula quarta-feira, ordenações irrestritas

#4.1 - Load required libraries and Preparation of the Data
library(vegan)
library(ape)

#### PCA

#Import the data from CSV files
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)
#Remove empty site 8
spe <- spe[-8,]
env <- env[-8,]

#4.2 - PCA on the full dataset (correlation matrix: scale=TRUE)
env.pca <- rda(env, scale=TRUE) #scale=TRUE padroniza as vari?veis #rda faz a pca
env.pca

summary(env.pca) #Default scaling 2. Utilizado quando o principal interesse est? na rela??o entre os descritores.
#Scaling=2: Os ?ngulos entre os descritores no biplot refletem suas correla??es. As dist?ncias entre
#os objetos no biplot n?o s?o aproxima??es de suas dist?ncias euclidianas no espa?o multidimensional.
summary(env.pca)$species
summary(env.pca)$sites

summary(env.pca, scaling=1)  #Utilizado quando o principal interesse est? na rela??o entre os objetos.
#Scaling=1: Os ?ngulos entre os vetores dos descritores n?o possuem significado. As dist?ncias entre os objetos
#no biplot s?o aproxima??es de suas dist?ncias euclidianas no espa?o multidimensional.
summary(env.pca, scaling=1)$species
summary(env.pca, scaling=1)$sites

#OBS: os autovalores s?o os mesmos em ambos e a proje??o de um objeto em ?ngulo reto sobre um descritor aproxima
#a posi??o daquele objeto ao longo daquele descritor.

#4.3 - Crit?rios de escolha dos eixos
(ev <- env.pca$CA$eig) #Eigenvalues (preciso calcular p/ despois comparar com o broken stik, da? eu escolho os valores que s?o maiores do q o BS)

#4.3.1 - Apply Kaiser-Guttman criterion to select axes
ev[ev > mean(ev)] # crit?rio de escolha maior do que 1

#4.3.2 - Broken stick model
n <- length(ev) # vejo quantas vari?veis eu tenho na minha matriz de dados ambientais
(bs.ev<-bstick (n,n)) # gero o modelo de broken stik para 11 valores
(ev <- env.pca$CA$eig) #Eigenvalues

#4.4 - Plot eigenvalues and Kaiser-Guttman/Broken stick model
#windows(title="PCA eigenvalues")
par(mfrow=c(2,1))
barplot(ev, main="Eigenvalues - Kaiser-Guttman criterion", col="bisque", las=2)
abline(h=mean(ev), col="red")	#average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

barplot(t(cbind(ev,bs.ev)), beside=TRUE, main="Eigenvalues - Broken stick model", col=c("bisque",2), las=2)
legend("topright", c("eigenvalue", "Broken stick model"), pch=15, col=c("bisque",2), bty="n")

#4.5 - Two PCA biplots: scaling 1 and scaling 2
#windows(title="PCA biplots - environment - biplot.rda", 12, 6) #dois primeiros comandos crio uma estrutura p/ colocar dois gr?ficos ao mesmo tempo
par(mfrow=c(1,2))
biplot(env.pca, scaling=1, main="PCA - scaling 1")
biplot(env.pca, main="PCA - scaling 2")  # Default scaling = 2

#4.6 - Loadings (Coeficientes de estrutura)
cor(env,(summary(env.pca)$sites)) # correla??o de uma vari?vel com outra matriz original vs escores d

#### PCoA # posso usar qquer matriz de dist?ncia

#5.1 - An?lise e autovalores
spe.bray <- vegdist(spe)
spe.b.pcoa <- pcoa(spe.bray, correction = "lingoes") 
spe.b.pcoa
spe.b.pcoa$vectors #obter todos os eixos
spe.b.pcoa$vectors[,1:2] #obter os eixos 1 e 2
spe.b.pcoa$values #Eigenvalues #autovalores observados
par(mfrow=c(1,1))
biplot(spe.b.pcoa,spe)

#cmdscale é uma outra função para PCoA do pacote stats que já vem embutido no R (matriz de dist?ncia, K=numero de eixos max (numero de linhas-1)

spe.b.pcoa_cen <- cmdscale(spe.bray, k=(nrow(spe)-1), eig=TRUE) #cmdscale fun??o para PCoA (matriz de dist?ncia, K=numero de eixos max (numero de linhas-1)
spe.b.pcoa_cen
spe.b.pcoa_cen$points #obter todos os eixos
spe.b.pcoa_cen$points[,1:2] #obter os eixos 1 e 2
spe.b.pcoa_cen$eig #Eigenvalues #autovalores observados

(bs.ev<-bstick (17,17)) #ele adicionou apenas os valores positivos, tem que confirmar se est? certo

#Plot of the sites and weighted average projection of species
ordiplot(scores(spe.b.pcoa_cen)[,c(1,2)], type="t", main="PCoA with species")
abline(h=0, lty=3)
abline(v=0, lty=3)
#Add species
spe.wa <- wascores(spe.b.pcoa_cen$points[,1:2], spe)
text(spe.wa, rownames(spe.wa), cex=0.7, col="red")

#8.2 - Compute CA
spe.ca <- cca(spe)    #in?rcia, total explicado (soma dos autovalores) CA1 at? CA8 s?o os eixos 
spe.ca

#default scaling 2 - Esta configura??o ? mais apropriada caso haja interesse na ordena??o das esp?cies.
#No espa?o multidimensional, a dist?ncia de X2 ? preservada entre as esp?cies. Uma esp?cie pr?xima de um objeto provavelmente ter? elevada ocorr?ncia naquele objeto. Ver Sp BLA e a ua16.

summary(spe.ca) #default scaling 2
summary(spe.ca)$species #separo os scores das esp?cies
summary(spe.ca)$sites #separo os scores dos locais

#scaling 1 - Esta configura??o ? mais apropriada caso haja interesse na ordena??o dos objetos (locais).
#No espa?o multidimensional, a dist?ncia de X2 ? preservada entre os objetos. Um objeto pr?ximo de uma
#esp?cie provavelmente ter? alta contribui??o dessa esp?cie. Ver ua1 e Sp TRU.
summary(spe.ca, scaling=1)
summary(spe.ca, scaling=1)$species
summary(spe.ca, scaling=1)$sites

#OBS: Note que os autovalores s?o os mesmos em ambos. O scaling afeta apenas os autovetores.

#8.3 - CA biplots
#windows(title="CA biplots", 14, 7)
par(mfrow=c(1,2))
#Scaling 1: sites are centroids of species
plot(spe.ca, scaling=1, main="CA fish abundances - biplot scaling 1")
#Scaling 2 (default): species are centroids of sites
plot(spe.ca, main="CA fish abundances - biplot scaling 2")

#### DCA   

#11.1 - Dados e an?lise
spe
spe.dca<-decorana(spe)
spe.dca
summary(spe.dca) #Utilizar Decorana values
plot(spe.dca)
plot(spe.dca, display=c("sites"), type = "text")

#11.2 - Scores 
dca.scor.sites<-scores(spe.dca, display=c("sites"))
dca.scor.sp<-scores(spe.dca, display=c("species"))
dca.scor.sites
dca.scor.sp

#### NMDS

#9.1 - Fish species - Bray-Curtis distance matrix
spe.nmds <- metaMDS(spe, distance="bray",k=2)
spe.nmds
spe.nmds$stress
spe.nmds$points
spe.nmds$species #Calculadas por meio da fun??o wascores {vegan}. Computes Weighted Averages scores
#of species for ordination configuration or for environmental variables

#9.2 - Gr?fico 
plot(spe.nmds, type="t", main=paste("NMDS/Bray - Stress =",	round(spe.nmds$stress,3)))

#9.3 - Shepard plot
stressplot(spe.nmds, main="Shepard plot")
