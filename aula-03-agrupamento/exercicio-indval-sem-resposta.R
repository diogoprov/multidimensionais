library(labdsv)
gir.bocaina <- read.table("exemploIndVal.txt", h=T)#importem os dados <bocaina.txt>
?indval
fitofis <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4))#cria um vetor que informa os grupos de fitofisionomias para os quais deseja-se encontrar espécies indicadoras

#note que nesta matriz, as especies estão nas linhas e não nas colunas, 
#portanto você terá que transpor a matriz, veja a função t()