#Chi quadro e v di cramer
vcramer ==
  
  
v=vcramer(hero$Gender, hero$alignment)
round(v,2)

#eta quadro
#per calcolare l'Eta quadro in R è possibile utilizzare
#o la funzione eta ne pacchetto labstatR
#legato al testo di Iacus e Massarotto

library(labstatR)
eta(x,y)

eta2 <- function(x,y){my <- by(y,x,mean)
ny <- by(y,x,lenght)
}

#correlazione di Pearson o correlazione lineare


##Correlation Matrix of Multivariate sample:
(c1 <- cor(longley))
##Graphics

#Rappresentazione grafica di una matrice di correlazione = heatmap
set.seed(123)
data <- matrix(rnorm(100,))

#heatmap con ggplot
library("ggplot")
library("reshape")
data_melt <- melt(data)   #riordina i dati
head(data_melt)

ggp <- ggplot(data_melt, aes(x1, x2)) +
  geom

#studio grafico della correlazione = scatterplot
data("mtcars")
mtcars$cyl <- as.factor()