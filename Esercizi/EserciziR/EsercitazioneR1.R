#Creiamo vettore età, peso e altezza
Mamma = c(19, 22, 21, 23, 22, 20)
Papà = c(50, 75, 80, 56, 75, 58)
Sorella = c(1.6, 1.78, 1.91, 1.72, 1.81, 1.68)

ALT.CM = Sort * 100
ALT.CM

#Calcoliamo il BMI 

#Vettori con dati categorici
SESSO = c("F", "M", "M", "F", "M", "F")
SESSO

SESSO = factor(SESSO)
SESSO

PROV = c("RM", "RM", "TO", "NA", "TO", "NA")
PROV = factor(PROV)

#Seleziona il peso della prima unità
Patt[1]

#Selezioniamo 3o e 6o elemento
Patt[c]



#MATRICI
#Creiamo una matrice con cbind o rbind
matr = cbind(Sort, Patt)
matr
dim(matr)



#DATAFRAME
#Creiamo un dataframe
Dati = data.frame(Mammeta, Patt, Sort, SESSO, PROV)
Dati

#Dataframe structure
str(Dati)



#LISTE
#Lista con insieme di eementi

#INDICI E RAPPRESENTAZIONE GRAFICHE
#Carichiamo i dati
data("airquality")

#Valutiamo cosa contiene il dataset
str(airquality)

#Indici di posizione e di variabilità
summary(airquality)

#Costruimamo un boxplot sulla variabile del df di interesse
boxplot(airquality$Ozone)

#Aggiungiamo dettagli al boxplot
b = boxplot(airquality$Ozone,
  main= "Media Ozono in parti per miliardi a Roosevelt Island",
  xlab = "Parti per miliardi",
  ylab = "Ozono",
  col = "orange",
  border = "brown",
  horizontal = TRUE,
  notch = TRUE)

#Prepariamo i dati
ozono <- airquality$Ozone
temp <- airquality$Temp

#Normalizziamo i dati
ozone_norm <- rnorm(200, mean=mean(ozone, na.rm=TRUE), sd=sd(ozone, na.rm = TRUE))
temp_norm <- rnorm(200, mean=mean(temp, na.rm=TRUE), sd=sd(temp, na.rm=TRUE))

#Costruiamo 4 diversi boxplot


#Boxplot stratificato
boxplot(Temp)~Month,
  
#Istogramma semplice
Temperature <- airquality$Temp
hist(Temperature)

#Istogramma con parametri aggiunti
hist(Temperature,
     main ="Temp max giornaliera all'aeroporto di La Guardia",
     xlab ="Temperatura in gradi Fahrenheit",
     )