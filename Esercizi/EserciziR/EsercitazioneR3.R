str(heart)
heart$sex <- as.factor(hear$sex)

#Grafico sul sesso
plot(heart$sex)
plot(heart$age, types="l")

t<-table(heart$target, heart$sex)
round(prop.table(t) *100.2)  #frequenze relative

cor(dataset, method = )

#leggere documentazione as.factor
#feature numeriche = istogramma 