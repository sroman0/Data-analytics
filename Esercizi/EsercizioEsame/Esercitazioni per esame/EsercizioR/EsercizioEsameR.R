#Leggiamo il dataset e ne vediamo le caratteristihce

file_path <- "C:\\Users\\simon\\Desktop\\Università\\Esami terzo anno\\Data Analytics\\Esercizi\\EsercizioEsame\\Esercitazioni per esame\\EsercizioR\\heart.csv"
data <- read.csv(file_path)

#Guardiamo le info generali del dataset

head(data)
summary(data)
str(data)

#Creo istogramma per ogni variabile 

library(ggplot2)

variabili <- c("age", "sex", "chest.pain.type","resting.bp.s","cholesterol","fasting.blood.sugar","resting.ecg","max.heart.rate","exercise.angina","oldpeak","ST.slope","target")

for (variabile in variabili) {
  p<- ggplot(data = data, aes(x = get(variabile))) +
    geom_histogram() +
    labs(x = variabile, y = "Frequenza", title = paste("Istogramma di", variabile)) +
    theme_minimal()
  
  print(p)
}

#Creo le tabelle di contingenza delle variabili

i=0

for (variabile in variabili) {
  i=i+1
  tab1 <- table (data[,i], data$target)
  tab2 <- prop.table(tab1)
  round(tab2, 2)
  round((tab2*100),2)
  message("\n\n", variabile)
  print(tab2)
}

#Calcolo V di Cramer 

library(vcd)

vcramer <- function (x, y = NULL) 
{
  if(!is.null(y)) {
    tab <- table(x, y)
  } else tab = as.matrix(x)
  
  n <- (min(nrow(tab), ncol(tab))-1) * margin.table(tab)
  chiq <- as.numeric(chisq.test(tab, correct = FALSE)$statistic)
  p <- chisq.test(tab, correct = FALSE)$p.value
  v = sqrt(chiq / n)
  res <- c("chi.sq" = chiq, "p" = p, "v di Cramer" = v)
  res
}

vcramer_multiple <- function(data, target, variabili) {
  results <- list()
  
  for (variabile in variabili) {
    result <- vcramer(data[[variabile]], target)
    results[[variabile]] <- result
  }
  
  return(results)
}

results <- vcramer_multiple(data, data$target, variabili)
for (variabile in names(results)) {
  message(paste("Risultati per", variabile, ":"))
  print(results[[variabile]])
}

#Prima Matrice di correlazione

cor_matrix <- cor(data)
colnames(cor_matrix) <- colnames(data)
rownames(cor_matrix) <- colnames(data)

set.seed(123)
dataf <- matrix(rnorm(144, 0, 10), nrow = 12, ncol = 12)
colnames(dataf) <- colnames(data)
rownames(dataf) <- colnames(data)
heatmap(dataf, labRow = rownames(dataf), labCol = colnames(dataf))


#Seconda matrice di correlazione

library(ggplot2)

# Dati di esempio
set.seed(123)
dataf <- matrix(rnorm(100, 0, 10), nrow = 11, ncol = 11)
colnames(dataf) <- paste0("col", 1:11)

# Calcola la matrice di correlazione
cor_matrix <- cor(dataf)

# Trasforma la matrice di correlazione in un data frame
cor_df <- as.data.frame(cor_matrix)
cor_df$var1 <- rownames(cor_df)
cor_df <- tidyr::gather(cor_df, var2, value, -var1)

# Crea la heatmap di correlazione con ggplot2
n <- ggplot(cor_df, aes(var1, var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") +
  scale_y_discrete(labels = colnames(data)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_discrete(labels = colnames(data)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs( title = "Heatmap di Correlazione")

print(n)


