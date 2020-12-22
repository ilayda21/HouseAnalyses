library(CORElearn)
library(tibble)
library(tidyr)

load("house.data")

data <- antalya_train

dataFrame <- data.frame(data)
dataDimension <- dim(data)

dataFrame[dataFrame==""]<-NA

# Id is removed because for each row is unique
dataFrame$Id <- NULL

# Type of each column
print("Column Types: ")
columnTypes <- sapply(dataFrame, class)
print(columnTypes)

# number of NA for each column
print("Number of Nas for each column")
naTable <- colSums(is.na(dataFrame))
print(naTable)

# by using attr eval, we are going to calculate the correlation between target value and all others
# because we want to eliminate some unrelated columns (attrEval -> CORElearn)
# attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")

dataFrame$MustakilMi <- factor(dataFrame$MustakilMi, labels = c("no", "yes"))

# odasayisi+salonsayisi (factor)
dataFrame$OdaBilgisi <- NA

for (i in 1:dataDimension[1]) {
  dataFrame$OdaBilgisi[i] <- paste(dataFrame$OdaSayisi[i], "+", dataFrame$SalonSayisi[i])
}

# attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")

# manzara => make it factor (Doga, Sehir, Deniz, Bogaz)
dataFrame$Manzara <- NA
for (i in 1:dataDimension[1]) {
  viewValue <- ""
  if (dataFrame$ManzaraGol[i] == 1) {
    viewValue = paste0(viewValue, "Gol")
  } 
  
  if(dataFrame$ManzaraDeniz[i] == 1) {
    viewValue = paste0(viewValue, "Deniz")
  }
  
  if (dataFrame$ManzaraBogaz[i] == 1) {
    viewValue = paste0(viewValue, "Bogaz")
  }
  
  if (dataFrame$ManzaraSehir[i] == 1) {
    viewValue = paste0(viewValue, "Sehir")
  }  
  
  if (dataFrame$ManzaraDoga[i] == 1) {
    viewValue = paste0(viewValue, "Doga")
  }
  
  if (viewValue == "") {
    viewValue <- "ManzaraYok"
  } 
  
  dataFrame$Manzara[i] <- viewValue
}

# dataFrame
# attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")

# cephe => Bati Dogu Guney Kuzey
dataFrame$Cephe <- NA
for (i in 1:dataDimension[1]) {
  frontValue <- ""
  if (dataFrame$Kuzey[i] == 1) {
    frontValue = paste0(viewValue, "Kuzey")
  } 
  
  if(dataFrame$Guney[i] == 1) {
    frontValue = paste0(viewValue, "Guney")
  }
  
  if (dataFrame$Dogu[i] == 1) {
    frontValue = paste0(viewValue, "Dogu")
  }
  
  if (dataFrame$Bati[i] == 1) {
    frontValue = paste0(viewValue, "Bati")
  }  
  
  dataFrame$Cephe[i] <- frontValue
}

tibbledData = as_tibble(dataFrame)
