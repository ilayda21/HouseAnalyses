library(CORElearn)
library(tibble)
library(tidyr)
library(knitr)
library(dplyr)
library(arules)
library(arulesViz)
library(cluster)
library(fpc)
library(DMwR2)

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
    viewValue <- "ManazaraYok"
  } 
  
  dataFrame$Manzara[i] <- viewValue
}

# dataFrame
# attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")


dataFrame = subset(dataFrame, select = -c(OdaSayisi,SalonSayisi, ToplamOdaSayisi,
                                          ManzaraSehir, ManzaraDoga, ManzaraGol,
                                          ManzaraDeniz, ManzaraBogaz))

# --------------------------------------AGE------------------------------------

for (i in 1:dataDimension[1]) {
  
  if(is.na(dataFrame$GercekYas[i])) {
      firstFilter <- dataFrame[dataFrame$Ilce == dataFrame$Ilce[i] &
                        dataFrame$Mahalle == dataFrame$Mahalle[i] &
                        dataFrame$OdaBilgisi == dataFrame$OdaBilgisi[i],]
      
      if(dim(firstFilter)[1] == 0) {
        print("CANNOT PASS FIRST FILTER")
      } else {
        x <- firstFilter[firstFilter$BanyoSayisi == dataFrame$BanyoSayisi[i]  & 
                           firstFilter$BulunduguKat > dataFrame$BulunduguKat[i] - 3 
                         & firstFilter$BulunduguKat < dataFrame$BulunduguKat[i] + 3
                         & firstFilter$Fiyat < dataFrame$Fiyat[i] + 20000
                         & firstFilter$Fiyat > dataFrame$Fiyat[i] - 20000,]
        if (dim(firstFilter)[1] == 0) {
          print("CANNOT PASS SECOND FILTER")
        } else {
          y <- x[x$OrijinalAlan  < dataFrame$OrijinalAlan [i] + 30
                   & x$OrijinalAlan  > dataFrame$OrijinalAlan [i] - 30,]
          medianResult <- (median(y$GercekYas, na.rm=TRUE))
          if (!is.na(medianResult)) {
            dataFrame$GercekYas[i] <- medianResult
          }
        }
      }

  }
}

# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# cephe => Bati Dogu Guney Kuzey
dataFrame$Cephe <- NA
naTable <- colSums(is.na(dataFrame))
print(naTable)

for (i in 1:dataDimension[1]) {
  frontValue <- ""
  if (dataFrame$CepheKuzey[i] == 1) {
    frontValue = paste0(frontValue, "Kuzey")
  } 
  
  if(dataFrame$CepheGuney[i] == 1) {
    frontValue = paste0(frontValue, "Guney")
  }
  
  if (dataFrame$CepheDogu[i] == 1) {
    frontValue = paste0(frontValue, "Dogu")
  }
  
  if (dataFrame$CepheBati[i] == 1) {
    frontValue = paste0(frontValue, "Bati")
  }  
  
  if(frontValue == "") {
    if(dataFrame$MustakilMi[i]=="yes") {
      frontValue = "KuzeyGuneyDoguBati"
    } else {
      
      firstFilter <- dataFrame[dataFrame$Ilce == dataFrame$Ilce[i] &
                        dataFrame$Mahalle == dataFrame$Mahalle[i] &
                        dataFrame$MustakilMi[i]=="no" &
                        dataFrame$OdaBilgisi == dataFrame$OdaBilgisi[i] &
                        dataFrame$BanyoSayisi == dataFrame$BanyoSayisi[i]  & 
                        dataFrame$BulunduguKat > dataFrame$BulunduguKat[i] - 3 
                      & dataFrame$BulunduguKat < dataFrame$BulunduguKat[i] + 3
                      & dataFrame$Fiyat < dataFrame$Fiyat[i] + 20000
                      & dataFrame$Fiyat > dataFrame$Fiyat[i] - 20000,]
      if(dim(firstFilter)[1] == 0) {
        print("CANNOT FOUND")
        frontValue <- NA
      } else {
        
        medianBatiResult <- median(firstFilter$CepheBati, na.rm=TRUE)
        if (!is.na(medianBatiResult) && medianBatiResult == 1) {
          frontValue = paste0(frontValue, "Bati")
        } 
        
        medianDoguResult <- median(firstFilter$CepheDogu, na.rm=TRUE)
        if (!is.na(medianDoguResult) && medianDoguResult == 1) {
          frontValue = paste0(frontValue, "Dogu")
        } 
        
        medianGuneyResult <- median(firstFilter$CepheGuney, na.rm=TRUE)
        if (!is.na(medianGuneyResult) && medianGuneyResult == 1) {
          frontValue = paste0(frontValue, "Guney")
        } 
        
        medianKuzeyResult <- median(firstFilter$CepheKuzey, na.rm=TRUE)
        if (!is.na(medianKuzeyResult) && medianKuzeyResult == 1) {
          frontValue = paste0(frontValue, "Kuzey")
        } 
        
        if(frontValue == "") {
           frontValue <- NA
        } 
      }

    }
  }
  
  dataFrame$Cephe[i] <- frontValue
}

naTable <- colSums(is.na(dataFrame))
print(naTable)

# ---------------------------------------------------------------------------

dataFrame = subset(dataFrame, select = -c(CepheBati,CepheDogu, CepheGuney,
                                          CepheKuzey))

print(dim(dataFrame))
dataFrame <- dataFrame[complete.cases(dataFrame), ]
print(dim(dataFrame))

# attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")

detect_outliers <- function(x, upperLimit, lowerLimit)
{
  Q1 <- quantile(x, lowerLimit)
  Q3 <- quantile(x, upperLimit)
  IQR <- Q3 - Q1
  Vl <- Q1 - 1.5 * IQR
  Vr <- Q3 + 1.5 * IQR
  return (which(x < Vl | x > Vr))
}

# find Outlier with IQR

# OrijinalAlan 
print(detect_outliers(dataFrame$OrijinalAlan, 0.985, 0.015))

# AlanMetrekare 
print(detect_outliers(dataFrame$AlanMetrekare, 0.985, 0.015))

# FiyatTL  
print(detect_outliers(dataFrame$FiyatTL, 0.985, 0.015))



# find Outlier with DBSCAN

numericData <- select_if(dataFrame, is.numeric)
# d <- scale(numericData)
# db <- dbscan(d, eps=0.9, MinPts=5)
# db
# 
# table(db$cluster,dataFrame$OrijinalAlan)
# table(db$cluster,dataFrame$AlanMetrekare)
# table(db$cluster,dataFrame$FiyatTL)

# -----------------------------------------------------------------------------
# DBscan

dbscan.outliers <- function(data, ...) {
  require(fpc, quietly=TRUE)
  cl <- dbscan(data, ...)
  posOuts <- which(cl$cluster == 0)
  list(positions = posOuts,
       outliers = data[posOuts,],
       dbscanResults = cl)
}

outs <- dbscan.outliers(numericData,
                        eps = 3,
                        scale=TRUE)
# 
# tibbledData = as_tibble(dataFrame)

