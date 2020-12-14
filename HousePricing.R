
load("house.data")

data <- antalya_train

dataFrame <- data.frame(data)
dim(data)

dataFrame[dataFrame==""]<-NA

# Id is removed because for each row is unique
dataFrame$Id <- NULL

dim(dataFrame)
names(dataFrame)

columnTypes <- sapply(dataFrame, class)



tibbledData = as_tibble(dataFrame)