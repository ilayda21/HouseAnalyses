library(CORElearn)
library(tibble)

load("house.data")

data <- antalya_train

dataFrame <- data.frame(data)
dim(data)

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
attrEval(FiyatTL ~ . , dataFrame, estimator = "Gini")

dataFrame$MustakilMi <- factor(dataFrame$MustakilMi, labels = c("no", "yes"))
print(dataFrame$MustakilMi)

tibbledData = as_tibble(dataFrame)