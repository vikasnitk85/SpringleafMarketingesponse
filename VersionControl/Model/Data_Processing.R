# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(caret)

# Load EDA Summary
load("EDA/EDA_SUMMARY_20150904.RData")

# Trim Function
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#----------------------------------------------------------------------------
# Remove Variables for which IV < 0.02
#----------------------------------------------------------------------------
rmVars1 <- names(retIVcat[retIVcat < 0.02])
rmVars2 <- names(retIVnum[retIVnum < 0.02])

#----------------------------------------------------------------------------
# Remove Variables for which p-Value of statistical test is greater than 0.05
#----------------------------------------------------------------------------
pValues <- NULL
for(i in names(retStatOut)) {
  pValues[i] <- retStatOut[[i]][3, 2]
}
pValues <- trim(pValues)
pValues1 <- as.numeric(pValues)
names(pValues1) <- names(pValues)
pValues1 <- sort(pValues1)
rmVars3 <- names(pValues1[pValues1 > 0.05])

#----------------------------------------------------------------------------
# Remove Variables for which Variance is Zero
#----------------------------------------------------------------------------
load("Data/train.RData")
train <- train[, setdiff(names(train), c(rmVars1, rmVars2, rmVars3))]
numVars <- c(names(which(sapply(train, class) == "integer")), names(which(sapply(train, class) == "numeric")))
zeroVar <- nearZeroVar(train[, numVars])
rmVars4 <- numVars[zeroVar]
gc()

#----------------------------------------------------------------------------
# Remove Variables for which all observations are missing
#----------------------------------------------------------------------------
rmVars5 <- names(which(sapply(train, class) == "logical"))
rmVars <- c(rmVars1, rmVars2, rmVars3, rmVars4, rmVars5)
rmVars <- unique(rmVars)

train <- train[, setdiff(names(train), rmVars)]
load("Data/test.RData")
test <- test[, setdiff(names(test), rmVars)]

save(train, file="Data/train_1.RData")
save(test, file="Data/test_1.RData")

#----------------------------------------------------------------------------
# Remove Variables with high missing percentages
#----------------------------------------------------------------------------
sort(sapply(train[, numVars], function(x) sum(is.na(x))))
rmVars <- c("VAR_0208", "VAR_0210", "VAR_0211", "VAR_0209")
train <- train[, setdiff(names(train), rmVars)]
test <- test[, setdiff(names(test), rmVars)]

#----------------------------------------------------------------------------
# Find Highly Correlated Variables
#----------------------------------------------------------------------------
numVars <- names(which(sapply(train, class) == "integer"))
numVars <- setdiff(numVars, c("ID", "target"))
corMat <- cor(train[, numVars], use="pairwise.complete.obs")
highCor <- findCorrelation(corMat, cutoff=0.9999)
rmVars <- numVars[highCor]
train <- train[, setdiff(names(train), rmVars)]
test <- test[, setdiff(names(test), rmVars)]

#----------------------------------------------------------------------------
# Find Linear Combinations
#----------------------------------------------------------------------------
numVars <- names(which(sapply(train, class) == "integer"))
numVars <- setdiff(numVars, c("ID", "target"))

table(sapply(train[, numVars], function(x) sum(is.na(x))))

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 56))
linComb1 <- findLinearCombos(train[complete.cases(train[, tmpVars]), tmpVars])
rmVars6 <- tmpVars[linComb1$remove]

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 89))
linComb2 <- findLinearCombos(train[complete.cases(train[, tmpVars]), tmpVars])

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 91))
linComb3 <- findLinearCombos(train[complete.cases(train[, tmpVars]), tmpVars])

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 918))
linComb4 <- findLinearCombos(train[complete.cases(train[, tmpVars]), tmpVars])

train <- train[, setdiff(names(train), rmVars6)]
test <- test[, setdiff(names(test), rmVars6)]
save(train, file="Data/train_1.RData")
save(test, file="Data/test_1.RData")

load(file="Data/train_1.RData")
load(file="Data/test_1.RData")

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 0))
linComb5 <- findLinearCombos(train[, tmpVars[501:600]])
rmVars6 <- c("VAR_1292")

tmpVars <- names(which(sapply(train[, numVars], function(x) sum(is.na(x))) == 0))
linComb5 <- findLinearCombos(train[, tmpVars[1:300]])
rmVars6 <- tmpVars[1:300][linComb5$remove]




