# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(xgboost)

# Load Data
load(file="Data/train_1.RData")
load(file="Data/test_1.RData")

train[, "VAR_0200"] <- NULL
test[, "VAR_0200"] <- NULL

#----------------------------------------------------------------------------
# 
#----------------------------------------------------------------------------
dateVars <- names(which(sapply(train, class) == "Date"))
catVars <- names(which(sapply(train, class) == "factor"))
numVars <- setdiff(c(names(which(sapply(train, class) == "integer")), names(which(sapply(train, class) == "numeric"))), c("ID", "target"))

train <- train[, c("ID", "target", numVars)]
test <- test[, c("ID", numVars)]
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

feature.names <- numVars

train <- train[sample(nrow(train), 40000), ]
gc()

#----------------------------------------------------------------------------
clf <- xgboost(data=data.matrix(train[, feature.names]), label=train$target, nrounds=100, objective="binary:logistic", eval_metric="auc")

#----------------------------------------------------------------------------
submission <- data.frame(ID=test$ID)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(test[rows, feature.names]))
}

write.csv(submission, "Models/Model_Development.csv", row.names=FALSE)


