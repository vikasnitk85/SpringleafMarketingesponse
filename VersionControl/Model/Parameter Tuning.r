# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(xgboost)
library(ROCR)

# Load Data
load(file="Data/train_1.RData")
load(file="Data/test_1.RData")

train[, "VAR_0200"] <- NULL
test[, "VAR_0200"] <- NULL

#----------------------------------------------------------------------------
# Preparing Data
#----------------------------------------------------------------------------
dateVars <- names(which(sapply(train, class) == "Date"))
catVars <- names(which(sapply(train, class) == "factor"))
numVars <- setdiff(c(names(which(sapply(train, class) == "integer")), names(which(sapply(train, class) == "numeric"))), c("ID", "target"))

for(i in catVars) {
  if(sum(is.na(train[, i])) > 0) {
    levels(train[, i]) <- c(levels(train[, i]), "MISSING")
    train[which(is.na(train[, i])), i] <- "MISSING"
  }
}

train <- train[, c("ID", "target", numVars, catVars)]
test <- test[, c("ID", numVars, catVars)]
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

#----------------------------------------------------------------------------
# Splitting Data
#----------------------------------------------------------------------------
set.seed(1)
index <- sample(nrow(train), 96820)

train_1 <- train[index, ]
train_2 <- train[-index, ]

set.seed(1)
index_2 <- sample(nrow(train_1), 48410)
train <- train_1[index_2, ]
train_3 <- train_1[-index_2, ]
gc()

varImp <- read.csv("Models/varImp_Model_Development_3.csv", stringsAsFactors=FALSE)
feature.names <- varImp$Feature[1:150]

dtrain <- xgb.DMatrix(data.matrix(train[, feature.names]), label=train$target)
dval1 <- xgb.DMatrix(data.matrix(train_2[, feature.names]), label=train_2$target)
dval2 <- xgb.DMatrix(data.matrix(train_3[, feature.names]), label=train_3$target)

rm(train)
rm(train_1)
rm(train_2)
rm(train_3)
rm(index)
rm(index_2)
gc()

#----------------------------------------------------------------------------
# Parameter Tuning
#----------------------------------------------------------------------------
watchlist <- list(train=dtrain, validation1=dval1, validation2=dval2)
param <- list(objective = "binary:logistic",
              booster = "gbtree",
              eta = 0.01,
              gamma = 1,
              max_depth = 3,
              min_child_weight = 50,
              subsample = 0.7,
              colsample_bytree = 0.6,
              eval_metric = "auc"
              )

modelFit <- xgb.train(params = param,
              data = dtrain,
              nrounds = 10000,
              watchlist = watchlist,
              verbose = 2,
              print.every.n = 1,
              maximize = TRUE)

history <- xgb.cv(data=dtrain, nround=600, nthread=2, nfold=5, metrics="auc",
                  max.depth=6, eta=.02, objective="binary:logistic")
print(history)
