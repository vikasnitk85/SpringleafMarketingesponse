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

set.seed(1)
index <- sample(nrow(train), 80000)

inTrain <- train[index, ]
outTrain <- train[-index, ]

set.seed(1)
h <- sample(nrow(inTrain), 40000)
train <- inTrain[h, ]
val <- inTrain[-h, ]
gc()

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(train[, feature.names]), label=train$target)
dval <- xgb.DMatrix(data.matrix(val[, feature.names]), label=val$target)

watchlist <- list(eval=dval, train=dtrain)
param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.1,
                max_depth           = 3,  # changed from default of 6
                # subsample           = 0.6,
                # colsample_bytree    = 0.6,
                eval_metric         = "auc"
                # alpha = 0.01 
                # lambda = 1
                )

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 375,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#----------------------------------------------------------------------------
crossValOut <- data.frame(target=outTrain$target)
crossValOut$predicted <- NA 
for (rows in split(1:nrow(outTrain), ceiling((1:nrow(outTrain))/10000))) {
    crossValOut[rows, "predicted"] <- predict(clf, data.matrix(outTrain[rows, feature.names]))
}
str(crossValOut)
pred <- prediction(crossValOut$predicted, outTrain$target)
tmpAuc <- performance(pred, "auc")
as.numeric(tmpAuc@y.values)

#----------------------------------------------------------------------------
submission <- data.frame(ID=test$ID)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(test[rows, feature.names]))
}

write.csv(submission, "Models/Model_Development_4.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Models/varImp_Model_Development_4.csv", row.names=FALSE)

