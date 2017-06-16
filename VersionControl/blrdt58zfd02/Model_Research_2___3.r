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
load(file="Data/rawData_split.RData")

# List of numeric variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- names(which(sapply(dtrain[, Variables], class) != "factor"))
feature.names <- Variables

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval <- xgb.DMatrix(data.matrix(dval[, feature.names]), label=dval$target, missing = NA)
gc()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective           = "binary:logistic",
              # booster = "gblinear",
              missing = NA,
              eta                 = 0.01,
              max_depth           = 8,
              subsample           = 0.5,
              # colsample_bytree    = 0.6,
              eval_metric         = "auc"
              )

set.seed(2012)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2500,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#  [499]  train-auc:0.906743      dval-auc:0.782726
#  [999]  train-auc:0.951966      dval-auc:0.787862
# [1499]  train-auc:0.976867      dval-auc:0.789536
# [1999]  train-auc:0.989546      dval-auc:0.790185
# [2499]  train-auc:0.995873      dval-auc:0.790266
# Model_Research_2                Leaderboad-auc:0.79148
# Model_Research_3                Leaderboad-auc:0.79144


#----------------------------------------------------------------------------
load(file="Data/scoreData.RData")

submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model/Model_Research_2.csv", row.names=FALSE)

submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA, ntreelimit = 1999)
}
write.csv(submission, "Model/Model_Research_3.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Model/varImp_Model_Research_2.csv", row.names=FALSE)
