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
load(file="Data/scoreData.RData")

# List of numeric variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- names(which(sapply(dtrain[, Variables], class) != "factor"))
feature.names <- Variables

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval <- xgb.DMatrix(data.matrix(dval[, feature.names]), label=dval$target, missing = NA)

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective           = "binary:logistic",
              # booster = "gblinear",
              missing = NA,
              eta                 = 0.01,
              max_depth           = 6,
              subsample           = 0.5,
              # colsample_bytree    = 0.6,
              eval_metric         = "auc"
              )

set.seed(2012)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#  [499]  train-auc:0.831214      dval-auc:0.778632
#  [999]  train-auc:0.868739      dval-auc:0.784739
# [1499]  train-auc:0.896041      dval-auc:0.786879
# [1999]  train-auc:0.918042      dval-auc:0.788196
#    Leaderboad-auc:0.78978

#----------------------------------------------------------------------------
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
# dtest <- xgb.DMatrix(data.matrix(scoreData[, feature.names]), missing = NA)
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}

write.csv(submission, "Model/Model_Research_1.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Model/varImp_Model_Research_1.csv", row.names=FALSE)
