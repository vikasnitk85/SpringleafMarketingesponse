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
load(file="Data/rawData_split_1.RData")
load(file="Data/scoreData_1.RData")

# List of numeric variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- names(which(sapply(dtrain[, Variables], class) != "factor"))
feature.names <- numVars

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target)
dval1 <- xgb.DMatrix(data.matrix(dval1[, feature.names]), label=dval1$target)
dval2 <- xgb.DMatrix(data.matrix(dval2[, feature.names]), label=dval2$target)

watchlist <- list(train=dtrain, dval1=dval1, dval2=dval2)
param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.01,
                max_depth           = 6,  # changed from default of 6
                subsample           = 0.5,
                # colsample_bytree    = 0.6,
                eval_metric         = "auc"
                # alpha = 0.01 
                # lambda = 1
                )

set.seed(2012)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#----------------------------------------------------------------------------
submission <- data.frame(ID=scoreData_1$ID)
submission$target <- NA 
for (rows in split(1:nrow(scoreData_1), ceiling((1:nrow(scoreData_1))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData_1[rows, feature.names]))
}

write.csv(submission, "Models/Model_Development_11.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Models/varImp_Model_Development_11.csv", row.names=FALSE)

