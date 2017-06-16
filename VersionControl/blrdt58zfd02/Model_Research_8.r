# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("~/Springleaf/Development")

sink(file="Model/Model_Research_8.txt")
startTime <- Sys.time()

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
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 12,
              subsample = 0.5,
              # colsample_bytree = 0.6,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param, 
                 data                = dtrain, 
                 nrounds             = 25000,
                 verbose             = 2, 
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)
save(clf, file="Model/Model_Research_8.RData")
#----------------------------------------------------------------------------
# temporaryFile <- "~/Springleaf/Development/Data/scoreData.RData"
# dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/scoreData.RData"
# download.file(dataAddress, temporaryFile, method = "curl")

load(file="Data/scoreData.RData")

submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model/Model_Research_8.csv", row.names=FALSE)

# varImp <- xgb.importance(feature_names=feature.names, model=clf)
# write.csv(varImp, "Model/varImp_Model_Research_8.csv", row.names=FALSE)
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
