#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

startTime <- Sys.time()

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- train$target
train <- train[, -c(1, 1934)]
test <- test[, -1]
for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}
train[is.na(train)] <- -9999
test[is.na(test)] <- -9999

xgtrain <- xgb.DMatrix(as.matrix(train), label = y, missing = NA)

#----------------------------------------------------------------
# Model
#----------------------------------------------------------------
param0 <- list(
   "objective" = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.01
  , "subsample" = 0.7
  , "colsample_bytree" = 0.5
  , "min_child_weight" = 6
  , "max_depth" = 9
  , "alpha" = 4
)

set.seed(2012)
model = xgb.cv(
  nrounds = 5000
  , params = param0
  , nfold = 5
  , data = xgtrain
  , print.every.n = 10
)

#----------------------------------------------------------------
# Results
#----------------------------------------------------------------
write.csv(model, paste0("CV_Results_", subversion, ".csv"))
endTime <- Sys.time()
difftime(endTime, startTime)
