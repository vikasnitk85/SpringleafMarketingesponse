#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

startTime <- Sys.time()

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 9

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- train$target
train <- train[, -c(1, 1934)]
test <- test[, -1]

# Character variables
train_char <- train[, sapply(train, is.character)]
train_date <- train_char[, grep("JAN1|FEB1|MAR1", train_char), ]
train_char <- train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date <- sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date <- do.call(cbind.data.frame, train_date)
train_date <- sapply(train_date, function(x) as.numeric(format(x, "%Y")))
train_date <- data.frame(train_date)
train_char[train_char==-1] <- NA
train[, names(train_char)] <- train_char
train[, names(train_date)] <- train_date

test_char <- test[, sapply(test, is.character)]
test_date <- test_char[, grep("JAN1|FEB1|MAR1", test_char), ]
test_char <- test_char[, !colnames(test_char) %in% colnames(test_date)]
test_date <- sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date <- do.call(cbind.data.frame, test_date)
test_date <- sapply(test_date, function(x) as.numeric(format(x, "%Y")))
test_date <- data.frame(test_date)
test_char[test_char==-1] <- NA
test[, names(test_char)] <- test_char
test[, names(test_date)] <- test_date

for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}

train[train==-99999] <- NA
test[test==-99999] <- NA
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
