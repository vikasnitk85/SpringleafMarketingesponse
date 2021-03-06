#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1
version <- 6

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
train_YR <- sapply(train_date, function(x) as.numeric(format(x, "%Y")))
train_YR <- data.frame(train_YR)
train_MTH <- sapply(train_date, function(x) as.numeric(format(x, "%m")))
train_MTH <- data.frame(train_MTH)
names(train_MTH) <- paste0(names(train_MTH), "_MTH")
train[, names(train_char)] <- train_char
train[, names(train_date)] <- train_YR
train <- cbind(train, train_MTH)

test_char <- test[, sapply(test, is.character)]
test_date <- test_char[, grep("JAN1|FEB1|MAR1", test_char), ]
test_char <- test_char[, !colnames(test_char) %in% colnames(test_date)]
test_date <- sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date <- do.call(cbind.data.frame, test_date)
test_YR <- sapply(test_date, function(x) as.numeric(format(x, "%Y")))
test_YR <- data.frame(test_YR)
test_MTH <- sapply(test_date, function(x) as.numeric(format(x, "%m")))
test_MTH <- data.frame(test_MTH)
names(test_MTH) <- paste0(names(test_MTH), "_MTH")
test[, names(test_char)] <- test_char
test[, names(test_date)] <- test_YR
test <- cbind(test, test_MTH)

for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}

set.seed(1948 ^ subversion)
hold <- sample(1:nrow(train), 15000) #10% training data for stopping
xgtrain <- xgb.DMatrix(as.matrix(train[-hold, ]), label = y[-hold], missing = NA)
xgval <- xgb.DMatrix(as.matrix(train[hold, ]), label = y[hold], missing = NA)
gc()

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

watchlist <- list('val' = xgval)
model = xgb.train(
  nrounds = 5000   # increase for more results at home
  , params = param0
  , data = xgtrain
  # , early.stop.round = 5
  , watchlist = watchlist
  , print.every.n = 5
)

#----------------------------------------------------------------
# Scoring
#----------------------------------------------------------------
xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
preds_out <- predict(model, xgtest, ntreelimit = 4995)

sub <- read.csv("sample_submission.csv")
sub$target <- preds_out
write.csv(sub, paste0("test_submission_", version, ".csv"), row.names=FALSE)
