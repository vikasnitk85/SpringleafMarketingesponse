#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1
version <- 3

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- train$target
train <- train[, -c(1, 1934)]
test <- test[, -1]

# Missing observations in character variables
train_char <- train[, sapply(train, is.character)]
train_char[train_char==-1] <- NA
train_char[train_char==""] <- NA
train_char[train_char=="[]"] <- NA
train[, names(train_char)] <- train_char

test_char <- test[, sapply(test, is.character)]
test_char[test_char==-1] <- NA
test_char[test_char==""] <- NA
test_char[test_char=="[]"] <- NA
test[, names(test_char)] <- test_char


for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}
# train[is.na(train)] <- -9999
# test[is.na(test)] <- -9999

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
preds_out <- predict(model, xgtest, ntreelimit = 4865)

sub <- read.csv("sample_submission.csv")
sub$target <- preds_out
write.csv(sub, paste0("test_submission_", version, ".csv"), row.names=FALSE)
