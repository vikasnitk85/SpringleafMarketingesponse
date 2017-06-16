#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
library(ROCR)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- "1_01"
startTime <- Sys.time()
print(startTime)

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- train$target
train <- train[, -c(1, 1934)]
test <- test[, -1]

# Convert character variables to numeric
for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}

# By taking all variables at a time, maximum auc in validation set 0.781098
for(i in names(train)) {
	train[, i] <- as.numeric(train[, i])
}

# Split training data
set.seed(1948)
hold <- sample(1:nrow(train), 30000) # 20% training data for stopping
hold1 <- sample(hold, 15000)
hold2 <- hold[!hold %in% hold1]

#----------------------------------------------------------------
# Model - 1
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

xgtrain <- xgb.DMatrix(as.matrix(train[-hold, ]), label = y[-hold], missing = NA)
xgval1 <- xgb.DMatrix(as.matrix(train[hold1, ]), label = y[hold1], missing = NA)
xgval2 <- xgb.DMatrix(as.matrix(train[hold2, ]), label = y[hold2], missing = NA)

watchlist <- list('train' = xgtrain, 'val1' = xgval1, 'val2' = xgval2)
set.seed(2012)
aucVal <- capture.output(model1 <- xgb.train(
	nrounds = 500
  , params = param0
  , data = xgtrain
  , watchlist = watchlist
  , print.every.n = 5
))

endTime <- Sys.time()
difftime(endTime, startTime)

#----------------------------------------------------------------
# Model - 2
#----------------------------------------------------------------
colIndex <- 1:ncol(train)
kFolds <- split(colIndex, ceiling(seq_along(colIndex)/100))
valOut1 <- NULL
valOut2 <- NULL

for(k in 1:length(kFolds)) {
	cat("Processing set ", k, "\n")
	xgtrain <- xgb.DMatrix(as.matrix(train[-hold, kFolds[[k]]]), label = y[-hold], missing = NA)
	xgval1 <- xgb.DMatrix(as.matrix(train[hold1, kFolds[[k]]]), label = y[hold1], missing = NA)
	xgval2 <- xgb.DMatrix(as.matrix(train[hold2, kFolds[[k]]]), label = y[hold2], missing = NA)
	gc()

	set.seed(2012)
	model <- xgb.train(
		nrounds = 500
	  , params = param0
	  , data = xgtrain
	  , verbose = 0
	)
	
	preds_out1 <- predict(model, xgval1, ntreelimit = 500)
	valOut1 <- cbind(valOut1, preds_out1)
	preds_out2 <- predict(model, xgval2, ntreelimit = 500)
	valOut2 <- cbind(valOut2, preds_out2)
}

endTime <- Sys.time()
difftime(endTime, startTime)

valOut1 <- data.frame(valOut1)
valOut2 <- data.frame(valOut2)
xgvalOut1 <- xgb.DMatrix(as.matrix(valOut1), label = y[hold1], missing = NA)
xgvalOut2 <- xgb.DMatrix(as.matrix(valOut2), label = y[hold2], missing = NA)

watchlist <- list('valOut1' = xgvalOut1, 'valOut2' = xgvalOut2)
set.seed(2012)
finalModel <- xgb.train(
	nrounds = 500
  , params = param0
  , data = xgvalOut1
  , watchlist = watchlist
  , print.every.n = 5
)

endTime <- Sys.time()
difftime(endTime, startTime)



#----------------------------------------------------------------
# Score Validation set
#----------------------------------------------------------------


sub <- y[hold]
sub$target <- preds_out
write.csv(sub, paste0("test_submission_seed_", version, ".csv"), row.names=FALSE)
