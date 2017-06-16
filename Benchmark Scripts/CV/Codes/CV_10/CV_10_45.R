#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- "10_45"
startTime <- Sys.time()

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- train$target
train <- train[, -c(1, 1934)]
test <- test[, -1]

# Separate date and character variables
train_char <- train[, sapply(train, is.character)]
train_date <- train_char[, grep("JAN1|FEB1|MAR1", train_char), ]
train_char <- train_char[, !colnames(train_char) %in% colnames(train_date)]
train_char[train_char=="[]"] <- NA
train[, names(train_char)] <- train_char

test_char <- test[, sapply(test, is.character)]
test_date <- test_char[, grep("JAN1|FEB1|MAR1", test_char), ]
test_char <- test_char[, !colnames(test_char) %in% colnames(test_date)]
test_char[test_char=="[]"] <- NA
test[, names(test_char)] <- test_char

# Converting date variables to year
train_date <- sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date <- do.call(cbind.data.frame, train_date)
train_date <- sapply(train_date, function(x) as.numeric(format(x, "%Y")))
train_date <- data.frame(train_date)
train[, names(train_date)] <- train_date

test_date <- sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date <- do.call(cbind.data.frame, test_date)
test_date <- sapply(test_date, function(x) as.numeric(format(x, "%Y")))
test_date <- data.frame(test_date)
test[, names(test_date)] <- test_date

# Convert character variables to numeric
for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}

# Replace -99999 by NA
train[train==-99999] <- NA
test[test==-99999] <- NA

#----------------------------------------------------------------
# Other data processing
#----------------------------------------------------------------
nUnique <- sapply(train, function(x) length(unique(na.omit(x))))
Max <- sapply(train, function(x) max(x, na.rm=TRUE))
modVars1 <- names(which(Max==99 & nUnique >= 3 & nUnique <= 10))
modVars2 <- names(which(Max==99 & nUnique >= 11 & nUnique <= 20))
modVars <- c(modVars1, modVars2)
modVars

for(i in modVars) {
  tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
  train[, i] <- head(tmp, nrow(train))
  test[, i] <- tail(tmp, nrow(test))
}

#----------------------------------------------------------------
# Model Parameters
#----------------------------------------------------------------
# Create index for cross validation
set.seed(1948)
index <- sample(nrow(train), nrow(train)-1)
kFolds <- split(index, ceiling(seq_along(index)/14523))

# Define model parameters
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

#----------------------------------------------------------------
# Develop Model
#----------------------------------------------------------------
nTree <- 5000
Outputs <- data.frame("Tree"=0:(nTree-1))
fold <- 1

for(fold in 1:10) {
  cat("Processing Fold: ", fold, "\n")
  hold <- kFolds[[fold]]
  xgtrain <- xgb.DMatrix(as.matrix(train[-hold, ]), label = y[-hold], missing = NA)
  xgval <- xgb.DMatrix(as.matrix(train[hold, ]), label = y[hold], missing = NA)
  watchlist <- list('val' = xgval)
  aucVal <- capture.output(model <- xgb.train(
      nrounds = nTree
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
  ))

  AUC <- sapply(aucVal, function(x) as.numeric(unlist(strsplit(x, split=":"))[2]))
  names(AUC) <- sapply(names(AUC), function(x) unlist(strsplit(x, split=":"))[1])
  names(AUC) <- gsub("\\t", "", names(AUC))
  names(AUC) <- gsub("val-auc", "", names(AUC))
  names(AUC) <- gsub(" ", "", names(AUC))
  names(AUC) <- gsub("]", "", names(AUC))
  names(AUC) <- gsub("\\[", "", names(AUC))
  Outputs <- cbind(Outputs, AUC)
  names(Outputs)[fold + 1] <- paste0("Fold_", fold)
  write.csv(Outputs, paste0("Outputs_CV_", subversion, ".csv"), row.names=FALSE)

  tmpAvg <- rowMeans(as.data.frame(Outputs[, -1]))
  cat("Highest accuracy:", round(max(tmpAvg), 6), " Tree: ", which.max(tmpAvg), "\n")
}

Mean <- rowMeans(Outputs[, -1])
SD <- apply(Outputs[, -1], 1, sd)
Outputs$Mean <- Mean
Outputs$SD <- SD
write.csv(Outputs, paste0("Outputs_CV_", subversion, ".csv"), row.names=FALSE)
head(Outputs[order(Outputs$Mean, decreasing=TRUE), ])

endTime <- Sys.time()
difftime(endTime, startTime)
