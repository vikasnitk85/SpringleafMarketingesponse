#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1
startTime <- Sys.time()

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

#----------------------------------------------------------------
# Model Parameters
#----------------------------------------------------------------
# Create index for cross validation
set.seed(1948)
index <- sample(nrow(train), nrow(train)-1)
kFolds <- split(index, ceiling(seq_along(index)/29046))

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

for(fold in 1:5) {
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

  cat("Highest accuracy:", round(max(AUC), 6), " Tree: ", names(which.max(AUC)), "\n")
}

Mean <- rowMeans(Outputs[, -1])
SD <- apply(Outputs[, -1], 1, sd)
Outputs$Mean <- Mean
Outputs$SD <- SD
write.csv(Outputs, paste0("Outputs_CV_", subversion, ".csv"), row.names=FALSE)
head(Outputs[order(Outputs$Mean, decreasing=TRUE), ])

endTime <- Sys.time()
difftime(endTime, startTime)
