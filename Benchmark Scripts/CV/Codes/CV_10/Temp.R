#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/polk/nas/analytics/users/agrav00/new_used")
subversion <- "1"
startTime <- Sys.time()

#----------------------------------------------------------------
# Data
#----------------------------------------------------------------
load("data/TGT_TSPH_V1A_RD201408.RData")
names(rawData) <- toupper(names(rawData))

varList <- read.csv("data/Independent Variable List.csv", stringsAsFactors=FALSE)$Variable
varList <- toupper(varList)

train <- rawData[, varList]
y <- rawData$NVEH_NEW
y <- as.numeric(as.character(y))

# Convert character variables to numeric
for(i in names(train)) {
  if(class(train[, i]) == "factor") {
    train[, i] <- as.numeric(train[, i])
  }
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

varList <- c("VAR_1495", "VAR_1181", "VAR_1201", "VAR_1489", "VAR_0648", "VAR_1180")
for(i in 1:ncol(train)) {
  train[, i] <- as.numeric(train[, i])
}
train <- train[, varList]
for(i in 1:ncol(train)) {
  train[, i] <- as.numeric(as.factor(train[, i]))
}

for(fold in 1:10) {
  cat("Processing Fold: ", fold, "\n")
  hold <- kFolds[[fold]]
  xgtrain <- xgb.DMatrix(as.matrix(train[-hold, varList]), label = y[-hold], missing = NA)
  xgval <- xgb.DMatrix(as.matrix(train[hold, varList]), label = y[hold], missing = NA)
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
