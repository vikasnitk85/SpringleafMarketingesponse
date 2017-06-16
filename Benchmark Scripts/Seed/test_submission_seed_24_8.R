#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1
version <- "24_8"
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
train[, names(train_char)] <- train_char

test_char <- test[, sapply(test, is.character)]
test_date <- test_char[, grep("JAN1|FEB1|MAR1", test_char), ]
test_char <- test_char[, !colnames(test_char) %in% colnames(test_date)]
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
varTrans <- read.csv("Variable_Transformation_3.csv", stringsAsFactors=FALSE)

# Numeric Factor Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="NUM_FACT")]
for(i in tmpVars) {
  train[, i] <- as.numeric(as.factor(train[, i]))
  test[, i] <- as.numeric(as.factor(test[, i]))
}

# Log Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="LOG")]
for(i in tmpVars) {
  train[, i] <- log(train[, i])
  test[, i] <- log(test[, i])
}

# Replace 999999999 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999999999")]
for(i in tmpVars) {
  train[which(train[, i] == 999999999), i] <- NA
  test[which(test[, i] == 999999999), i] <- NA
}

# Replace 98 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_98")]
for(i in tmpVars) {
  train[which(train[, i] == 98), i] <- NA
  test[which(test[, i] == 98), i] <- NA
}

# Replace 9998 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_9998")]
for(i in tmpVars) {
  train[which(train[, i] == 9998), i] <- NA
  test[which(test[, i] == 9998), i] <- NA
}

# Replace 97 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_97")]
for(i in tmpVars) {
  train[which(train[, i] == 97), i] <- NA
  test[which(test[, i] == 97), i] <- NA
}

# Inverse Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="INV")]
for(i in tmpVars) {
  train[, i] <- train[, i]^-1
  test[, i] <- test[, i]^-1
}

# Replace REP_999999999_999999994 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999999999_999999994")]
for(i in tmpVars) {
  train[which(train[, i] %in% c(999999999, 999999998, 999999997, 999999996, 999999995, 999999994)), i] <- NA
  test[which(test[, i] %in% c(999999999, 999999998, 999999997, 999999996, 999999995, 999999994)), i] <- NA
}

# Replace REP_999999997 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999999997")]
for(i in tmpVars) {
  train[which(train[, i] == 999999997), i] <- NA
  test[which(test[, i] == 999999997), i] <- NA
}

# Replace REP_99_90 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_99_90")]
for(i in tmpVars) {
  train[which(train[, i] %in% c(99, 98, 97, 96, 95, 94, 90)), i] <- NA
  test[which(test[, i] %in% c(99, 98, 97, 96, 95, 94, 90)), i] <- NA
}

# Cube Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="CUBE")]
for(i in tmpVars) {
  train[, i] <- train[, i]^3
  test[, i] <- test[, i]^3
}

# Square Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="SQUARE")]
for(i in tmpVars) {
  train[, i] <- train[, i]^2
  test[, i] <- test[, i]^2
}

# Replace REP_9999 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_90")]
for(i in tmpVars) {
  train[which(train[, i] == 9999), i] <- NA
  test[which(test[, i] == 9999), i] <- NA
}

# Replace REP_99 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_99")]
for(i in tmpVars) {
  train[which(train[, i] == 99), i] <- NA
  test[which(test[, i] == 99), i] <- NA
}

# Replace REP_90 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_90")]
for(i in tmpVars) {
  train[which(train[, i] == 90), i] <- NA
  test[which(test[, i] == 90), i] <- NA
}

# Square Root Transformation
tmpVars <- varTrans$Variable[which(varTrans$BEST=="SQRT")]
for(i in tmpVars) {
  train[, i] <- train[, i]^0.5
  test[, i] <- test[, i]^0.5
}

# Replace REP_997 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_997")]
for(i in tmpVars) {
  train[which(train[, i] == 997), i] <- NA
  test[which(test[, i] == 997), i] <- NA
}

# Replace REP_94 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_94")]
for(i in tmpVars) {
  train[which(train[, i] == 94), i] <- NA
  test[which(test[, i] == 94), i] <- NA
}

# Replace REP_9996 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_9996")]
for(i in tmpVars) {
  train[which(train[, i] == 9996), i] <- NA
  test[which(test[, i] == 9996), i] <- NA
}

# Replace REP_96 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_96")]
for(i in tmpVars) {
  train[which(train[, i] == 96), i] <- NA
  test[which(test[, i] == 96), i] <- NA
}

# Replace REP_999999998 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999999998")]
for(i in tmpVars) {
  train[which(train[, i] == 999999998), i] <- NA
  test[which(test[, i] == 999999998), i] <- NA
}

# Replace REP_999 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999")]
for(i in tmpVars) {
  train[which(train[, i] == 999), i] <- NA
  test[which(test[, i] == 999), i] <- NA
}

# Replace REP_995 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_995")]
for(i in tmpVars) {
  train[which(train[, i] == 995), i] <- NA
  test[which(test[, i] == 995), i] <- NA
}

# Replace REP_994 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_994")]
for(i in tmpVars) {
  train[which(train[, i] == 994), i] <- NA
  test[which(test[, i] == 994), i] <- NA
}

# Replace REP_95 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_95")]
for(i in tmpVars) {
  train[which(train[, i] == 95), i] <- NA
  test[which(test[, i] == 95), i] <- NA
}

# Replace REP_999_990 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999_990")]
for(i in tmpVars) {
  train[which(train[, i] %in% c(999, 998, 997, 996, 995, 994, 990)), i] <- NA
  test[which(test[, i] %in% c(999, 998, 997, 996, 995, 994, 990)), i] <- NA
}

# Replace REP_998 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_998")]
for(i in tmpVars) {
  train[which(train[, i] == 998), i] <- NA
  test[which(test[, i] == 998), i] <- NA
}

# Replace REP_9999_9990 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_9999_9990")]
for(i in tmpVars) {
  train[which(train[, i] %in% c(9999, 9998, 9997, 9996, 9995, 9994, 9990)), i] <- NA
  test[which(test[, i] %in% c(9999, 9998, 9997, 9996, 9995, 9994, 9990)), i] <- NA
}

# Replace REP_990 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_990")]
for(i in tmpVars) {
  train[which(train[, i] == 990), i] <- NA
  test[which(test[, i] == 990), i] <- NA
}

# Replace REP_996 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_996")]
for(i in tmpVars) {
  train[which(train[, i] == 996), i] <- NA
  test[which(test[, i] == 996), i] <- NA
}

# Replace REP_9994 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_9994")]
for(i in tmpVars) {
  train[which(train[, i] == 9994), i] <- NA
  test[which(test[, i] == 9994), i] <- NA
}

# Replace REP_1 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_1")]
for(i in tmpVars) {
  train[which(train[, i] == -1), i] <- NA
  test[which(test[, i] == -1), i] <- NA
}

# Replace REP_999999996 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999999996")]
for(i in tmpVars) {
  train[which(train[, i] == 999999996), i] <- NA
  test[which(test[, i] == 999999996), i] <- NA
}

# Replace REP_999994 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_999994")]
for(i in tmpVars) {
  train[which(train[, i] == 999994), i] <- NA
  test[which(test[, i] == 999994), i] <- NA
}

# Replace REP_9990 by NA
tmpVars <- varTrans$Variable[which(varTrans$BEST=="REP_9990")]
for(i in tmpVars) {
  train[which(train[, i] == 9990), i] <- NA
  test[which(test[, i] == 9990), i] <- NA
}

# Replace Inf by NA
for(i in names(train)) {
  if(sum(is.infinite(train[, i]))) train[is.infinite(train[, i]), i] <- NA
  if(sum(is.nan(train[, i]))) train[is.nan(train[, i]), i] <- NA
  if(sum(is.infinite(test[, i]))) test[is.infinite(test[, i]), i] <- NA
  if(sum(is.nan(test[, i]))) test[is.nan(test[, i]), i] <- NA
}

# Split data
set.seed(1948 ^ subversion)
hold <- sample(1:nrow(train), 15000) #10% training data for stopping
xgtrain <- xgb.DMatrix(as.matrix(train[-hold, ]), label = y[-hold], missing = NA)
xgval <- xgb.DMatrix(as.matrix(train[hold, ]), label = y[hold], missing = NA)
gc()

#----------------------------------------------------------------
# Model
#----------------------------------------------------------------
sink(file=paste0("test_submission_seed_", version, ".txt"))
param0 <- list(
   "objective" = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.001
  , "subsample" = 0.8
  , "colsample_bytree" = 0.5
  , "min_child_weight" = 6
  , "max_depth" = 10
  , "alpha" = 4
)

set.seed(2012)
watchlist <- list('val' = xgval)
model = xgb.train(
  nrounds = 60000   # increase for more results at home
  , params = param0
  , data = xgtrain
  # , early.stop.round = 5
  , watchlist = watchlist
  , print.every.n = 5
)

endTime <- Sys.time()
difftime(endTime, startTime)
sink()

#----------------------------------------------------------------
# Scoring
#----------------------------------------------------------------
# Extract best tree
tempOut <- readLines(paste0("test_submission_seed_", version, ".txt"))
tempOut <- tempOut[-length(tempOut)]
AUC <- sapply(tempOut, function(x) as.numeric(unlist(strsplit(x, split=":"))[2]))
names(AUC) <- NULL
modPerf <- data.frame(AUC)
tree <- sapply(tempOut, function(x) unlist(strsplit(x, split=":"))[1])
names(tree) <- NULL
tree <- gsub("\\t", "", tree)
tree <- gsub("val-auc", "", tree)
tree <- gsub(" ", "", tree)
tree <- gsub("]", "", tree)
tree <- gsub("\\[", "", tree)
tree <- as.numeric(tree)
modPerf$tree <- tree
modPerf <- modPerf[order(modPerf$AUC, decreasing=TRUE), ]

xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
preds_out <- predict(model, xgtest, ntreelimit = modPerf$tree[1])

sub <- read.csv("sample_submission.csv")
sub$target <- preds_out
write.csv(sub, paste0("test_submission_seed_", version, ".csv"), row.names=FALSE)
