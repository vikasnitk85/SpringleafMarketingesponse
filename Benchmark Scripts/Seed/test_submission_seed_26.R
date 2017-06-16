#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 1
version <- 26
startTime <- Sys.time()

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
train[, names(train_char)] <- train_char
train[, names(train_date)] <- train_date

test_char <- test[, sapply(test, is.character)]
test_date <- test_char[, grep("JAN1|FEB1|MAR1", test_char), ]
test_char <- test_char[, !colnames(test_char) %in% colnames(test_date)]

test_date <- sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date <- do.call(cbind.data.frame, test_date)
test_date <- sapply(test_date, function(x) as.numeric(format(x, "%Y")))
test_date <- data.frame(test_date)
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

# drop variables with all missing observations
dropVars <- names(which(sapply(train, class) == "logical"))
train <- train[, setdiff(names(train), dropVars)]
test <- test[, setdiff(names(test), dropVars)]

# Remove variables for which standard deviation is zero
SD <- sapply(train, sd, na.rm=TRUE)
train <- train[, setdiff(names(train), names(SD[SD==0]))]
test <- test[, setdiff(names(test), names(SD[SD==0]))]

#
maxValues <- sapply(train, function(x) max(x, na.rm=TRUE))
tmpData <- train[, names(maxValues[maxValues==99])]
tempUnique <- sapply(tmpData, function(x) length(unique(na.omit(x))))

for(i in names(tempUnique[tempUnique < 99])) {
  tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
  train[, i] <- head(tmp, nrow(train))
  test[, i] <- tail(tmp, nrow(test))
}

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
  , "subsample" = 0.7
  , "colsample_bytree" = 0.4
  , "min_child_weight" = 6
  , "max_depth" = 9
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
head(modPerf)

xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
preds_out <- predict(model, xgtest, ntreelimit = modPerf$tree[1])

sub <- read.csv("sample_submission.csv")
sub$target <- preds_out
write.csv(sub, paste0("test_submission_seed_", version, ".csv"), row.names=FALSE)
