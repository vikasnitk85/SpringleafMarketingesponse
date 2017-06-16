#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# library(RStudioAMI)
# linkDropbox()
# install.packages(c("xgboost"))

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- "01"
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
train[train==-99999] <- NA

for(i in 1:ncol(train)) {
  if(class(train[, i]) == "character") {
    train[, i] <- as.numeric(as.factor(train[, i]))
  }
}

missingVales <- sapply(train, function(x) sum(is.na(x)))
length(names(missingVales[missingVales==0]))

for(f in names(train)) {
  train[, f] <- as.numeric(train[, f])
}

train <- train[names(missingVales[missingVales==0])]
gc()

# train[is.na(train)] <- -99999
# test[is.na(test)] <- -99999

#-------------------------------------------------------------------------------
# Remove the extreme near-zero variance
#-------------------------------------------------------------------------------
library(caret)
nzv <- nearZeroVar(train, saveMetrics = TRUE)
print(paste('Range:',range(nzv$percentUnique)))
print(head(nzv))
table(nzv$nzv)
table(nzv$zeroVar)
gc()

rownames(nzv[nzv$zeroVar, ])

train_nzv <- train[, setdiff(names(train), rownames(nzv[nzv$zeroVar, ]))]
dfEvaluate <- cbind(train_nzv, target=y)

#-------------------------------------------------------------------------------
# Function for building model
#-------------------------------------------------------------------------------
EvaluateAUC <- function(dfEvaluate) {
    require(xgboost)
    require(Metrics)
    CVs <- 5
    cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
    indexCount <- 1
    outcomeName <- c('target')
    predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
    lsErr <- c()
    lsAUC <- c()
    for(cv in seq(1:CVs)) {
        print(paste('cv',cv))
        dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
        dataTest <- dfEvaluate[dataTestIndex,]
		# cat("Test Dimension: ", dim(dataTest), "\n")
        dataTrain <- dfEvaluate[-dataTestIndex,]
		# cat("Train Dimension: ", dim(dataTrain), "\n")

        bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                       label = dataTrain[,outcomeName],
                       max.depth=6, eta = 1, verbose=0,
                       nround=5, nthread=4,
                       objective = "reg:linear")

        predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
        err <- rmse(dataTest[,outcomeName], predictions)
        auc <- auc(dataTest[,outcomeName],predictions)

        lsErr <- c(lsErr, err)
        lsAUC <- c(lsAUC, auc)
        gc()
    }
    print(paste('Mean Error:',mean(lsErr)))
    print(paste('Mean AUC:',mean(lsAUC)))
	mean(lsAUC)
}

#-------------------------------------------------------------------------------
# Performs without any PCA transformation
#-------------------------------------------------------------------------------
EvaluateAUC(dfEvaluate) # 0.7270646

#-------------------------------------------------------------------------------
# PCA
#-------------------------------------------------------------------------------
pmatrix <- scale(train_nzv)
princ <- prcomp(pmatrix)
predPCA <- predict(princ, newdata=pmatrix)

lsAUC <- c()
for(nComp in 1:100) {
    print(nComp)
	dfComponents <- predPCA[,1:nComp]
	dfEvaluate <- cbind(as.data.frame(dfComponents), target=y)
	auc <- EvaluateAUC(dfEvaluate)
    lsAUC <- c(lsAUC, auc)
}
