#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Link DropBox
# library(RStudioAMI)
# linkDropbox()

# Install Packages
# pkgs <- c("doSNOW", "ROCR", "xgboost", "foreach")
# install.packages(pkgs)

# Load Libraries
library(xgboost)
library(ROCR)

# Set working directory
setwd("/home/rstudio/Dropbox/Public/Springleaf")

# Load Data
load("rawData_split_1.RData")
load("scoreData_1.RData")

# Replace Missing Value (-99999) with NA
dtrain[dtrain==-99999] <- NA
dval[dval==-99999] <- NA
scoreData[scoreData==-99999] <- NA
gc()

# Replace Missing Value (-1) with NA
tmpVars <- names(which(sort(colSums(dtrain==-1, na.rm = TRUE)) > 0))
tmpVars <- tmpVars[-grep("MTHS_", tmpVars)]
tmpData <- dtrain[, tmpVars]
tmpData[tmpData==-1] <- NA
dtrain[, tmpVars] <- tmpData

tmpVars <- names(which(sort(colSums(dval==-1, na.rm = TRUE)) > 0))
tmpVars <- tmpVars[-grep("MTHS_", tmpVars)]
tmpData <- dval[, tmpVars]
tmpData[tmpData==-1] <- NA
dval[, tmpVars] <- tmpData

tmpVars <- names(which(sort(colSums(scoreData==-1, na.rm = TRUE)) > 0))
tmpVars <- tmpVars[-grep("MTHS_", tmpVars)]
tmpData <- scoreData[, tmpVars]
tmpData[tmpData==-1] <- NA
scoreData[, tmpVars] <- tmpData

rm(tmpData)
rm(tmpVars)
gc()

# Number of unique values
tmpData <- rbind(dtrain[, setdiff(names(dtrain), c("ID", "target"))], dval[, setdiff(names(dval), c("ID", "target"))])
tmpData <- rbind(tmpData, scoreData[, setdiff(names(scoreData), c("ID", "target"))])
uniqueValue <- sapply(tmpData, function(x) length(unique(na.omit(x))))

# Drop variables with zero or one unique value or all values are unique
dropVars <- names(uniqueValue[uniqueValue==0 | uniqueValue==1 | uniqueValue==290343])
dtrain <- dtrain[, setdiff(names(dtrain), dropVars)]
dval <- dval[, setdiff(names(dval), dropVars)]
scoreData <- scoreData[, setdiff(names(scoreData), dropVars)]
tmpData <- tmpData[, setdiff(names(tmpData), dropVars)]

# Analyse Variables
headData <- lapply(tmpData, function(x) head(sort(unique(x))))
tailData <- lapply(tmpData, function(x) tail(sort(unique(x))))

capture.output(str(headData, list.len=2005), file ="headData.txt")
capture.output(str(tailData, list.len=2005), file ="tailData.txt")

# Negative value present only in dtrain
if(sum(dtrain[, "VAR_0154"] < 0, na.rm=TRUE) > 0) dtrain[which(dtrain[, "VAR_0154"] < 0), "VAR_0154"] <- NA

dtrain[, "VAR_0212"] <- NULL
dval[, "VAR_0212"] <- NULL
scoreData[, "VAR_0212"] <- NULL

# Year variables
"VAR_0294", "VAR_0314", "VAR_0332", 

# Month_Year (YYYYMM)
"VAR_0531"







summary(dtrain[, "VAR_0018"])



# List of character variables
charVars <- names(which(sapply(dtrain, class) == "character"))
for(tmpVar in charVars) {
  Levels <- unique(c(dtrain[, tmpVar], dval[, tmpVar], scoreData[, tmpVar]))
  dtrain[, tmpVar] <- factor(dtrain[, tmpVar], levels=Levels)
  dval[, tmpVar] <- factor(dval[, tmpVar], levels=Levels)
  scoreData[, tmpVar] <- factor(scoreData[, tmpVar], levels=Levels)
}

# Edit variables with with 1 class
oneClassCharVars <- names(which(sapply(dtrain[, charVars], nlevels) == 1))
for(tmpVar in oneClassCharVars) {
  dtrain[, tmpVar] <- ifelse(is.na(dtrain[, tmpVar]), 0, 1)
  dval[, tmpVar] <- ifelse(is.na(dval[, tmpVar]), 0, 1)
  scoreData[, tmpVar] <- ifelse(is.na(scoreData[, tmpVar]), 0, 1)
}


# List of numeric variables
numrVars <- setdiff(names(dtrain)[sapply(dtrain, is.numeric)], c("ID", "target"))

# Standard deviation of numeric variables
stdev <- sapply(dtrain[, numrVars], sd, na.rm=TRUE)
sort(stdev)
zeroSD <- c(names(stdev[which(stdev==0)]), names(stdev[is.na(stdev)]))

# Adjust dtrain
dtrain <- dtrain[, setdiff(names(dtrain), c(zeroSD, dropCharVars))]
dval <- dval[, setdiff(names(dval), c(zeroSD, dropCharVars))]
numrVars <- setdiff(names(dtrain)[sapply(dtrain, is.numeric)], c("ID", "target"))

# Create Logs and Sqrt of Numeric Variables
for(tmpVar in numrVars) {
  # Create log variable of dtrain
  dtrain[, paste0("LOG_", tmpVar)] <- log(dtrain[, tmpVar])
  if(sum(is.infinite(dtrain[, paste0("LOG_", tmpVar)])) > 0)
    dtrain[is.infinite(dtrain[, paste0("LOG_", tmpVar)]), paste0("LOG_", tmpVar)] <- 0
  
  # Create log variable of dval
  dval[, paste0("LOG_", tmpVar)] <- log(dval[, tmpVar])
  if(sum(is.infinite(dval[, paste0("LOG_", tmpVar)])) > 0)
    dval[is.infinite(dval[, paste0("LOG_", tmpVar)]), paste0("LOG_", tmpVar)] <- 0
  
  # Create sqrt variable of dtrain
  dtrain[, paste0("SQRT_", tmpVar)] <- sqrt(dtrain[, tmpVar])
  if(sum(is.infinite(dtrain[, paste0("SQRT_", tmpVar)])) > 0)
    dtrain[is.infinite(dtrain[, paste0("SQRT_", tmpVar)]), paste0("SQRT_", tmpVar)] <- 0
  
  # Create sqrt variable of dval
  dval[, paste0("SQRT_", tmpVar)] <- sqrt(dval[, tmpVar])
  if(sum(is.infinite(dval[, paste0("SQRT_", tmpVar)])) > 0)
    dval[is.infinite(dval[, paste0("SQRT_", tmpVar)]), paste0("SQRT_", tmpVar)] <- 0
}

Variables <- setdiff(names(dtrain), c("ID", "target"))
feature.names <- Variables

# Load score data

for(tmpVar in charVars) {
  scoreData[, tmpVar] <- factor(scoreData[, tmpVar])
}


scoreData[scoreData==-1] <- NA
scoreData <- scoreData[, setdiff(names(scoreData), c(zeroSD, dropCharVars))]

# Create Logs and Sqrt of Numeric Variables
for(tmpVar in numrVars) {
  # Create log variable of scoreData
  scoreData[, paste0("LOG_", tmpVar)] <- log(scoreData[, tmpVar])
  if(sum(is.infinite(scoreData[, paste0("LOG_", tmpVar)])) > 0)
    scoreData[is.infinite(scoreData[, paste0("LOG_", tmpVar)]), paste0("LOG_", tmpVar)] <- 0
  
  # Create sqrt variable of scoreData
  scoreData[, paste0("SQRT_", tmpVar)] <- sqrt(scoreData[, tmpVar])
  if(sum(is.infinite(scoreData[, paste0("SQRT_", tmpVar)])) > 0)
    scoreData[is.infinite(scoreData[, paste0("SQRT_", tmpVar)]), paste0("SQRT_", tmpVar)] <- 0
}

dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval <- xgb.DMatrix(data.matrix(dval[, feature.names]), label=dval$target, missing = NA)
gc()

#----------------------------------------------------------------------------
# Model development - 1
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_23.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.1,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_23.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_23.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 2
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_24.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.2,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_24.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_24.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 3
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_25.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.3,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_25.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_25.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 4
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_26.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.4,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_26.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_26.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 5
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_27.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.5,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_27.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_27.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 6
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_28.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.6,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_28.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_28.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 7
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_29.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.7,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_29.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_29.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 8
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_30.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.8,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_30.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_30.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 9
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_31.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.9,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_31.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_31.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()

#----------------------------------------------------------------------------
# Model development - 10
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_32.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 10000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

# Save model specs
# save(clf, file="Model_Research_32.RData")

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_32.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
