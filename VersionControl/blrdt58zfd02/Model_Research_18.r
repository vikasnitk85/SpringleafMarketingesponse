#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Install Packages
# pkgs <- c("RCurl", "doSNOW", "caret", "ROCR", "xgboost", "foreach")
# install.packages(pkgs)

# Load Libraries
library(RCurl)
library(xgboost)
library(ROCR)

# Create Folders
# dir.create(paste(getwd(), "Springleaf", sep = "/"))
# dir.create(paste(getwd(), "Springleaf", "Development", sep = "/"))
# dir.create(paste(getwd(), "Springleaf", "Development", "Data", sep = "/"))
# dir.create(paste(getwd(), "Springleaf", "Development", "Model", sep = "/"))

# Set working directory
setwd("~/Springleaf/Development")

# Download training data from DropBox
# temporaryFile <- "~/Springleaf/Development/Data/rawData_split_1.RData"
# dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/rawData_split_1.RData"
# download.file(dataAddress, temporaryFile, method = "curl")

# Start recording console outputs
sink(file="Model/Model_Research_18.txt")

# Record start time
startTime <- Sys.time()

# Load Data
load(file="Data/rawData_split_1.RData")

# List of character variables
charVars <- names(which(sapply(dtrain, class) == "character"))
for(tmpVar in charVars) {
  dtrain[, tmpVar] <- factor(dtrain[, tmpVar])
  dval[, tmpVar] <- factor(dval[, tmpVar])
}

# Drop character variables with 1 class
dropCharVars <- names(which(sapply(dtrain[, charVars], nlevels) == 1))

# Missing Values
dtrain[dtrain==-99999] <- NA
dval[dval==-99999] <- NA
dtrain[dtrain==-1] <- NA
dval[dval==-1] <- NA

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

#----------------------------------------------------------------------------
# Model development
#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval <- xgb.DMatrix(data.matrix(dval[, feature.names]), label=dval$target, missing = NA)
gc()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 6,
              subsample = 0.8,
              # colsample_bytree = 0.6,
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
save(clf, file="Model/Model_Research_18.RData")

#----------------------------------------------------------------------------
# Model scoring
#----------------------------------------------------------------------------
# Download test data from DropBox
# temporaryFile <- "~/Springleaf/Development/Data/scoreData_1.RData"
# dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/scoreData_1.RData"
# download.file(dataAddress, temporaryFile, method = "curl")

# Load score data
load(file="Data/scoreData_1.RData")
for(tmpVar in charVars) {
  scoreData[, tmpVar] <- factor(scoreData[, tmpVar])
}

scoreData[scoreData==-99999] <- NA
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

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model/Model_Research_18.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
