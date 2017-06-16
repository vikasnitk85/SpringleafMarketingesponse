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
# library(RCurl)
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
sink(file="Model/Model_Research_14.txt")

# Record start time
startTime <- Sys.time()

# Load Data
load(file="Data/rawData_split_1.RData")

# List of numeric variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- names(which(sapply(dtrain[, Variables], class) != "factor"))
charVars <- names(which(sapply(dtrain[, Variables], class) == "character"))
feature.names <- Variables
for(tmpVar in charVars) {
  dtrain[, tmpVar] <- factor(dtrain[, tmpVar])
  dval[, tmpVar] <- factor(dval[, tmpVar])
}

dtrain[dtrain==-99999] <- NA
dval[dval==-99999] <- NA

dtrain[dtrain==-1] <- NA
dval[dval==-1] <- NA

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
              max_depth = 10,
              subsample = 0.5,
              # colsample_bytree = 0.6,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 25000,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)
save(clf, file="Model/Model_Research_14.RData")

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

# Score data
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model/Model_Research_14.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
