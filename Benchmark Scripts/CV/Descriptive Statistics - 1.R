
#----------------------------------------------------------------
# Environment Set-up
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

library(xgboost)
setwd("/home/rstudio/Dropbox/Public/Springleaf")
subversion <- 9
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

#-------------------------------------------------------------------------------
# Function for generating basic descriptive statistics of categorical variables
#-------------------------------------------------------------------------------
desStatsCatVars <- function(x, Data) {
  tempData <- Data[, x]
  n <- length(tempData)                                                        # Number of observations
  nMiss <- sum(is.na(tempData))                                                # Number of missing observations
  nNonMiss <- n - nMiss                                                        # Number of non-missing observations
  nMissPer <- nMiss/n                                                          # Missing percentage
  nFacs <- length(unique(na.omit(tempData)))                                   # Number of factors
  tab <- sort(table(tempData), decreasing=TRUE)
  u2 <- round(1 - sum((tab/n)^2), 4)                                           # Variability factor
  retVal <- c(n, nMiss, nNonMiss, nMissPer, nFacs, u2)
  names(retVal) <- c("Number of Observations", "Number of Missing", "Number of Non Missing",
    "Percent Missing", "Number of Factors", "Variability Factor")
  return(retVal)
}

#-------------------------------------------------------------------------------
# Function for generating basic descriptive statistics of numerical variables
#-------------------------------------------------------------------------------
desStatsNumVars <- function(x, Data, freqCut=95/5, uniqueCut=10) {
  tempData <- Data[, x]
  
  n <- length(tempData)                                                        # Number of observations
  nMiss <- sum(is.na(tempData))                                                # Number of missing observations
  nNonMiss <- n - nMiss                                                        # Number of non-missing observations
  nMissPer <- nMiss/n                                                          # Missing percentage
  nUnique <- length(unique(na.omit(tempData)))                                 # Number of unique values
  
  variance <- var(tempData, na.rm=TRUE)                                        # Variance of the data
  stdev <- sd(tempData, na.rm=TRUE)                                            # Standard deviation of the data
  stats <- c(variance, stdev, summary(tempData)[1:6])                          # Other basic summary
  zeroVar <- as.character((nUnique==1) | all(is.na(tempData)))                 # Zero variance flag
  
  # Near zero variance
  t <- table(tempData[!is.na(tempData)])
  if(length(t) <= 1) {
    freqRatio <- 0
  } else {
    w <- which.max(t)
    freqRatio <- max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE)
  }
  percentUnique <- 100 * nUnique/n
  nearZeroVar <- as.character(freqRatio > freqCut & percentUnique <= uniqueCut)

  # Output  
  retVal <- c(n, nMiss, nNonMiss, nMissPer, nUnique, stats, zeroVar, nearZeroVar)
  names(retVal) <- c("Number of Observations", "Number of Missing", "Number of Non Missing",
    "Percent Missing", "Number of Unique Values", "Variance", "Standard Deviation", "Minimum",
    "1st Qu.", "Median", "Mean", "3rd Qu.", "Maximum", "Zero Variance", "Near Zero Variance")
  return(retVal)
}


#-------------------------------------------------------------------------------
# Generate basic statistics for numeric variables
#-------------------------------------------------------------------------------
numr_vars <- names(train)
numOut <- NULL
for(tmpVar in numr_vars) {
  numOut <- rbind(numOut, desStatsNumVars(tmpVar, train, freqCut=95/5, uniqueCut=10))
}
numOut <- data.frame(numOut)
names(numOut) <- c("Number of Observations", "Number of Missing", "Number of Non Missing",
  "Percent Missing", "Number of Unique Values", "Variance", "Standard Deviation", "Minimum",
  "1st Qu.", "Median", "Mean", "3rd Qu.", "Maximum", "Zero Variance", "Near Zero Variance")
numOut$Variable_Name <- numr_vars

#-------------------------------------------------------------------------------
# Export Outputs
#-------------------------------------------------------------------------------
write.csv(numOut, "Descriptive Statistics - 1.csv", row.names=FALSE)
