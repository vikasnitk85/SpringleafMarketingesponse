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
# Generate basic statistics for categorical variables
#-------------------------------------------------------------------------------
char_vars <- names(prepData[, sapply(prepData, is.factor)])
catOut <- NULL
for(tmpVar in char_vars) {
  catOut <- rbind(catOut, desStatsCatVars(tmpVar, prepData))
}
catOut <- data.frame(catOut)
names(catOut) <- c("Number of Observations", "Number of Missing", "Number of Non Missing",
  "Percent Missing", "Number of Factors", "Variability Factor")
catOut$Variable_Name <- char_vars

#-------------------------------------------------------------------------------
# Generate basic statistics for numeric variables
#-------------------------------------------------------------------------------
numr_vars <- setdiff(names(prepData[, sapply(prepData, is.numeric)]), c("ID", "target"))
numOut <- NULL
for(tmpVar in numr_vars) {
  numOut <- rbind(numOut, desStatsNumVars(tmpVar, prepData, freqCut=95/5, uniqueCut=10))
}
numOut <- data.frame(numOut)
names(numOut) <- c("Number of Observations", "Number of Missing", "Number of Non Missing",
  "Percent Missing", "Number of Unique Values", "Variance", "Standard Deviation", "Minimum",
  "1st Qu.", "Median", "Mean", "3rd Qu.", "Maximum", "Zero Variance", "Near Zero Variance")
numOut$Variable_Name <- numr_vars

#-------------------------------------------------------------------------------
# Export Outputs
#-------------------------------------------------------------------------------
write.csv(catOut, "catOut.csv", row.names=FALSE)
write.csv(numOut, "numOut.csv", row.names=FALSE)
