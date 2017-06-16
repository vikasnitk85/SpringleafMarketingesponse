#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Link DropBox
library(RStudioAMI)
linkDropbox()
install.packages(c("xgboost", "rpart"))

# Set working directory
setwd("/home/rstudio/Dropbox/Public/Springleaf")

# Install Packages
pkgs <- c("doSNOW", "ROCR", "xgboost", "foreach", "caret", "rpart")
install.packages(pkgs)

# Load libraries
library(caret)
# library(xgboost)

# Load data
load("combinedData.RData")

# Initiate Prepared Dataset
prepData <- combinedData[, c("ID", "target")]

# Remove ID and target variable from combinedData
combinedData <- combinedData[, setdiff(names(combinedData), c("ID", "target"))]

#----------------------------------------------------------------
# Categorical Variables
#----------------------------------------------------------------
# Extract data for categorical variables
train_char <- combinedData[, sapply(combinedData, is.character)]
combinedData <- combinedData[, setdiff(names(combinedData), names(train_char))]

# Missing Values
train_char[train_char==-1] <- NA
train_char[train_char==""] <- NA
train_char[train_char=="[]"] <- NA

# Separate date variables from character variables
train_date <- train_char[, grep("JAN1|FEB1|MAR1", train_char), ]
train_char <- train_char[, !colnames(train_char) %in% colnames(train_date)]

# Convert character variables into factors
for(tmpVar in names(train_char)) {
  train_char[, tmpVar] <- factor(train_char[, tmpVar])
}

# Number of classes
nLevels <- sapply(train_char, nlevels)

# Remove variables with zero class
train_char <- train_char[, setdiff(names(train_char), names(nLevels[nLevels==0]))]

# Modify variables with one class
for(tmpVar in names(nLevels[nLevels==1])) {
  train_char[, tmpVar] <- ifelse(is.na(train_char[, tmpVar]), 1, 0)
}

# Correlation among numerical variables in train_char
corMat <- cor(train_char[, sapply(train_char, is.numeric)])

# Remove variables which are perfectly correlated with 'VAR_0008' in train_char
train_char <- train_char[, setdiff(names(train_char), names(which(corMat[-1, 1] == 1)))]

# Drop numerical variables from train_char and add in combinedData
combinedData <- cbind(combinedData, train_char[, sapply(train_char, is.numeric)])
train_char <- train_char[, !sapply(train_char, is.numeric)]

# Add final train_char to prepData
prepData <- cbind(prepData, train_char)

# Remove temporary variables to boost up RAM
rm(corMat)
rm(train_char)
rm(nLevels)
rm(tmpVar)
gc()

#----------------------------------------------------------------
# Date and Time Variables
#----------------------------------------------------------------
# Convert variables in date format
train_date <- sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date <- do.call(cbind.data.frame, train_date)

# Extract variables which are having time
train_time <- train_date[, colnames(train_date) %in% c("VAR_0204", "VAR_0217")]
train_time <- data.frame(sapply(train_time, function(x) strftime(x, "%H:%M:%S")))
train_hour <- as.data.frame(sapply(train_time, function(x) as.numeric(as.character(substr(x, 1, 2)))))
names(train_hour) <- paste0("HOUR_", names(train_hour))

# Drop "HOUR_VAR_0217" as only one unique value
train_hour[, "HOUR_VAR_0217"] <- NULL

# Add final train_hour to combinedData
combinedData <- cbind(combinedData, train_hour)

# Extract year and months from date variables
dateVars <- names(train_date)
for(tmpVar in dateVars) {
  combinedData[, paste0("YR_", tmpVar)] <- as.numeric(format(train_date[, tmpVar], "%Y"))
  combinedData[, paste0("MTH_", tmpVar)] <- as.numeric(format(train_date[, tmpVar], "%m"))
  combinedData[, paste0("DT_", tmpVar)] <- as.numeric(format(train_date[, tmpVar], "%d"))
  combinedData[, paste0("DIFF_", tmpVar)] <- as.numeric(difftime(train_date[, tmpVar], min(train_date[, tmpVar], na.rm=TRUE), units = "days"))
}

# Compute difference between two dates
for(i in 1:(length(dateVars) - 1)) {
  baseVar <- dateVars[i]
  refVars <- dateVars[-c(1:i)]
  for(tmpVar in refVars) {
    varName <- paste0("DAY_DIFF", gsub("VAR", "", tmpVar), gsub("VAR", "", baseVar))
    combinedData[, varName] <- as.numeric(difftime(train_date[, tmpVar], train_date[, baseVar], units = "days"))
  }
}

# Remove temporary variables to boost up RAM
rm(baseVar)
rm(dateVars)
rm(i)
rm(refVars)
rm(tmpVar)
rm(train_date)
rm(train_hour)
rm(train_time)
rm(varName)
gc()

#----------------------------------------------------------------
# Numerical and Logical Variables
#----------------------------------------------------------------
# Replace -99999 and -1 with NA
for(tmpVar in names(combinedData)) {
  if(length(which(combinedData[, tmpVar] == -99999)) > 0) combinedData[which(combinedData[, tmpVar] == -99999), tmpVar] <- NA
  if(length(which(combinedData[, tmpVar] == -1)) > 0) combinedData[which(combinedData[, tmpVar] == -1), tmpVar] <- NA
}
# combinedData[combinedData==-99999] <- NA
# combinedData[combinedData==-1] <- NA
gc()

# Number of unique values
nUnique <- sapply(combinedData, function(x) length(unique(na.omit(x))))

# Drop variables with nUnique equal to zero
combinedData <- combinedData[, setdiff(names(combinedData), names(nUnique[nUnique == 0]))]
rm(nUnique)

# Maximum and Minimum Values
maxValues <- sapply(combinedData, max, na.rm=TRUE)
minValues <- sapply(combinedData, min, na.rm=TRUE)

# Variables with maximum values equal to 99
length(maxValues[maxValues==99])
max_99 <- combinedData[, names(maxValues[maxValues==99])]
max_99[max_99==99] <- NA
max_99[max_99==98] <- NA
max_99[max_99==97] <- NA
max_99[max_99==96] <- NA
max_99[max_99==95] <- NA
max_99[max_99==94] <- NA
prepData <- cbind(prepData, max_99)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_99))]
rm(max_99)
gc()

# Variables with maximum values equal to 999
length(maxValues[maxValues==999])
max_999 <- combinedData[, names(maxValues[maxValues==999])]
max_999[max_999==999] <- NA
max_999[max_999==998] <- NA
max_999[max_999==997] <- NA
max_999[max_999==996] <- NA
max_999[max_999==995] <- NA
max_999[max_999==994] <- NA
max_999[max_999==990] <- NA
prepData <- cbind(prepData, max_999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_999))]
rm(max_999)
gc()

# Variables with maximum values equal to 9998
length(maxValues[maxValues==9998])
max_9998 <- combinedData[, names(maxValues[maxValues==9998])]
max_9998 <- data.frame(max_9998)
names(max_9998) <- names(maxValues[maxValues==9998])
max_9998[max_9998==9998] <- NA
prepData <- cbind(prepData, max_9998)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_9998))]
rm(max_9998)
gc()

# Variables with maximum values equal to 9999
length(maxValues[maxValues==9999])
max_9999 <- combinedData[, names(maxValues[maxValues==9999])]
max_9999[max_9999==9999] <- NA
max_9999[max_9999==9998] <- NA
max_9999[max_9999==9997] <- NA
max_9999[max_9999==9996] <- NA
max_9999[max_9999==9995] <- NA
max_9999[max_9999==9994] <- NA
max_9999[max_9999==9990] <- NA
prepData <- cbind(prepData, max_9999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_9999))]
rm(max_9999)
gc()

# Variables with maximum values equal to 99999
length(maxValues[maxValues==99999])
max_99999 <- combinedData[, names(maxValues[maxValues==99999])]
max_99999[max_99999==99999] <- NA
prepData <- cbind(prepData, max_99999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_99999))]
rm(max_99999)
gc()

# Variables with maximum values equal to 999999
length(maxValues[maxValues==999999])
max_999999 <- combinedData[, names(maxValues[maxValues==999999])]
max_999999[max_999999==999999] <- NA
max_999999[max_999999==999994] <- NA
prepData <- cbind(prepData, max_999999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_999999))]
rm(max_999999)
gc()

# Variables with maximum values equal to 9999999
length(maxValues[maxValues==9999999])
max_9999999 <- combinedData[, names(maxValues[maxValues==9999999])]
max_9999999 <- data.frame(max_9999999)
names(max_9999999) <- names(maxValues[maxValues==9999999])
max_9999999[max_9999999==9999999] <- NA
prepData <- cbind(prepData, max_9999999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_9999999))]
rm(max_9999999)
gc()

# Variables with maximum values equal to 999999999
length(maxValues[maxValues==999999999])
max_999999999 <- combinedData[, names(maxValues[maxValues==999999999])]
max_999999999[max_999999999==999999999] <- NA
max_999999999[max_999999999==999999998] <- NA
max_999999999[max_999999999==999999997] <- NA
max_999999999[max_999999999==999999996] <- NA
max_999999999[max_999999999==999999995] <- NA
max_999999999[max_999999999==999999994] <- NA
prepData <- cbind(prepData, max_999999999)
combinedData <- combinedData[, setdiff(names(combinedData), names(max_999999999))]
rm(max_999999999)
gc()

# Add remaining variables in combinedData to prepData
prepData <- cbind(prepData, combinedData)

# List of numerical and categorical variables
char_vars <- names(prepData[, sapply(prepData, is.factor)])
numr_vars <- setdiff(names(prepData[, sapply(prepData, is.numeric)]), c("ID", "target"))

# Modify class of numeric variables
for(tmpVar in numr_vars) {
  prepData[, tmpVar] <- as.numeric(prepData[, tmpVar])
}

#----------------------------------------------------------------
# Again check data for redundant variables
#----------------------------------------------------------------
# Number of unique values
nUnique <- sapply(prepData, function(x) length(unique(na.omit(x))))
prepData <- prepData[, setdiff(names(prepData), names(nUnique[nUnique==1]))]
nUnique <- nUnique[nUnique!=1]
nUnique <- nUnique[!names(nUnique) %in% c("ID", "target")]

# Negative Values
numr_vars <- setdiff(names(prepData[, sapply(prepData, is.numeric)]), c("ID", "target"))
all_neg <- sapply(prepData[, numr_vars], function(x) all(na.omit(x) < 0))
for(tmpVar in names(which(all_neg))) {
  prepData[, tmpVar] <- -1 * prepData[, tmpVar]
}

#----------------------------------------------------------------
# Create difference of other year variables
#----------------------------------------------------------------
prepData[, "VAR_0531_YR"] <- as.numeric(substr(prepData[, "VAR_0531"], 1, 4))
prepData[, "VAR_0531_MTH"] <- as.numeric(substr(prepData[, "VAR_0531"], 5, 6))
prepData[, "VAR_0531"] <- NULL

prepData[, "DIFF_0332_0294"] <- prepData[, "VAR_0332"] - prepData[, "VAR_0294"]
prepData[, "DIFF_0332_0314"] <- prepData[, "VAR_0332"] - prepData[, "VAR_0314"]
prepData[, "DIFF_0332_0531"] <- prepData[, "VAR_0332"] - prepData[, "VAR_0531_YR"]
prepData[, "DIFF_0294_0314"] <- prepData[, "VAR_0294"] - prepData[, "VAR_0314"]
prepData[, "DIFF_0294_0531"] <- prepData[, "VAR_0294"] - prepData[, "VAR_0531_YR"]
prepData[, "DIFF_0314_0531"] <- prepData[, "VAR_0314"] - prepData[, "VAR_0531_YR"]
prepData[, "DIFF_0332_0531"] <- -1*prepData[, "DIFF_0332_0531"]
prepData[, "DIFF_0294_0531"] <- -1*prepData[, "DIFF_0294_0531"]
prepData[, "DIFF_0314_0531"] <- -1*prepData[, "DIFF_0314_0531"]

prepData[, "VAR_0332"] <- NULL
prepData[, "VAR_0294"] <- NULL
prepData[, "VAR_0314"] <- NULL
prepData[, "VAR_0531_YR"] <- NULL
# prepData[, "VAR_0212"] <- NULL
# prepData[, "VAR_0228"] <- NULL
# prepData[, "VAR_0227"] <- NULL
# prepData[, "VAR_0493"] <- NULL
# prepData[, "VAR_0404"] <- NULL
# prepData[, "VAR_0200"] <- NULL

#----------------------------------------------------------------
# Create log and sqrt variables
#----------------------------------------------------------------
numr_vars <- setdiff(names(prepData[, sapply(prepData, is.numeric)]), c("ID", "target"))
for(tmpVar in numr_vars) {
  prepData[, paste0("SQRT_", tmpVar)] <- sqrt(prepData[, tmpVar])
  if(sum(is.infinite(prepData[, paste0("SQRT_", tmpVar)])) > 0)
    prepData[is.infinite(prepData[, paste0("SQRT_", tmpVar)]), paste0("SQRT_", tmpVar)] <- NA
  
  prepData[, paste0("LOG_", tmpVar)] <- log(prepData[, tmpVar])
  if(sum(is.infinite(prepData[, paste0("LOG_", tmpVar)])) > 0)
    prepData[is.infinite(prepData[, paste0("LOG_", tmpVar)]), paste0("LOG_", tmpVar)] <- NA  
}
gc()

rm(list=setdiff(ls(all=TRUE), "prepData"))
gc()

