# Remove All Objects
rm(list=ls(all=TRUE))
gc()

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(readr)

#----------------------------------------------------------------------------
# Data Preparation for training data
#----------------------------------------------------------------------------
rawData <- read_csv("Data/train.csv")
gc()

# Data classes
table(sapply(rawData, class))

# Variables which need to be removed from data
rmVars1 <- c("VAR_0205", "VAR_0207", "VAR_0213", "VAR_0214", "VAR_0840")       # All observations are missing
rmVars2 <- c("VAR_0212")                                                       # Looks like an ID variable

# Date and categorical variables
dateVars <- c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159",
  "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178",
  "VAR_0179", "VAR_0204", "VAR_0217")
catVars <- c("VAR_0001", "VAR_0005", "VAR_0200", "VAR_0226", "VAR_0230", "VAR_0232",
  "VAR_0236", "VAR_0237", "VAR_0274", "VAR_0283", "VAR_0305", "VAR_0325", "VAR_0342",
  "VAR_0352", "VAR_0353", "VAR_0354", "VAR_0404", "VAR_0466", "VAR_0467", "VAR_0493", 
  "VAR_1934")
binaryVars <- c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0043",
  "VAR_0044", "VAR_0196", "VAR_0202", "VAR_0216", "VAR_0222", "VAR_0229", "VAR_0239")

# Modify class of date variables
for(tmpVar in dateVars) {
  rawData[which(rawData[, tmpVar] == ""), tmpVar] <- NA
  rawData[, tmpVar] <- substr(rawData[, tmpVar], start=1, stop=7)
  rawData[, tmpVar] <- as.Date(rawData[, tmpVar], "%d%b%y")
}

# Modify binary variables
for(tmpVar in binaryVars) {
  rawData[, tmpVar] <- ifelse(rawData[, tmpVar] == "", 1, 0)
}

# Modify categorical variables
for(tmpVar in catVars) {
  rawData[which(rawData[, tmpVar] == ""), tmpVar] <- "MISSING"
  rawData[, tmpVar] <- factor(rawData[, tmpVar])
}

# Remove Variables
rawData <- rawData[, setdiff(names(rawData), c(rmVars1, rmVars2))]

# Generate features from date variables
for(tmpVar in dateVars) {
  rawData[, paste0(tmpVar, "_YR")] <- as.numeric(format(rawData[, tmpVar], "%Y"))
  rawData[, paste0(tmpVar, "_MTH")] <- as.numeric(format(rawData[, tmpVar], "%m"))
  rawData[, paste0(tmpVar, "_DT")] <- as.numeric(format(rawData[, tmpVar], "%d"))
}

# Remove date variables
rawData <- rawData[, setdiff(names(rawData), dateVars)]

# Missing Value Treatment
rawData[is.na(rawData)] <- -1
gc()

# Data classes
table(sapply(rawData, class))

# Remove variables with zero standard deviation
numVars <- names(which(sapply(rawData, class) != "factor"))
numVars <- setdiff(numVars, c("ID", "target"))
zeroSD <- names(which(sapply(rawData[, numVars], function(x) sd(x, na.rm=TRUE)) == 0))
rawData <- rawData[, setdiff(names(rawData), zeroSD)]

# Save data
save(rawData, file="Data/rawData.RData")

#----------------------------------------------------------------------------
# Split Data
#----------------------------------------------------------------------------
set.seed(1)
index1 <- sample(nrow(rawData), 96820)

tmpData <- rawData[ index1, ]
dtrain <- rawData[-index1, ]

set.seed(1)
index2 <- sample(nrow(tmpData), 48410)
dval1 <- tmpData[ index2, ]
dval2 <- tmpData[-index2, ]
gc()

# Remove unnecessary components
rm(index1)
rm(index2)
rm(rawData)
rm(tmpData)
gc()

# Save data
save(dtrain, dval1, dval2, file="Data/rawData_split.RData")

#----------------------------------------------------------------------------
# Data Preparation for scoring data
#----------------------------------------------------------------------------
scoreData <- read_csv("Data/test.csv")
gc()

# Data classes
table(sapply(scoreData, class))

# Modify class of date variables
for(tmpVar in dateVars) {
  scoreData[which(scoreData[, tmpVar] == ""), tmpVar] <- NA
  scoreData[, tmpVar] <- substr(scoreData[, tmpVar], start=1, stop=7)
  scoreData[, tmpVar] <- as.Date(scoreData[, tmpVar], "%d%b%y")
}

# Modify binary variables
for(tmpVar in binaryVars) {
  scoreData[, tmpVar] <- ifelse(scoreData[, tmpVar] == "", 1, 0)
}

# Modify categorical variables
for(tmpVar in catVars) {
  scoreData[which(scoreData[, tmpVar] == ""), tmpVar] <- "MISSING"
  scoreData[, tmpVar] <- factor(scoreData[, tmpVar])
}

# Remove Variables
scoreData <- scoreData[, setdiff(names(scoreData), c(rmVars1, rmVars2))]

# Generate features from date variables
for(tmpVar in dateVars) {
  scoreData[, paste0(tmpVar, "_YR")] <- as.numeric(format(scoreData[, tmpVar], "%Y"))
  scoreData[, paste0(tmpVar, "_MTH")] <- as.numeric(format(scoreData[, tmpVar], "%m"))
  scoreData[, paste0(tmpVar, "_DT")] <- as.numeric(format(scoreData[, tmpVar], "%d"))
}

# Remove date variables
scoreData <- scoreData[, setdiff(names(scoreData), dateVars)]

# Data classes
table(sapply(scoreData, class))

# Missing Value Treatment
scoreData[is.na(scoreData)] <- -1
gc()

# Remove variables with zero standard deviation
scoreData <- scoreData[, setdiff(names(scoreData), zeroSD)]

# Save data
save(scoreData, file="Data/scoreData.RData")
