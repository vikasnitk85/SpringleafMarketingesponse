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

# Date Variables
dateVars <- c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159",
  "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178",
  "VAR_0179", "VAR_0204", "VAR_0217")
for(tmpVar in dateVars) {
  rawData[which(rawData[, tmpVar] == ""), tmpVar] <- NA
  rawData[, tmpVar] <- substr(rawData[, tmpVar], start=1, stop=7)
  rawData[, tmpVar] <- as.Date(rawData[, tmpVar], "%d%b%y")
}
str(rawData[, dateVars])

# Modify binary variables
binaryVars <- c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0043",
  "VAR_0044", "VAR_0196", "VAR_0202", "VAR_0216", "VAR_0222", "VAR_0229", "VAR_0239")
for(tmpVar in binaryVars) {
  rawData[, tmpVar] <- ifelse(rawData[, tmpVar] == "", 1, 0)
}
str(rawData[, binaryVars])

# Modify categorical variables
catVars <- c("VAR_0001", "VAR_0005", "VAR_0200", "VAR_0226", "VAR_0230", "VAR_0232",
  "VAR_0236", "VAR_0237", "VAR_0274", "VAR_0283", "VAR_0305", "VAR_0325", "VAR_0342",
  "VAR_0352", "VAR_0353", "VAR_0354", "VAR_0404", "VAR_0466", "VAR_0467", "VAR_0493", 
  "VAR_1934")
for(tmpVar in catVars) {
  rawData[which(rawData[, tmpVar] == ""), tmpVar] <- NA
}

for(tmpVar in catVars) {
  if(length(which(rawData[, tmpVar] == "-1")) > 0) rawData[which(rawData[, tmpVar] == "-1"), tmpVar] <- NA
}

for(tmpVar in catVars) {
  rawData[, tmpVar] <- factor(rawData[, tmpVar])
}

str(rawData[, catVars])

rawData[, "VAR_0466"] <- as.numeric(rawData[, "VAR_0466"])
rawData[which(is.na(rawData[, "VAR_0466"])), "VAR_0466"] <- 0

# Generate features from date variables
for(tmpVar in dateVars) {
  rawData[, paste0(tmpVar, "_YR")] <- as.numeric(format(rawData[, tmpVar], "%Y"))
  rawData[, paste0(tmpVar, "_MTH")] <- as.numeric(format(rawData[, tmpVar], "%m"))
  rawData[, paste0(tmpVar, "_DT")] <- as.numeric(format(rawData[, tmpVar], "%d"))
}

# Variables with 0 or 1 observations
nMiss <- sapply(rawData, function(x) sum(is.na(x)))
uniqueObs <- sapply(rawData, function(x) length(unique(na.omit(x))))

summary(rawData[, names(uniqueObs[uniqueObs == 1])])
str(rawData[, names(uniqueObs[uniqueObs == 1])])

# Variables to drop
dropVars <- NULL
dropVars <- c(dropVars, binaryVars[-1])
dropVars <- c(dropVars, dateVars)
dropVars <- c(dropVars, names(uniqueObs[uniqueObs == 0])) # All observations are missing
dropVars <- c(dropVars, names(uniqueObs[uniqueObs == 1])) # Only 1 observations

rawData <- rawData[, setdiff(names(rawData), dropVars)]
rawData[, "VAR_0526"] <- NULL
rawData[, "VAR_0529"] <- NULL

indVars <- setdiff(names(rawData), c("ID", "target"))
numVars <- names(which(sapply(rawData[, indVars], class) != "factor"))
corMat <- cor(rawData[, numVars], use="pairwise.complete.obs")


# Save data
save(rawData, file="Data/rawData.RData")

#----------------------------------------------------------------------------
# Split Data
#----------------------------------------------------------------------------
set.seed(1)
index <- sample(nrow(rawData), 80000)

dtrain <- rawData[ index, ]
dval   <- rawData[-index, ]

# Remove unnecessary components
rm(index)
rm(rawData)
gc()

# Save data
save(dtrain, dval, file="Data/rawData_split.RData")

#----------------------------------------------------------------------------
# Data Preparation for scoring data
#----------------------------------------------------------------------------
scoreData <- read_csv("Data/test.csv")
gc()

# Date Variables
dateVars <- c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159",
  "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178",
  "VAR_0179", "VAR_0204", "VAR_0217")
for(tmpVar in dateVars) {
  scoreData[which(scoreData[, tmpVar] == ""), tmpVar] <- NA
  scoreData[, tmpVar] <- substr(scoreData[, tmpVar], start=1, stop=7)
  scoreData[, tmpVar] <- as.Date(scoreData[, tmpVar], "%d%b%y")
}
str(scoreData[, dateVars])

# Modify binary variables
binaryVars <- c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0043",
  "VAR_0044", "VAR_0196", "VAR_0202", "VAR_0216", "VAR_0222", "VAR_0229", "VAR_0239")
for(tmpVar in binaryVars) {
  scoreData[, tmpVar] <- ifelse(scoreData[, tmpVar] == "", 1, 0)
}
str(scoreData[, binaryVars])

# Modify categorical variables
catVars <- c("VAR_0001", "VAR_0005", "VAR_0200", "VAR_0226", "VAR_0230", "VAR_0232",
  "VAR_0236", "VAR_0237", "VAR_0274", "VAR_0283", "VAR_0305", "VAR_0325", "VAR_0342",
  "VAR_0352", "VAR_0353", "VAR_0354", "VAR_0404", "VAR_0466", "VAR_0467", "VAR_0493", 
  "VAR_1934")
for(tmpVar in catVars) {
  scoreData[which(scoreData[, tmpVar] == ""), tmpVar] <- NA
}

for(tmpVar in catVars) {
  if(length(which(scoreData[, tmpVar] == "-1")) > 0) scoreData[which(scoreData[, tmpVar] == "-1"), tmpVar] <- NA
}

for(tmpVar in catVars) {
  scoreData[, tmpVar] <- factor(scoreData[, tmpVar])
}

str(scoreData[, catVars])

scoreData[, "VAR_0466"] <- as.numeric(scoreData[, "VAR_0466"])
scoreData[which(is.na(scoreData[, "VAR_0466"])), "VAR_0466"] <- 0

# Generate features from date variables
for(tmpVar in dateVars) {
  scoreData[, paste0(tmpVar, "_YR")] <- as.numeric(format(scoreData[, tmpVar], "%Y"))
  scoreData[, paste0(tmpVar, "_MTH")] <- as.numeric(format(scoreData[, tmpVar], "%m"))
  scoreData[, paste0(tmpVar, "_DT")] <- as.numeric(format(scoreData[, tmpVar], "%d"))
}

# Variables with 0 or 1 observations
nMiss <- sapply(scoreData, function(x) sum(is.na(x)))
uniqueObs <- sapply(scoreData, function(x) length(unique(na.omit(x))))

summary(scoreData[, names(uniqueObs[uniqueObs == 1])])
str(scoreData[, names(uniqueObs[uniqueObs == 1])])

# Variables to drop
dropVars <- NULL
dropVars <- c(dropVars, binaryVars[-1])
dropVars <- c(dropVars, dateVars)
dropVars <- c(dropVars, names(uniqueObs[uniqueObs == 0])) # All observations are missing
dropVars <- c(dropVars, names(uniqueObs[uniqueObs == 1])) # Only 1 observations

scoreData <- scoreData[, setdiff(names(scoreData), dropVars)]

# Save data
save(scoreData, file="Data/scoreData.RData")
