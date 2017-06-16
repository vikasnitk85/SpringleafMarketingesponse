# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load functions for generating EDA summary
source("VersionControl/EDA/EDA_Summary_Functions.R")

# Load training data
load("Data/train.RData")

# define target variable
y <- "target"
train[, y] <- as.factor(train[, y])

# Remove variables with no data
missValue <- sapply(train, function(x) sum(is.na(x)))
noData <- names(which(missValue==nrow(train))) # 5 variables are having no data
train <- train[, !(names(train) %in% noData)]

# Variable lists
varList <- setdiff(names(train), c(y, "ID"))
catVars <- setdiff(names(which(sapply(train, class)=="factor")), y)
dateVars <- names(which(sapply(train, class)=="Date"))
numVars <- setdiff(names(train), c(y, "ID", catVars, dateVars))

# Remove variables with no deviation
trainSD <- sapply(train[, numVars], sd, na.rm=TRUE)
zeroVar <- names(which(trainSD == 0))          # 38 variables are not having any variation in the data

train <- train[, !(names(train) %in% zeroVar)]
varList <- setdiff(varList, zeroVar)
numVars <- setdiff(numVars, zeroVar)

# Load libraries
library(rpart)
library(ggplot2)
library(lsr)
library(data.table)

#-------------------------------------------------------------------------------
# Generate Descriptive Statistics
#-------------------------------------------------------------------------------
startTime <- Sys.time()
retDesStat <- list()
for(x in c(numVars, catVars)) {
  print(paste0(x, " - ", which(c(numVars, catVars) %in% x)))
  retDesStat[[x]] <- desStats(x, train, nCats=20, freqCut=95/5, uniqueCut=10)
}
endTime <- Sys.time()
timeTaken1 <- difftime(endTime, startTime)
timeTaken1 # 2.45 mins

#-------------------------------------------------------------------------------
# Perform Correlation Analysis
#-------------------------------------------------------------------------------
startTime <- Sys.time()
retCorAna <- list()
for(x in numVars) {
  print(paste0(x, " - ", which(numVars %in% x)))
  retCorAna[[x]] <- corAnalysis(x, train, numVars, cutOff=0.7)
}
endTime <- Sys.time()
timeTaken2 <- difftime(endTime, startTime)
timeTaken2 # 2.420789 hours

#-------------------------------------------------------------------------------
# Perform Statistical Analysis
#-------------------------------------------------------------------------------
startTime <- Sys.time()
retStatOut <- list()
for(x in c(numVars, catVars)) {
  print(paste0(x, " - ", which(c(numVars, catVars) %in% x)))
  retStatOut[[x]] <- statsTest(x, y, train)
}
endTime <- Sys.time()
timeTaken3 <- difftime(endTime, startTime)
timeTaken3 # 4.89418 mins

#-------------------------------------------------------------------------------
# Generate IV for Numeric Variables using Decision Tree
#-------------------------------------------------------------------------------
startTime <- Sys.time()
retIVnum <- list()
for(x in numVars) {
  print(paste0(x, " - ", which(numVars %in% x)))
  tmpData <- try(ivNumDT(x, y, train, rcontrol=rpart.control(cp=10^-8, maxdepth=3)))
  if(class(tmpData) != "try-error") {
    tmpIvTab <- getIVtable(paste0(x, "_DT"), y, tmpData)
    retIVnum[[x]] <- sum(tmpIvTab[, "miv"])
  }
}
retIVnum <- unlist(retIVnum)
endTime <- Sys.time()
timeTaken4 <- difftime(endTime, startTime)
timeTaken4 #

#-------------------------------------------------------------------------------
# Generate IV for Categorical Variables
#-------------------------------------------------------------------------------
startTime <- Sys.time()
retIVcat <- NULL
for(x in catVars) {
  print(paste0(x, " - ", which(catVars %in% x)))
  tmpData <- train[, c(y, x)]
  if(class(tmpData) != "try-error") {
    tmpIvTab <- getIVtable(x, y, tmpData)
    retIVcat[[x]] <- sum(tmpIvTab[, "miv"])
  }
}
endTime <- Sys.time()
timeTaken5 <- difftime(endTime, startTime)
timeTaken5 #


save(retDesStat, retCorAna, retStatOut, retIVnum, retIVcat, file="EDA/EDA_SUMMARY_20150904.RData")


