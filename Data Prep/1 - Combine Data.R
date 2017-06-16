#-------------------------------------------------------------------------------
# Environment Set-up
#-------------------------------------------------------------------------------
# Remove All Objects
rm(list=ls(all=TRUE))
gc()

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development/Data Prep")

# Load Libraries
library(readr)

# Read training data
train <- read_csv("train.csv")
gc()

# Read test data
test <- read_csv("test.csv")
test$target <- NA
test <- test[, names(train)]
gc()

# Combine train and test data
combinedData <- rbind(train, test)
rm(train)
rm(test)
gc()

# Save data
save(combinedData, file="combinedData.RData")
