#----------------------------------------------------------------
# Code to Download Africa Soil Data from Google Drive
#----------------------------------------------------------------

#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
rm(list = ls(all = TRUE))
pkgs <- c("caret", "ROCR", "xgboost", "foreach")
install.packages(c("RCurl", "doSNOW", pkgs))
library(RCurl)

#----------------------------------------------------------------
# Create Folders
#----------------------------------------------------------------
dir.create(paste(getwd(), "Springleaf", sep = "/"))
dir.create(paste(getwd(), "Springleaf", "Development", sep = "/"))
dir.create(paste(getwd(), "Springleaf", "Development", "Data", sep = "/"))
dir.create(paste(getwd(), "Springleaf", "Development", "Model", sep = "/"))

#----------------------------------------------------------------
# Download Raw Data
#----------------------------------------------------------------
temporaryFile <- "~/Springleaf/Development/Data/rawData_split.RData"
dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/rawData_split.RData"
download.file(dataAddress, temporaryFile, method = "curl")

temporaryFile <- "~/Springleaf/Development/Data/rawData_split_1.RData"
dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/rawData_split_1.RData"
download.file(dataAddress, temporaryFile, method = "curl")

temporaryFile <- "~/Springleaf/Development/Data/scoreData.RData"
dataAddress <- "https://dl.dropboxusercontent.com/u/100462810/Springleaf/scoreData.RData"
download.file(dataAddress, temporaryFile, method = "curl")
