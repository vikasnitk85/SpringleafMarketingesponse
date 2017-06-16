# Environment set-up
rm(list=ls(all=TRUE))
gc()

setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development/Data")
library(readr)
library(lsr)

#-------------------------------------------------------------------------------
# Generate R data object for training
# 1932 independent variables
#   - 1882 numeric variables
#   - 34 categorical variables
#   - 16 date variables
# 1 id variable
# 1 target variable
# 145,231 observations
#-------------------------------------------------------------------------------
train <- read_csv("train.csv")
tmpVars <- names(which(sapply(train, class)=="character"))
sort(sapply(train[, tmpVars], function(x) length(unique(na.omit(x)))))
sort(sapply(train[, tmpVars], function(x) sum(x == "")))

# Missing value treatment for categorical variables
missVars <- c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0043", "VAR_0044", "VAR_0196",
  "VAR_0200", "VAR_0202", "VAR_0216", "VAR_0222", "VAR_0226", "VAR_0229", "VAR_0230", "VAR_0232", "VAR_0236",
  "VAR_0237", "VAR_0239", "VAR_0274", "VAR_0283", "VAR_0305", "VAR_0325", "VAR_0342", "VAR_0352", "VAR_0353",
  "VAR_0354", "VAR_0404", "VAR_0466", "VAR_0467", "VAR_0493")

for(tmpVar in missVars) {
  train[which(train[, tmpVar] == ""), tmpVar] <- "MISSING"
}

# Modify class of character variables
catVars <- c(missVars, "VAR_0001", "VAR_0005", "VAR_1934")
for(tmpVar in catVars) {
  train[, tmpVar] <- factor(train[, tmpVar])
}
sort(sapply(train[, catVars], nlevels))

# Modify class of date variables
dateVars <- c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167",
  "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

for(tmpVar in dateVars) {
  train[which(train[, tmpVar] == ""), tmpVar] <- NA
  train[, tmpVar] <- substr(train[, tmpVar], start=1, stop=7)
  train[, tmpVar] <- as.Date(train[, tmpVar], "%d%b%y")
}

train <- data.frame(train)
save(train, file="train.RData")

#-------------------------------------------------------------------------------
# Generate R data object for test
# 1932 independent variables
#   - 1882 numeric variables
#   - 34 categorical variables
#   - 16 date variables
# 1 id variable
# 145,232 observations
#-------------------------------------------------------------------------------
test <- read_csv("test.csv")
tmpVars <- names(which(sapply(test, class)=="character"))
sort(sapply(test[, tmpVars], function(x) length(unique(na.omit(x)))))
sort(sapply(test[, tmpVars], function(x) sum(x == "")))

# Missing value treatment for categorical variables
missVars <- c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0043", "VAR_0044", "VAR_0196",
  "VAR_0200", "VAR_0202", "VAR_0216", "VAR_0222", "VAR_0226", "VAR_0229", "VAR_0230", "VAR_0232", "VAR_0236",
  "VAR_0237", "VAR_0239", "VAR_0274", "VAR_0283", "VAR_0305", "VAR_0325", "VAR_0342", "VAR_0352", "VAR_0353",
  "VAR_0354", "VAR_0404", "VAR_0466", "VAR_0467", "VAR_0493")

for(tmpVar in missVars) {
  test[which(test[, tmpVar] == ""), tmpVar] <- "MISSING"
}

# Modify class of character variables
catVars <- c(missVars, "VAR_0001", "VAR_0005", "VAR_1934")
for(tmpVar in catVars) {
  test[, tmpVar] <- factor(test[, tmpVar])
}
sort(sapply(test[, catVars], nlevels))

# Modify class of date variables
dateVars <- c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167",
  "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

for(tmpVar in dateVars) {
  test[which(test[, tmpVar] == ""), tmpVar] <- NA
  test[, tmpVar] <- substr(test[, tmpVar], start=1, stop=7)
  test[, tmpVar] <- as.Date(test[, tmpVar], "%d%b%y")
}

test <- data.frame(test)
save(test, file="test.RData")
