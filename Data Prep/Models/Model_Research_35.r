#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
# Load Libraries
library(xgboost)
library(ROCR)

# Set working directory
setwd("/home/rstudio/Dropbox/Public/Springleaf")

# Remove variables for which p-Value of statistical test is greater than 0.05
dropVars <- names(which(retStatOut > 0.05))
prepData <- prepData[, setdiff(names(prepData), dropVars)]

# Remove variables for which IV is less than 0.02
dropVars <- names(which(retIV < 0.02))
prepData <- prepData[, setdiff(names(prepData), dropVars)]

# rm(list=setdiff(ls(all=TRUE), "prepData"))
# gc()

# Prepare datasets
train <- prepData[!is.na(prepData[, "target"]), ]
test <- prepData[is.na(prepData[, "target"]), ]
rm(prepData)
gc()

# Split training dataset into train and validation
set.seed(1)
index <- sample(nrow(train), 80000)
dtrain <- train[ index, ]
dval   <- train[-index, ]
gc()

# Prepare dataset for modeling
feature.names <- setdiff(names(dtrain), c("ID", "target"))
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval <- xgb.DMatrix(data.matrix(dval[, feature.names]), label=dval$target, missing = NA)
gc()

#----------------------------------------------------------------------------
# Model development
#----------------------------------------------------------------------------
# Start recording console outputs
sink(file="Model_Research_35.txt")

# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval=dval)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.001,
              max_depth = 10,
              subsample = 0.5,
              colsample_bytree = 1,
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

# Save model specs
# save(clf, file="Model_Research_35.RData")

# Score data
submission <- data.frame(ID=test$ID)
submission$target <- NA
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(test[rows, feature.names]), missing = NA)
}
write.csv(submission, "Model_Research_35.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
