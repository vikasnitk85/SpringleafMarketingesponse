#----------------------------------------------------------------
# Environment Setup
#----------------------------------------------------------------
# Load Libraries
library(xgboost)
library(ROCR)

# Set working directory
setwd("/home/rstudio/Dropbox/Public/Springleaf")

# Prepare datasets
train <- prepData[!is.na(prepData[, "target"]), ]
test  <- prepData[ is.na(prepData[, "target"]), ]
rm(prepData)
gc()

# Split training dataset into train and validation
set.seed(1)
index <- sample(nrow(train), 96820)
temp1 <- train[ index, ]
dval2 <- train[-index, ]
gc()

set.seed(1)
index <- sample(nrow(temp1), 48410)
dtrain <- temp1[ index, ]
dval1  <- temp1[-index, ]

rm(temp1)
gc()

dtrain_target <- dtrain$target
dval1_target <- dval1$target
dval2_target <- dval2$target

# Prepare dataset for modeling
feature.names <- setdiff(names(dtrain), c("ID", "target"))
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target, missing = NA)
dval1 <- xgb.DMatrix(data.matrix(dval1[, feature.names]), label=dval1$target, missing = NA)
dval2 <- xgb.DMatrix(data.matrix(dval2[, feature.names]), label=dval2$target, missing = NA)
gc()

#----------------------------------------------------------------------------
# Model development - 1
#----------------------------------------------------------------------------
# Record start time
startTime <- Sys.time()

watchlist <- list(train=dtrain, dval1=dval1, dval2=dval2)
param1 <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.01,
              max_depth = 10,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf1 <- xgb.train(params             = param1,
                 data                = dtrain,
                 nrounds             = 100,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

#----------------------------------------------------------------------------
# Model development - 2
#----------------------------------------------------------------------------
param2 <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.01,
              max_depth = 8,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf2 <- xgb.train(params             = param2,
                 data                = dtrain,
                 nrounds             = 100,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

#----------------------------------------------------------------------------
# Model development - 3
#----------------------------------------------------------------------------
param3 <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.01,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf3 <- xgb.train(params             = param3,
                 data                = dtrain,
                 nrounds             = 100,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

#----------------------------------------------------------------------------
# Model development - 4
#----------------------------------------------------------------------------
param4 <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.01,
              max_depth = 4,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf4 <- xgb.train(params             = param4,
                 data                = dtrain,
                 nrounds             = 100,
                 verbose             = 2,
                 # early.stop.round  = 10,
                 watchlist           = watchlist,
                 maximize            = TRUE)

#----------------------------------------------------------------------------
# Predict dval1 and dval2
#----------------------------------------------------------------------------
dval1_pred <- data.frame(target=dval1_target)
dval1_pred[, "clf1"] <- predict(clf1, dval1, missing = NA)
dval1_pred[, "clf2"] <- predict(clf2, dval1, missing = NA)
dval1_pred[, "clf3"] <- predict(clf3, dval1, missing = NA)
dval1_pred[, "clf4"] <- predict(clf4, dval1, missing = NA)

dval2_pred <- data.frame(target=dval2_target)
dval2_pred[, "clf1"] <- predict(clf1, dval2, missing = NA)
dval2_pred[, "clf2"] <- predict(clf2, dval2, missing = NA)
dval2_pred[, "clf3"] <- predict(clf3, dval2, missing = NA)
dval2_pred[, "clf4"] <- predict(clf4, dval2, missing = NA)

feature.names <- setdiff(names(dval1_pred), c("target"))
dval1_pred <- xgb.DMatrix(data.matrix(dval1_pred[, feature.names]), label=dval1_pred$target, missing = NA)
dval2_pred <- xgb.DMatrix(data.matrix(dval2_pred[, feature.names]), label=dval2_pred$target, missing = NA)
gc()

#----------------------------------------------------------------------------
# Model development - 1
#----------------------------------------------------------------------------
watchlist <- list(train=dval1_pred, val=dval2_pred)
param <- list(objective = "binary:logistic",
              missing = NA,
              eta = 0.01,
              max_depth = 2,
              subsample = 0.5,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

set.seed(2012)
clf <- xgb.train(params              = param,
                 data                = dtrain,
                 nrounds             = 100,
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
    
}
write.csv(submission, "Model_Research_35.csv", row.names=FALSE)

# Record end time
endTime <- Sys.time()
print(difftime(endTime, startTime))
sink()
