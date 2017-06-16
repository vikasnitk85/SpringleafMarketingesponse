# Environment set-up
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(xgboost)
library(ROCR)

# Load Data
load(file="Data/rawData_split.RData")
load(file="Data/scoreData.RData")

#----------------------------------------------------------------------------
# 
#----------------------------------------------------------------------------
feature.names <- c('VAR_0089', 'VAR_1029', 'VAR_0078', 'VAR_1791', 'VAR_0073_YR', 'VAR_0795',
	'VAR_0970', 'VAR_1271', 'VAR_0503', 'VAR_0120', 'VAR_0054', 'VAR_0885', 'VAR_0868', 
	'VAR_1041', 'VAR_0508', 'VAR_0576', 'VAR_0712', 'VAR_0475', 'VAR_0004')

dtrain <- dtrain[, c("target", feature.names)]
dval1 <- dval1[, c("target", feature.names)]
dval2 <- dval2[, c("target", feature.names)]
scoreData <- scoreData[, c("ID", feature.names)]
gc()

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target)
dval1 <- xgb.DMatrix(data.matrix(dval1[, feature.names]), label=dval1$target)
dval2 <- xgb.DMatrix(data.matrix(dval2[, feature.names]), label=dval2$target)

watchlist <- list(train=dtrain, dval1=dval1, dval2=dval2)
param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.01,
                max_depth           = 4,  # changed from default of 6
                subsample           = 0.5,
                # colsample_bytree    = 0.6,
                eval_metric         = "auc"
                # alpha = 0.01 
                # lambda = 1
                )

set.seed(2012)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#----------------------------------------------------------------------------
crossValOut <- data.frame(target=outTrain$target)
crossValOut$predicted <- NA 
for (rows in split(1:nrow(outTrain), ceiling((1:nrow(outTrain))/10000))) {
    crossValOut[rows, "predicted"] <- predict(clf, data.matrix(outTrain[rows, feature.names]))
}
str(crossValOut)
pred <- prediction(crossValOut$predicted, outTrain$target)
tmpAuc <- performance(pred, "auc")
as.numeric(tmpAuc@y.values)

#----------------------------------------------------------------------------
submission <- data.frame(ID=test$ID)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(test[rows, feature.names]))
}

write.csv(submission, "Models/Model_Development_4.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Models/varImp_Model_Development_4.csv", row.names=FALSE)

