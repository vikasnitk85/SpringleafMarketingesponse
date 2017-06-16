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
# List of independent variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- c('VAR_0795', 'VAR_0070', 'VAR_0071', 'VAR_0004', 'VAR_1329', 'VAR_0810', 'VAR_0241',
  'VAR_0073_YR', 'VAR_0003', 'VAR_0886', 'VAR_0970', 'VAR_0088', 'VAR_0087', 'VAR_1127', 'VAR_0881',
  'VAR_0807', 'VAR_0137', 'VAR_0505', 'VAR_1136', 'VAR_0855', 'VAR_0540', 'VAR_1132', 'VAR_0707',
  'VAR_0002', 'VAR_0227', 'VAR_0121', 'VAR_0613', 'VAR_0544', 'VAR_1747', 'VAR_1022', 'VAR_0853',
  'VAR_0503', 'VAR_0273', 'VAR_0968', 'VAR_0885', 'VAR_0880', 'VAR_0870', 'VAR_1399', 'VAR_1791',
  'VAR_0884', 'VAR_0715', 'VAR_1390', 'VAR_0806', 'VAR_0712', 'VAR_1128', 'VAR_1748', 'VAR_0282',
  'VAR_0871', 'VAR_0555', 'VAR_0896')

dtrain <- dtrain[, c("target", numVars)]
dval1 <- dval1[, c("target", numVars)]
dval2 <- dval2[, c("target", numVars)]
scoreData <- scoreData[, c("ID", numVars)]
gc()


dropVars <- c("VAR_0806")
dtrain <- dtrain[, setdiff(names(dtrain), dropVars)]
numVars <- setdiff(names(dtrain), "target")
length(numVars)

tmpVar <- numVars[36]
sort(abs(cor(dtrain[, tmpVar], dtrain[, setdiff(numVars, tmpVar)])[1, ]))

feature.names <- setdiff(names(dtrain), "target")
corMat <- cor(dtrain[, feature.names])
corMat[upper.tri(corMat)]

#----------------------------------------------------------------------------
dtrain <- xgb.DMatrix(data.matrix(dtrain[, feature.names]), label=dtrain$target)
dval1 <- xgb.DMatrix(data.matrix(dval1[, feature.names]), label=dval1$target)
dval2 <- xgb.DMatrix(data.matrix(dval2[, feature.names]), label=dval2$target)
watchlist <- list(train=dtrain, dval1=dval1, dval2=dval2)

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.01,
                max_depth           = 5,  # changed from default of 6
                subsample           = 0.5,
                # colsample_bytree    = 0.6,
                eval_metric         = "auc"
                # alpha = 0.01 
                # lambda = 1
                )

set.seed(2012)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000,
                    verbose             = 2, 
                    # early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

#[2499]  train-auc:0.885946      dval1-auc:0.772661      dval2-auc:0.769041

#----------------------------------------------------------------------------
submission <- data.frame(ID=scoreData$ID)
submission$target <- NA 
for (rows in split(1:nrow(scoreData), ceiling((1:nrow(scoreData))/10000))) {
    submission[rows, "target"] <- predict(clf, data.matrix(scoreData[rows, feature.names]))
}

write.csv(submission, "Models/Model_Development_12.csv", row.names=FALSE)

varImp <- xgb.importance(feature_names=feature.names, model=clf)
write.csv(varImp, "Models/varImp_Model_Development_12.csv", row.names=FALSE)

