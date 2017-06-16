#----------------------------------------------------------------------------
# Environment set-up
#----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(xgboost)
library(ROCR)
library(caret)

# Load Data
load(file="Data/rawData_split.RData")

# List of independent variables
Variables <- setdiff(names(dtrain), c("ID", "target"))

#----------------------------------------------------------------------------
# Create xgb matrix
#----------------------------------------------------------------------------
# dtrain <- xgb.DMatrix(data.matrix(dtrain[, Variables]), label=dtrain$target)
# dval1 <- xgb.DMatrix(data.matrix(dval1[, Variables]), label=dval1$target)
# dval2 <- xgb.DMatrix(data.matrix(dval2[, Variables]), label=dval2$target)
gc()

#----------------------------------------------------------------------------
# Parameter Tuning
#----------------------------------------------------------------------------
# watchlist <- list(train=dtrain, validation1=dval1)
param <- list(objective = "binary:logistic",
              booster = "gbtree",
              eta = 1,
              gamma = 1,
              max_depth = 2,
              min_child_weight = 50,
              subsample = 1,
              colsample_bytree = 1,
              eval_metric = "auc"
              )

evalFun <- function(tmpVars) {
  set.seed(123)
  modelFit <- try(xgboost(data = data.matrix(dtrain[, tmpVars]),
              label = dtrain$target,
              params = param,
              nrounds = 1,
              verbose = 0,
              print.every.n = 1,
              maximize = TRUE))

  if(class(modelFit) != "try-error") {
    dval1[, "predicted"] <- predict(modelFit, data.matrix(dval1[, tmpVars]))
    pred <- prediction(dval1[, "predicted"], dval1[, "target"])
    tmpAuc <- performance(pred, "auc")
    out1 <- as.numeric(tmpAuc@y.values)

    dval2[, "predicted"] <- predict(modelFit, data.matrix(dval2[, tmpVars]))
    pred <- prediction(dval2[, "predicted"], dval2[, "target"])
    tmpAuc <- performance(pred, "auc")
    out2 <- as.numeric(tmpAuc@y.values)
  } else {
    out1 <- NA
    out2 <- NA
  }

  return(c(out1, out2))
}


#-------------------------------------------------------------------------------
# Selecting first variable
#-------------------------------------------------------------------------------
Outputs <- matrix(NA, nrow=length(Variables), ncol=3)
Outputs <- data.frame(Outputs)
names(Outputs) <- c("Variable", "AUC_VAL1", "AUC_VAL2")
Outputs[, 1] <- Variables

startTime <- Sys.time()
for(tmpVars in Variables) {
  tmpOut <- evalFun(tmpVars)
  Outputs[which(Variables %in% tmpVars), -1] <- tmpOut
  print(Outputs[which(Variables %in% tmpVars), ])
}
endTime <- Sys.time()
difftime(endTime, startTime)

Outputs[order(Outputs$AUC_VAL1, decreasing=TRUE), "Variable"][1]
write.csv(Outputs, "Models/xgboost/Outputs_0.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
# Selecting second variable
#-------------------------------------------------------------------------------
selectedVars <- c("VAR_0089", "VAR_1329", "VAR_0073_YR", "VAR_0079")
Variables <- setdiff(Variables, selectedVars)

Outputs <- matrix(NA, nrow=length(Variables), ncol=3)
Outputs <- data.frame(Outputs)
names(Outputs) <- c("Variable", "AUC_VAL1", "AUC_VAL2")
Outputs[, 1] <- Variables

startTime <- Sys.time()
for(tmpVars in Variables) {
  tmpOut <- evalFun(c(selectedVars, tmpVars))
  Outputs[which(Variables %in% tmpVars), -1] <- tmpOut
  print(Outputs[which(Variables %in% tmpVars), ])
}
endTime <- Sys.time()
difftime(endTime, startTime)

Outputs[order(Outputs$AUC_VAL1, decreasing=TRUE), ]
write.csv(Outputs, "Models/xgboost/Outputs_3.csv", row.names=FALSE)




# dtrain$target <- as.numeric(dtrain$target)



# Variable importance
varImp <- xgb.importance(feature_names=Variables, model=modelFit)
varImp

numVars <- names(which(sapply(dval2[, Variables], class) != "factor"))
corMat <- cor(dval2[, numVars], use="pairwise.complete.obs")

cor(dval2[, c("VAR_0008", "VAR_0009", "VAR_0010", "VAR_0011", "VAR_0012", "VAR_0018", "VAR_0019", "")])


corAna <- list()

for(tmpVar in varImp[, Feature]) {
  if(class(dval2[, tmpVar]) != "factor") {
    tmpCor <- cor(dval2[, tmpVar], dval2[, setdiff(numVars, tmpVar)], use="pairwise.complete.obs")[1, ]
    cat(which(varImp[, Feature] %in% tmpVar), "\n")
    corAna[[tmpVar]] <- head(sort(abs(tmpCor), decreasing=TRUE))
  }
}



