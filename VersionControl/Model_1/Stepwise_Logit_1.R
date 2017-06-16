#-------------------------------------------------------------------------------
# Environment set-up
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

# Set working directory
setwd("D:/Vikas_Agrawal/Education/Kaggle/Springleaf Marketing Response/Development")

# Load Libraries
library(car)
library(ROCR)

# Load Data
load(file="Data/rawData_split.RData")

# List of independent variables
Variables <- setdiff(names(dtrain), c("ID", "target"))
numVars <- sort(names(which(sapply(dtrain[, Variables], class) != "factor")))

#-------------------------------------------------------------------------------
# Function for logit model development
#-------------------------------------------------------------------------------
logitDevlopment <- function(varList) {
  logitForm <- as.formula(sprintf("%s ~ . ", "target"))
  tmpData <- dtrain[, c("target", varList)]

  # Build initial model
  logitFit <- try(glm(logitForm, data=tmpData, family=binomial, control=list(maxit=50)))
  if(!("try-error" %in% class(logitFit))) {
    colVars <- rownames(alias(logitFit)$Complete)
    if(!is.null(colVars)) {
      logitFit <- try(glm(logitForm, data=tmpData[, setdiff(names(tmpData), colVars)], family=binomial, control=list(maxit=50)))
    }
  }

  # Model performance
  if(!("try-error" %in% class(logitFit))) {
    dval1[, "prediction"] <- predict(logitFit, dval1[, c("target", varList)], type="response")
    pred <- prediction(dval1[, "prediction"], dval1[, "target"])
    tmpAuc <- performance(pred, "auc")
    out1 <- as.numeric(tmpAuc@y.values)

    dval2[, "prediction"] <- predict(logitFit, dval2[, c("target", varList)], type="response")
    pred <- prediction(dval2[, "prediction"], dval2[, "target"])
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
selectedVars <- c("VAR_0089")
for(i in 11:20) {
    cat(i, "\n")
    selectedVars <- unique(selectedVars)
    numVars <- setdiff(numVars, selectedVars)
    corMat <- abs(cor(dtrain[, selectedVars[length(selectedVars)]], dtrain[, numVars])[1, ])
    highCor <- names(corMat[corMat > 0.9])
    numVars <- setdiff(numVars, highCor)
    cat("High correlated variables - ", length(highCor), "\n")

    Outputs <- matrix(NA, nrow=length(numVars), ncol=3)
    Outputs <- data.frame(Outputs)
    names(Outputs) <- c("Variable", "AUC_VAL1", "AUC_VAL2")
    Outputs[, 1] <- numVars

    for(varList in numVars) {
      tmpOut <- logitDevlopment(c(selectedVars, varList))
      Outputs[which(numVars %in% varList), -1] <- tmpOut
    }
    endTime <- Sys.time()

    Outputs$Average <- c(Outputs$AUC_VAL1 + Outputs$AUC_VAL2) / 2
    Outputs <- Outputs[order(Outputs$Average, decreasing=TRUE), ]
    selectedVars <- c(selectedVars, Outputs$Variable[1])
    print(Outputs[1, ])
    write.csv(Outputs, paste0("Models/logit_1/Outputs_", i, ".csv"), row.names=FALSE)
}

