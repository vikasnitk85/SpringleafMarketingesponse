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

  # Check VIF
  if(!("try-error" %in% class(logitFit)) & length(varList) > 1) {
    vifValues <- try(vif(logitFit))
    if(!(max(vifValues) <= 5) & class(vifValues) != "try-error") {
      while(max(vifValues) > 5 & class(vifValues) != "try-error") {
        tmpVars <- setdiff(names(vifValues), names(vifValues[which.max(vifValues)]))
        logitFit <- try(glm(logitForm, data=tmpData[, c("target", tmpVars)], family=binomial, control=list(maxit=50)))
        vifValues <- try(vif(logitFit))
      }
    }
  }

  # Check p-Values
  if(!("try-error" %in% class(logitFit))) {
    pValues <- round(summary(logitFit)$coefficients[, 4], 5)
    pValues <- pValues[-(which(names(pValues) %in% "(Intercept)"))]
    if(!(max(pValues) <= 0.05) & length(pValues) > 1) {
      while(max(pValues) > 0.05) {
        tmpVars <- setdiff(names(pValues), c(names(pValues[which.max(pValues)]), "(Intercept)"))
        logitFit <- try(glm(logitForm, data=tmpData[, c("target", tmpVars)], family=binomial, control=list(maxit=50)))
        if(!("try-error" %in% class(logitFit))) {
          pValues <- round(summary(logitFit)$coefficients[, 4], 5)
          if(sum(names(pValues) %in% "(Intercept)") > 0) {
            pValues <- pValues[-(which(names(pValues) %in% "(Intercept)"))]
          }
        }
      }
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
selectedVars <- c("VAR_0089", "VAR_1029", "VAR_0078", "VAR_1791", "VAR_0073_YR", "VAR_0795", "VAR_0970", 
  "VAR_1271", "VAR_0503", "VAR_0120")


for(i in 11:25) {
	print(i)
	selectedVars <- unique(selectedVars)
	numVars <- setdiff(numVars, selectedVars)
	Outputs <- matrix(NA, nrow=length(numVars), ncol=3)
	Outputs <- data.frame(Outputs)
	names(Outputs) <- c("Variable", "AUC_VAL1", "AUC_VAL2")
	Outputs[, 1] <- numVars


	for(varList in numVars) {
	  tmpOut <- logitDevlopment(c(selectedVars, varList))
	  Outputs[which(numVars %in% varList), -1] <- tmpOut
	  # print(Outputs[which(numVars %in% varList), ])
	}
	endTime <- Sys.time()

	Outputs$Average <- c(Outputs$AUC_VAL1 + Outputs$AUC_VAL2) / 2
	Outputs <- Outputs[order(Outputs$Average, decreasing=TRUE), ]
	selectedVars <- c(selectedVars, Outputs$Variable[1])
    print(Outputs[1, ])
	write.csv(Outputs, paste0("Models/logit/Outputs_", i, ".csv"), row.names=FALSE)
}
Outputs
difftime(endTime, startTime)
