#-------------------------------------------------------------------------------
# Environment Set-up
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

setwd("/home/rstudio/Dropbox/Public/Springleaf")
library(rpart)

subversion <- 1
startTime <- Sys.time()

#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------
load("Kaggle_RawData.RData")
y <- "target"
train$ID <- NULL

#-------------------------------------------------------------------------------
# parseRpart()
# The function takes decision rules and return the rules in a data frame
#-------------------------------------------------------------------------------
parseRpart <- function(x, rpart.rules) {
  out_rule <- data.frame(min=numeric(0), max=numeric(0),
                         min_comp=character(0), max_comp=character(0),
                         tree_node=character(0), stringsAsFactors=F)

  for(i in seq_along(rpart.rules)) {
    t1 <- gsub("Data[, x]", fixed=TRUE, replacement="", x=rpart.rules[[i]])
    ge <- gsub(pattern=">=", x=(t1[which(grepl("*>=[:digit:]*",x=t1,))]), fixed=T, replacement="")
    g  <- gsub(pattern=">" , x=(t1[which(grepl("*> [:digit:]*",x=t1,))]), fixed=T, replacement="")
    l  <- gsub(pattern="<" , x=(t1[which(grepl("*< [:digit:]*",x=t1,))]), fixed=T, replacement="")
    le <- gsub(pattern="<=", x=(t1[which(grepl("*<=[:digit:]*",x=t1,))]), fixed=T, replacement="")

    ge <- ifelse(length(ge)==0, NA, max(as.numeric(ge)))
    g  <- ifelse(length(g) ==0, NA, max(as.numeric(g)))
    le <- ifelse(length(le)==0, NA, min(as.numeric(le)))
    l  <- ifelse(length(l) ==0, NA, min(as.numeric(l)))

    out_rule[i, "min"] <- if (!(is.na(ge) && is.na(g))) max(ge, g, na.rm=T) else NA
    out_rule[i, "max"] <- if (!(is.na(le) && is.na(l))) max(le, l, na.rm=T) else NA
    out_rule[i, "min_comp"] <- ifelse(!is.na(ge), ">=", ifelse(!is.na(g), ">", ""))
    out_rule[i, "max_comp"] <- ifelse(!is.na(le), "<=", ifelse(!is.na(l), "<", ""))
    out_rule[i, "tree_node"] <- names(rpart.rules)[i]
  }
  out_rule
}

#-------------------------------------------------------------------------------
# ivNumDT()
# The function bins the numeric variable by decision tree rules
#-------------------------------------------------------------------------------
ivNumDT <- function(x, y, Data, rcontrol=NULL, x_new=paste0(x, "_DT")) {
  # Define rpart control parameters to run decision tree model
  if(is.null(rcontrol)) {
    rcontrol <- rpart.control(cp=10^-8, maxdepth=12)
  }

  # Build the decision tree model
  dtModel <- rpart(formula=as.numeric(Data[, y]) ~ Data[, x], data=Data, control=rcontrol)

  # Capture the components of decision tree
  rpart.rules <- path.rpart(dtModel, rownames(dtModel$frame)[dtModel$frame$var=="<leaf>"], print.it = FALSE)

  # Convert decision tree rules to data frame
  tree_rules <- parseRpart(x, rpart.rules)
  tree_rules <- tree_rules[order(tree_rules$max), ]

  if(nrow(tree_rules) > 1) {
    for(i in 1:nrow(tree_rules)) {
      tree_rules[i, "class_label"] <- paste0(
                  ifelse(tree_rules$min_comp[i] == ">=", "[", "("),
                  ifelse(is.na(tree_rules$min[i]), "", tree_rules$min[i]),
                  ", ",
                  ifelse(is.na(tree_rules$max[i]), "", tree_rules$max[i]),
                  ifelse(tree_rules$max_comp[i] == "<=", "]", ")")
                  )
    }

    # Add binned variable in the data
    for(rules in 1:nrow(tree_rules)) {
      if(tree_rules[rules, "min_comp"] == ">=" && tree_rules[rules, "max_comp"] == "<")
        Data[which(Data[, x] >= tree_rules[rules, "min"] & Data[, x] < tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == ">" && tree_rules[rules, "max_comp"] == "<")
        Data[which(Data[, x] > tree_rules[rules, "min"] & Data[, x] < tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == "" && tree_rules[rules, "max_comp"] == "<")
        Data[which(Data[, x] < tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == ">=" && tree_rules[rules, "max_comp"] == "<=")
        Data[which(Data[, x] >= tree_rules[rules, "min"] & Data[, x] <= tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == ">" && tree_rules[rules, "max_comp"] == "<=")
        Data[which(Data[, x] > tree_rules[rules, "min"] & Data[, x] <= tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == "" && tree_rules[rules, "max_comp"] == "<=")
        Data[which(Data[, x] <= tree_rules[rules, "max"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == ">=" && tree_rules[rules, "max_comp"] == "")
        Data[which(Data[, x] >= tree_rules[rules, "min"]), x_new] <- tree_rules[rules, "class_label"]
      if(tree_rules[rules, "min_comp"] == ">" && tree_rules[rules, "max_comp"] == "")
        Data[which(Data[, x] > tree_rules[rules, "min"]), x_new] <- tree_rules[rules, "class_label"]
    }

    Data[, x_new] <- factor(Data[, x_new], levels=tree_rules$class_label, ordered=TRUE)
    Data[, x_new] <- droplevels(Data[, x_new])
  }
  return(Data)
}

#-------------------------------------------------------------------------------
# getIVtable()
# The function generate information value table.
#-------------------------------------------------------------------------------
getIVtable <- function(x, y, Data) {
  # Count of good and bad in dependent variables
  tempTab <- table(Data[, y])
  total_1 <- as.numeric(tempTab["1"])
  total_0 <- as.numeric(tempTab["0"])

  iv_data <- as.data.frame.matrix(table(Data[, x], Data[, y]))
  names(iv_data) <- c("count_0", "count_1")

  # Compute WOE and IV for each category of the variable
  iv_data <- within(iv_data, {
               class <- row.names(iv_data)
               variable <- x
               pct_0 <- count_0 / total_0
               pct_1 <- count_1 / total_1
               odds <-  pct_0 / pct_1
               woe <- log(odds)
               miv <- (pct_0 - pct_1) * woe
  })

  # Some group for outcome 0 might have zero counts. This will result in -Inf or Inf WOE. Replacing - ODDS=1, WoE=0, MIV=0.
  if(any(iv_data$count_0 == 0) | any(iv_data$count_1 == 0)) {
    iv_data$woe <- ifelse(is.infinite(iv_data$woe), 0, iv_data$woe)
    iv_data$miv <- ifelse(is.infinite(iv_data$miv), 0, iv_data$miv)
    iv_data$odds <-ifelse(is.infinite(iv_data$odds), 1, iv_data$odds)
  }

  # Output
  iv_data
}

getIV <- function(x, y, Data) {
  # Convert variable to factor and compute IV
  tmpData <- Data
  tmpData[, x] <- as.factor(Data[, x])
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv1 <- sum(getIVtable(x, y, tmpData)$miv)

  # Compute IV for numeric variable
  tmpData <- Data
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv2 <- sum(getIVtable(x, y, tmpData)$miv)

  # Compute IV for log variable
  tmpData <- Data
  tmpData[, x] <- log(tmpData[, x])
  if(sum(is.infinite(tmpData[, x])) > 0) tmpData[which(is.infinite(tmpData[, x])), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv3 <- sum(getIVtable(x, y, tmpData)$miv)

  # Compute IV for sqrt variable
  tmpData <- Data
  tmpData[, x] <- sqrt(tmpData[, x])
  if(sum(is.infinite(tmpData[, x])) > 0) tmpData[which(is.infinite(tmpData[, x])), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv4 <- sum(getIVtable(x, y, tmpData)$miv)

  # Compute IV for inverse variable
  tmpData <- Data
  tmpData[, x] <- 1/tmpData[, x]
  if(sum(is.infinite(tmpData[, x])) > 0) tmpData[which(is.infinite(tmpData[, x])), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv5 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace -1 by NA
  tmpData <- Data
  if(sum(tmpData[, x] == -1, na.rm=TRUE) > 0) tmpData[which(tmpData[, x] == -1), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv6 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 99 by NA
  tmpData <- Data
  if(sum(tmpData[, x] == 99, na.rm=TRUE) > 0) tmpData[which(tmpData[, x] == 99), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv7 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 99 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(99, 98, 97, 96, 95, 94)) > 0)
    tmpData[which(tmpData[, x] %in% c(99, 98, 97, 96, 95, 94)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv8 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(999, 998, 997, 996, 995, 994)) > 0)
    tmpData[which(tmpData[, x] %in% c(999, 998, 997, 996, 995, 994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv9 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 9999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(9999, 9998, 9997, 9996, 9995, 9994)) > 0)
    tmpData[which(tmpData[, x] %in% c(9999, 9998, 9997, 9996, 9995, 9994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv10 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 99999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(99999, 99998, 99997, 99996, 99995, 99994)) > 0)
    tmpData[which(tmpData[, x] %in% c(99999, 99998, 99997, 99996, 99995, 99994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv11 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 999999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(999999, 999998, 999997, 999996, 999995, 999994)) > 0)
    tmpData[which(tmpData[, x] %in% c(999999, 999998, 999997, 999996, 999995, 999994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv12 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 9999999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(9999999, 9999998, 9999997, 9999996, 9999995, 9999994)) > 0)
    tmpData[which(tmpData[, x] %in% c(9999999, 9999998, 9999997, 9999996, 9999995, 9999994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv13 <- sum(getIVtable(x, y, tmpData)$miv)

  # Replace 999999999 by NA
  tmpData <- Data
  if(sum(tmpData[, x] %in% c(999999999, 999999998, 999999997, 999999996, 999999995, 999999994)) > 0)
    tmpData[which(tmpData[, x] %in% c(999999999, 999999998, 999999997, 999999996, 999999995, 999999994)), x] <- NA
  tmpData <- ivNumDT(x, y, tmpData, rcontrol=NULL, x_new=paste0(x, "_DT"))
  tmpData[, x] <- tmpData[, paste0(x, "_DT")]
  if(sum(is.na(tmpData[, x])) > 0) {
    levels(tmpData[, x]) <- c(levels(tmpData[, x]), "MISSING")
    tmpData[is.na(tmpData[, x]), x] <- "MISSING"
  }
  iv14 <- sum(getIVtable(x, y, tmpData)$miv)

  return(c(iv1, iv2, iv3, iv4, iv5, iv6, iv7, iv8, iv9, iv10, iv11, iv12, iv13, iv14))
}

#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
# List of Variables
varList <- setdiff(names(train), y)

# Convert character variables to numeric
for(i in names(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i], test[, i])))
    train[, i] <- head(tmp, nrow(train))
    test[, i] <- tail(tmp, nrow(test))
  }
}

# x <- "VAR_0001"
out <- data.frame(matrix(NA, nrow=length(varList), ncol=15))
out[, 1] <- varList

for(x in varList) {
  Data <- train[, c(y, x)]
  tmpOut <- try(getIV(x, y, Data))
  if(class(tmpOut) != "try-error") {
    out[which(varList %in% x), -1] <- tmpOut
  }
  print(out[which(varList %in% x), ])
  write.csv(out, paste0("Variable_Transformation_", subversion, ".csv"), row.names=FALSE)
}


