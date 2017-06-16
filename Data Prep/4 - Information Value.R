#-------------------------------------------------------------------------------
# statsTest()
# The function gives statistical test based on the variable type.
# Author - Vikas Agrawal
#-------------------------------------------------------------------------------
statsTest <- function(x, y, Data) {
  dataX <- Data[, x]
  classX <- class(dataX)

  dataY <- Data[, y]
  dataY <- as.factor(dataY)

  if(classX=="factor" | classX=="character") {
    dataX <- as.factor(dataX)
    tempOut <- try(chisq.test(x=dataX, y=dataY))
    if(class(tempOut)!="try-error") {
      retVal <- tempOut$p.value
    } else {
      retVal <- NA
    }
  } else {
    tempOut <- try(t.test(as.formula(paste0(x, " ~ ", y)), data=Data))
    if(class(tempOut)!="try-error") {
      retVal <- tempOut$p.value
    } else {
      retVal <- NA
    }
  }
  return(retVal)
}

#-------------------------------------------------------------------------------
# parseRpart()
# The function takes decision rules and return the rules in a data frame
# Author - Kumar Manglam Thakur, Vikas Agrawal
#-------------------------------------------------------------------------------
parseRpart <- function(x, rpart.rules) {
  out_rule <- data.frame(min=numeric(0), max=numeric(0),
                min_comp=character(0), max_comp=character(0),
                tree_node = character(0), stringsAsFactors=F)

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
# Author - Kumar Manglam Thakur, Vikas Agrawal
# Examples
#   Data <- ivNumDT("AD_FIRST_MTGE_AMT", y, rawData)
#-------------------------------------------------------------------------------
ivNumDT <- function(x, y, Data, rcontrol=NULL, x_new=paste0(x, "_DT")) {
  # Define rpart control parameters to run decision tree model
  if(is.null(rcontrol)) {
    rcontrol <- rpart.control(cp=10^-8, maxdepth=4)
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
	return(Data[, c(y, x_new)])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
# getIVtable()
# The function generate information value table.
# Author - Kumar Manglam Thakur, Vikas Agrawal
# Examples
#   ivTable <- getIVtable("AD_FIRST_MTGE_AMT_DT", y, Data)
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

  # Reorder variables
  iv_data <- iv_data[c("variable", "class", "woe", "miv", "count_0", "count_1", "pct_0", "pct_1", "odds")]
  rownames(iv_data) <- NULL

  # Output
  iv_data
}

#-------------------------------------------------------------------------------
# Perform Statistical Analysis
#-------------------------------------------------------------------------------
library(rpart)

startTime <- Sys.time()
retStatOut <- list()
y <- "target"
varList <- setdiff(names(prepData), c("ID", "target"))
for(x in varList) {
  print(paste0(x, " - ", which(varList %in% x)))
  retStatOut[[x]] <- statsTest(x, y, prepData)
}
endTime <- Sys.time()
timeTaken3 <- difftime(endTime, startTime)
timeTaken3 #24.60501 mins

#-------------------------------------------------------------------------------
# Generate Binned Variables using Decision Tree
#-------------------------------------------------------------------------------
startTime <- Sys.time()
numr_vars <- setdiff(names(prepData[, sapply(prepData, is.numeric)]), c("ID", "target"))
retIV <- list()
for(x in numr_vars) {
  print(paste0(x, " - ", which(numr_vars %in% x)))
  tmpData <- try(ivNumDT(x, y, prepData))
  if(!is.null(tmpData)) {
    tempIV <- getIVtable(paste0(x, "_DT"), y, tmpData)
    retIV[[x]] <- sum(tempIV$miv)
  } else {
    retIV[[x]] <- NA
  }
}
endTime <- Sys.time()
timeTaken4 <- difftime(endTime, startTime)
timeTaken4 #50.60337 mins

retStatOut <- unlist(retStatOut)
retStatOut <- round(retStatOut, 5)
retIV <- unlist(retIV)
