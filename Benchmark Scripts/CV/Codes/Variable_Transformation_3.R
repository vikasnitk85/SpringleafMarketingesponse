#-------------------------------------------------------------------------------
# Environment Set-up
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
gc()
options(scipen=999)

setwd("/home/rstudio/Dropbox/Public/Springleaf")
library(rpart)
library(doSNOW)

cl <- makeCluster(32, type="SOCK")
registerDoSNOW(cl)

subversion <- 3
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

#-------------------------------------------------------------------------------
# transData()
# The function takes data as input and return data with many transformed variables
#-------------------------------------------------------------------------------
transData <- function(train, x, y) {
  Data <- train[, c(y, x)]
  Data[, "NUM_FACT"] <- as.numeric(as.factor(Data[, x]))
  Data[, "LOG"] <- log(Data[, x])
  Data[, "INV"] <- (Data[, x])^-1
  Data[, "SQRT"] <- (Data[, x])^0.5
  Data[, "SQUARE"] <- (Data[, x])^2
  Data[, "CUBE"] <- (Data[, x])^3
  Data[, "REP_1"] <- Data[, x]
  Data[which(Data[, "REP_1"]==-1), "REP_1"] <- NA
  Data[, "REP_95"] <- Data[, "REP_94"] <- Data[, "REP_90"] <- Data[, "REP_99_90"] <- Data[, x]
  Data[, "REP_99"] <- Data[, "REP_98"] <- Data[, "REP_97"] <- Data[, "REP_96"] <- Data[, x]
  Data[which(Data[, "REP_99"]==99), "REP_99"] <- NA
  Data[which(Data[, "REP_98"]==98), "REP_98"] <- NA
  Data[which(Data[, "REP_97"]==97), "REP_97"] <- NA
  Data[which(Data[, "REP_96"]==96), "REP_96"] <- NA
  Data[which(Data[, "REP_95"]==95), "REP_95"] <- NA
  Data[which(Data[, "REP_94"]==94), "REP_94"] <- NA
  Data[which(Data[, "REP_90"]==90), "REP_90"] <- NA
  Data[which(Data[, "REP_99_90"] %in% c(99, 98, 97, 96, 95, 94, 90)), "REP_99_90"] <- NA
  Data[, "REP_995"] <- Data[, "REP_994"] <- Data[, "REP_990"] <- Data[, "REP_999_990"] <- Data[, x]
  Data[, "REP_999"] <- Data[, "REP_998"] <- Data[, "REP_997"] <- Data[, "REP_996"] <- Data[, x]
  Data[which(Data[, "REP_999"]==999), "REP_999"] <- NA
  Data[which(Data[, "REP_998"]==998), "REP_998"] <- NA
  Data[which(Data[, "REP_997"]==997), "REP_997"] <- NA
  Data[which(Data[, "REP_996"]==996), "REP_996"] <- NA
  Data[which(Data[, "REP_995"]==995), "REP_995"] <- NA
  Data[which(Data[, "REP_994"]==994), "REP_994"] <- NA
  Data[which(Data[, "REP_990"]==990), "REP_990"] <- NA
  Data[which(Data[, "REP_999_990"] %in% c(999, 998, 997, 996, 995, 994, 990)), "REP_999_990"] <- NA
  Data[, "REP_9995"] <- Data[, "REP_9994"] <- Data[, "REP_9990"] <- Data[, "REP_9999_9990"] <- Data[, x]
  Data[, "REP_9999"] <- Data[, "REP_9998"] <- Data[, "REP_9997"] <- Data[, "REP_9996"] <- Data[, x]
  Data[which(Data[, "REP_9999"]==9999), "REP_9999"] <- NA
  Data[which(Data[, "REP_9998"]==9998), "REP_9998"] <- NA
  Data[which(Data[, "REP_9997"]==9997), "REP_9997"] <- NA
  Data[which(Data[, "REP_9996"]==9996), "REP_9996"] <- NA
  Data[which(Data[, "REP_9995"]==9995), "REP_9995"] <- NA
  Data[which(Data[, "REP_9994"]==9994), "REP_9994"] <- NA
  Data[which(Data[, "REP_9990"]==9990), "REP_9990"] <- NA
  Data[which(Data[, "REP_9999_9990"] %in% c(9999, 9998, 9997, 9996, 9995, 9994, 9990)), "REP_9999_9990"] <- NA
  Data[, "REP_99999"] <- Data[, "REP_999999"] <- Data[, "REP_999994"] <- Data[, "REP_999999_999994"] <- Data[, "REP_9999999"] <- Data[, x]
  Data[which(Data[, "REP_99999"]==99999), "REP_99999"] <- NA
  Data[which(Data[, "REP_999999"]==999999), "REP_999999"] <- NA
  Data[which(Data[, "REP_999994"]==999994), "REP_999994"] <- NA
  Data[which(Data[, "REP_999999_999994"] %in% c(999999, 999994)), "REP_999999_999994"] <- NA
  Data[which(Data[, "REP_9999999"]==9999999), "REP_9999999"] <- NA
  Data[, "REP_999999995"] <- Data[, "REP_999999994"] <- Data[, "REP_999999999_999999994"] <- Data[, x]
  Data[, "REP_999999999"] <- Data[, "REP_999999998"] <- Data[, "REP_999999997"] <- Data[, "REP_999999996"] <- Data[, x]
  Data[which(Data[, "REP_999999999"]==999999999), "REP_999999999"] <- NA
  Data[which(Data[, "REP_999999998"]==999999998), "REP_999999998"] <- NA
  Data[which(Data[, "REP_999999997"]==999999997), "REP_999999997"] <- NA
  Data[which(Data[, "REP_999999996"]==999999996), "REP_999999996"] <- NA
  Data[which(Data[, "REP_999999995"]==999999995), "REP_999999995"] <- NA
  Data[which(Data[, "REP_999999994"]==999999994), "REP_999999994"] <- NA
  Data[which(Data[, "REP_999999999_999999994"] %in% c(999999999, 999999998, 999999997, 999999996, 999999995, 999999994)), "REP_999999999_999999994"] <- NA
  names(Data)[2] <- "RAW"
  for(i in 2:ncol(Data)) {
    if(sum(is.infinite(Data[, i]))) Data[is.infinite(Data[, i]), i] <- NA
    if(sum(is.nan(Data[, i]))) Data[is.nan(Data[, i]), i] <- NA
  }
  return(Data)
}

#-------------------------------------------------------------------------------
# Generate IV
#-------------------------------------------------------------------------------
# Separate date and character variables
train_char <- train[, sapply(train, is.character)]
train_date <- train_char[, grep("JAN1|FEB1|MAR1", train_char), ]
train_char <- train_char[, !colnames(train_char) %in% colnames(train_date)]
train[, names(train_char)] <- train_char

# Converting date variables to year
train_date <- sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date <- do.call(cbind.data.frame, train_date)
train_date <- sapply(train_date, function(x) as.numeric(format(x, "%Y")))
train_date <- data.frame(train_date)
train[, names(train_date)] <- train_date

# Convert character variables to numeric
for(i in names(train)) {
  if(class(train[, i]) == "character") {
    tmp <- as.numeric(as.factor(c(train[, i])))
    train[, i] <- head(tmp, nrow(train))
  }
}

train[train==-99999] <- NA

# List of Variables
dropVars <- names(which(sapply(train, class) == "logical"))
varList <- setdiff(names(train), c(y, dropVars))

# Data frame to store IV
Outputs <- data.frame(matrix(NA, nrow=length(varList), ncol=45))
Outputs[, 1] <- varList

# Loop for IV
# x <- "VAR_0001"
for(x in varList) {
  Data <- transData(train, x, y)
  tmpVars <- setdiff(names(Data), y)

  tmpOut <- foreach(k=1:length(tmpVars), .combine=c, .packages="rpart") %dopar% {
    tmpData <- try(ivNumDT(tmpVars[k], y, Data[, c(tmpVars[k], y)], rcontrol=NULL, x_new=paste0(tmpVars[k], "_DT")))
    if(all(class(tmpData) != "try-error") & ncol(tmpData) != 2) {
      tmpData[, tmpVars[k]] <- tmpData[, paste0(tmpVars[k], "_DT")]
      if(sum(is.na(tmpData[, tmpVars[k]])) > 0) {
        levels(tmpData[, tmpVars[k]]) <- c(levels(tmpData[, tmpVars[k]]), "MISSING")
        tmpData[is.na(tmpData[, tmpVars[k]]), tmpVars[k]] <- "MISSING"
      }
      iv <- try(getIVtable(tmpVars[k], y, tmpData))
      if(class(iv) != "try-error") {
        tmpOut <- sum(iv$miv)
      } else {
	    tmpOut <- NA
	  }
    } else {
	  tmpOut <- NA
	}
    tmpOut
  }

  Outputs[which(varList %in% x), -1] <- tmpOut
  print(Outputs[which(varList %in% x), ])
  write.csv(Outputs, paste0("Variable_Transformation_", subversion, ".csv"), row.names=FALSE)
}

names(Outputs) <- c("Variable", names(Data)[-1])
MAX <- NA
BEST <- NA
for(i in 1:nrow(Outputs)) {
  MAX[i] <- ifelse(is.finite(max(Outputs[i, -1], na.rm=TRUE)), max(Outputs[i, -1], na.rm=TRUE), NA)
  BEST[i] <- ifelse(!is.null(names(which.max(Outputs[i, -1]))), names(which.max(Outputs[i, -1])), "RAW")
}

Outputs$MAX <- MAX
Outputs$BEST <- BEST
write.csv(Outputs, paste0("Variable_Transformation_", subversion, ".csv"), row.names=FALSE)

endTime <- Sys.time()
difftime(endTime, startTime)
stopCluster(cl)



