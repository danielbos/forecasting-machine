source("preprocess/dataPreparation.R")
source("preprocess/deleteCorrelated.R")
source("preprocess/featureSelection.R")
source("utils/removeDFColumns.R")
#library("logging")
library("caret")
library("forecast")

#args <- commandArgs(TRUE)
#config <- loadConfigFile("../config/config.ini", args)

# Setup logger
#log.file <- paste0(config$logger$filepath, "/", Sys.Date(), ".log")
#basicConfig()
#addHandler(writeToFile, file = log.file, level = config$logger$threshold)

df <- read.csv("../datasets/salesallvar.csv", stringsAsFactors = FALSE)

#To DO define a time series model and put this inside the model and as parameter
df <- adjustVariables(df, df$Date, df$SALES)

df <- removeDFColumns(df, c("Date","SALES"))
df <- deleteCorrelated(df)

#linear regression model
model <- lm(df$Predict ~ ., data=df)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
lmbak <- train(df$Predict ~ ., data = df,
                 method = 'leapBackward',
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
p <-  df$Predict
df <- df[,!(names(df) %in% c("Predict"))]

findCorrelation(cor(x2), cutoff = .9, verbose = FALSE)
nzv <- nearZeroVar(x2)
x3 <- x2[, -nzv]
subsets <- c(1:5, 10, 15, 20, 25)
x2 <- model.matrix(~., data = df)[,-1]
lmProfile <- rfe(as.data.frame(x3), p, rfeControl = ctrl, sizes=subsets)
lmProfile <- rfe(df, p, rfeControl = ctrl, sizes=subsets)



#perform 
model <- featureSelection(model, "step")





fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmFit1 <- train(df$Predict ~ ., data = df,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
