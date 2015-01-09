source("preprocess/dataPreparation.R")
source("utils/global_functions.R")
#source("models/fmregressor.R")
#source("models/neural_networks.R")
#source("models/regression_trees.R")
#source("models/time_series.R")
library("caret")
library("forecast")
library(doMC)
cpus <- detectCores(all.tests = FALSE, logical = FALSE)
registerDoMC(cpus -1)
main <- function()
{
 
  

df <- read.csv("../datasets/salesallvar.csv", stringsAsFactors = FALSE)

#To DO define a time series model and put this inside the model and as parameter
df <- adjustVariables(df, df$Date, df$SALES)

trainingStartDate <- "2012-01-01"  
trainingEndDate <- "2014-09-14"
  
trainingDataSample <- getSample(df, trainingStartDate, trainingEndDate)
#For regression
trainingData <- removeDfColumns(trainingDataSample, c("SALES","TSDate",  "Date", "y"))
                                
#For timeseries
#trainingData <- removeDfColumns(trainingDataSample, c("Date","SALES","TSDate"))

trainingData <- deleteCorrelated(trainingData,cutoff=.90)
t<- trainingData[,1:30]

findCorrelation(cor(t), cutoff = .50, verbose = FALSE)
findLinearCombos(t)

trainingData <- deleteLinearCombos(trainingData)
trainingData$TSDate <- trainingDataSample$TSDate
trainingData$y <- trainingDataSample$y
#trainingData <- deleteNearZeroVar(trainingData)
#trainingData <- cbind(TSDate=trainingDataSample$TSDate , trainingData)

testStartDate <- "2014-09-15"
testEndDate <-  "2014-09-28"
testData  <- getSample(df, testStartDate, testEndDate)

fm <- FM$new()
fm$run(trainingData=trainingData, testData=testData)
#for time series
#testData <- testData[, names(testData) %in% names(trainingData)]

#Time Series Test

#results <- predictAllTimeSeriesModels(trainingData, testData, 14, 'D')


#Regressor Test
testData <- removeDfColumns(testData , c("TSDate","SALES", "Date", "y"))



fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
#rmmodel <- FMRegressor()
#results <- fitFMRegressorModel(rmmodel, "gbm", trainingData, fitControl, verbose=FALSE)



#fmmodellm <- FMRegressorLM()
#print(Sys.time())
#fmmodellm <- fitAllFMRegressorModel(fmmodellm, trainingData, fitControl)
#print(Sys.time())
#fmmodellm <- predictAllFMRegressorModel(fmmodellm, testData)
#fmmodellm


fmmodellm <- FMRegressorRegTrees()
print(Sys.time())
fmmodellm <- fitAllFMRegressorModel(fmmodellm, trainingData, fitControl)
print(Sys.time())
fmmodellm <- predictAllFMRegressorModel(fmmodellm, testData)
fmmodellm



}