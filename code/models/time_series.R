library(forecast)
library(xts)

#TODO: TSLM

FMTimeSeries <- function(newmodels=list()){
  me <- list(models = newmodels, 
             fittedModels = list(), 
             modelsPredictions = list())  
  ## Set the name for the class
  class(me) <- append(class(me),"FMTimeSeries")
  return(me)
}

getFMModels.FMTimeSeries <- function(obj){
  allModels<- list(
           #getModel.naive(),
           #getModel.snaive(),
           #getModel.rwf(),
           #getModel.meanf(),
           getModel.arimaTS(),
           getModel.arima(),
           getModel.ets())
  return(allModels)
}


getModel.ets <- function(){
  # Name:        "Time Series ETS Model"
  # Algorithm:   "ets"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "ets"
  model$Description <- "Time Series ETS Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}

getModel.arimaTS <- function(){
  # Name:        "Time Series Arima Model"
  # Algorithm:   "arimaTS"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "arimaTS"
  model$Description <- "Time Series Arima Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}

getModel.arima <- function(){
  # Name:        "Time Series Arima with Features Model"
  # Algorithm:   "arima"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "arima"
  model$Description <- "Time Series Arima with Features Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}


getModel.meanf <- function(){
  # Name:        "Time Series MeanF Model"
  # Algorithm:   "meanf"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "meanf"
  model$Description <- "Time Series MeanF Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}

getModel.rwf <- function(){
  # Name:        "Time Series RWF Model"
  # Algorithm:   "rwf"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "rwf"
  model$Description <- "Time Series RWF Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}

getModel.naive <- function(){
  # Name:        "Time Series Naive Model"
  # Algorithm:   "naive"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "naive"
  model$Description <- "Time Series Naive Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}

getModel.snaive <- function(){
  # Name:        "Time Series Sesonal Naive Model"
  # Algorithm:   "snaive"
  # Type:        "Time Series"
  # Package:     "forecast"
  # Params:      "h" 
  # Family:  
  model <- FMModel()
  model$name <- "snaive"
  model$Description <- "Time Series Seasonal Naive Model"
  model$featureSelection <- FALSE
  model$type <- "FMTimeSeries"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("forecast")
  return (model)
}



getTimeSeriesData <- function(data, agg="D", allColumns=TRUE){
  if(agg == "M"){
    return(getMonthlyTimeSeries(data))
  }
    return (getDailyTimeSeries(data, allColumns))
}

getDailyTimeSeries <- function(data, allColumns=TRUE){
  
  if(allColumns){
    dailyTS <- xts(x=data[,names(data) != "TSDate"], order.by = data[,"TSDate"])
    names(dailyTS) <- colnames(data[,names(data) != "TSDate"])
  }else{
    dailyTS <- xts(x=data[,c("y")], order.by = data[,"TSDate"])
    names(dailyTS) <- c("y")    
  }
   dailyTS$y <- as.numeric(dailyTS$y)
  
  return(dailyTS)
}

getMonthlyTimeSeries <- function(data){
  #TO DO -> return all columns???
  minDate <- min(Date$TSDate)
  minYear <- as.numeric(format(minDate, "%Y"))
  minMonth <- as.numeric(format(minDate, "%m"))
  monthlyTS <- ts(data, start=c(minYear, minMonth), frequency = 12)
  return(monthlyTS)
}

predictNaive <- function(trainingData, forecastPeriods){
  print('Predicting Naive Time Series')
  #NAIVE - the forecast is equal to the last observed value
  predNaive <- naive(trainingData, h=forecastPeriods) 
  return(predNaive)
}

predictSNaive <- function(trainingData, forecastPeriods){
  print('Predicting SNaive Time Series')
  #SNAIVE - the forecast is similar to the last observed value of the same period
  predSNaive <- snaive(trainingData, h=forecastPeriods) 
  return(predSNaive)
}

predictRWF <- function(trainingData, forecastPeriods){
  print('Predicting RWF Time Series')
  #RWF - the forecasts to increase or decrease over time, set to be the average change seen in the historical data
  predRWF <- rwf(trainingData, h=forecastPeriods, drift=TRUE)
  return(predRWF)
}

predictMeanf <- function(trainingData, forecastPeriods){
  print('Predicting Meanf Time Series')
  #MEANF - the forecast is equal to the mean of all the historical data 
  predMeanf <- meanf(trainingData, h=forecastPeriods)
  return(predMeanf)  
}


fitTslm <- function(trainingData, addTrend=TRUE, addSeasonality=TRUE, allColumns=TRUE){
  #TODO: How to set this features to run in the machine so that the user changes it easily and can run different algorithms???
  if(allColumns == FALSE)
  {
    if(addTrend == FALSE & addSeasonality == FALSE){
      stop("At least one option must be set to true (addTrend, addSeasonality, allColumns)")
    }
    if(addTrend & addSeasonality){
      log.info("Running tslm model with time series components only!")
      model <- tslm(trainingData[,"y"] ~ trend + season)
      return(model)
    }
    if(addTrend == FALSE & addSeasonality){
      log.info("Running tslm model with time series seasonality only!")
      model <- tslm(trainingData[,"y"] ~ season)
      return(model)
    }
    if(addTrend & addSeasonality == FALSE){
      log.info("Running tslm model with time series trend only!")
      model <- tslm(trainingData[,"y"] ~ trend)
      return(model)
    }
  }else{
    
    if(addTrend == FALSE & addSeasonality == FALSE){
      log.warn("Running time series linear model without time series components \n! 
               Consider running the linear model from FMLinearRegressor!")
      model <- tslm(trainingData[,"y"] ~ ., data=trainingData[,!names(trainingData) %in% c("y", "TSDate")])
      return(model)
    }
    if(addTrend & addSeasonality){
      log.info("Added trend and sezsonality has parameters to tslm!")
      model <- tslm(trainingData[,"y"] ~ trend + season + ., data=trainingData[,!names(trainingData) %in% c("y", "TSDate")])
      return(model)
    }
    if(addTrend == FALSE & addSeasonality){
      log.info("Running tslm model with more time series seasonality!")
      model <- tslm(trainingData[,"y"] ~ season + ., data=trainingData[,!names(trainingData) %in% c("y", "TSDate")])
      return(model)
    }
    if(addTrend & addSeasonality == FALSE){
      log.info("Running tslm model with more time series trend!")
      model <- tslm(trainingData[,"y"] ~ trend + ., data=trainingData[,!names(trainingData) %in% c("y", "TSDate")])
      return(model)
    }
  }
}



predictArima <- function(trainingData, testData, allColumns=TRUE, forecastHorizon){
  #TODO: Log fittedModel
  trainingData <- getTimeSeriesData(data=trainingData, allColumns=allColumns)
  testData <- getTimeSeriesData(data=testData, allColumns=allColumns)

  if(!allColumns){
    print('Training Arima Time Series without features')    
    model <- auto.arima(trainingData[,"y"])
    print('Forecasting Arima Time Series without features')    
    predictions <- forecast(model, h=forecastHorizon)
    return(predictions)
  }else{
    print('Training Arima Time Series with features')    
    model <- auto.arima(trainingData[,"y"], xreg=trainingData[,!names(trainingData) %in% c("y", "TSDate")])
    print('Forecasting Arima Time Series with features')  
    predictions <- forecast(model, xreg=testData[,!names(testData) %in% c("y", "TSDate")], h=forecastHorizon)
    return(predictions)
  }
}

predictETS <- function(trainingData, forecastHorizon){
  #TODO: Log fittedModel
  trainingData <- getTimeSeriesData(data=trainingData, allColumns=FALSE)
  print('Training ETS Time Series')    
  model <- ets(trainingData[,"y"])
  print('Forecasting ETS Time Series')  
  predictions <- forecast(model, h=forecastHorizon)
  return(predictions)
}


predictTSLM <- function(fit, testData ,forecastHorizon){
  #TODO: Convert all the TestData to Time Series????
  #fitTslm() ???
  print('Predicting Linear Regresion(TSLM) Time Series')
  
  y <- forecast(fit, h=forecastHorizon, newData=testData)
  return(y)
}

# fitFMModel.FMTimeSeries <- function(trainingData, testData, forecastPeriods, model, agg){
#       naive={  predictNaive(getTimeSeriesData(trainingData, agg, FALSE), forecastPeriods)  
#       },
#       snaive={ predictSNaive(getTimeSeriesData(trainingData, agg, FALSE), 
#                                    forecastPeriods) 
#       },
#       rwf={ predictRWF(getTimeSeriesData(trainingData, agg, FALSE), 
#                                 forecastPeriods)    
#       },
#       meanf={ predictMeanf(getTimeSeriesData(trainingData, agg, FALSE), 
#                                   forecastPeriods)  
#       },
#      

predictFMModel.FMTimeSeries <- function(obj, model, trainingData, testData, forecastHorizon){
  #TODO: Models With no training Data
  #TODO: Discover frequency of time-series automatically
  
  browser()
  switch(model$name, 
         arima={ predictions <- predictArima(trainingData, testData, allColumns=TRUE, forecastHorizon)
                 return (predictions) 
         },
         arimaTS={ predictions <- predictArima(trainingData, testData, allColumns=FALSE, forecastHorizon)
                   return (predictions)
         },
         ets={ predictions <- predictETS(trainingData, forecastHorizon)
               return (predictions) 
         },         
{
  #TODO: log a warning:
  print(paste("Time Series model with name %s not found in the training models.", model$name))
  #stop("Time Series model with name %s not found", model$name)
}
  )
}
