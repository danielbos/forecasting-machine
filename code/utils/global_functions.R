getSample <- function(data, startDate, endDate){
  # Gets a sample from a dataframe based on a Date interval.
  #
  # Args: 
  #       data:                 Dataframe with all the data.Must have a columns named TSDate of the Date type.
  #       params:               Parameters that are stored in the config file that contain the training period. 
  # Returns: 
  #          Dataframe with the sample data.
  
  startSmpl <- which(data$TSDate==startDate)
  endSmpl <- which(data$TSDate==endDate)
  
  return (sample(startSmpl, endSmpl, data))
}



sample <- function(startSmpl, endSmpl, data){
  
  data <- data[startSmpl:endSmpl,]
  
  return(data)
}


removeDfColumns <- function(df, columns){
  # Removes columns from a dataframe
  #
  # Args: 
  #       df:                   Dataframe with all the data.
  #       columns:              List with the names of columns to remove.
  #  
  # Returns: 
  #          Dataframe without the specified columns.
  
  return(df[ ,!(colnames(df) %in% columns)])
}


getDateList <- function(params, prod){
  dateList <- list()
  if(prod){
    startForecastDate <- as.Date(as.character( Sys.Date()))
    endForecastDate <- as.Date(as.character(startForecastDate + params$forecastHorizon ))
    startTrainingDate <- as.Date(as.character( Sys.Date() - params$prodTrainingLength))
    
    dateList$training <- list( startTrainingDate,  as.Date(Sys.Date() - 1))
    dateList$forecasting <- list(startForecastDate, endForecastDate)
  }else{
    startTrainingDate = as.Date(params$startDate)
    endTrainingDate = as.Date(ifelse(is.character(params$endTrainingDate), 
                                     params$endTrainingDate, 
                                     as.character(Sys.Date() - params$forecastHorizon + 1)))
    startForecastDate = as.Date(as.character(endTrainingDate + 1))
    endForecastDate = as.Date(as.character(startForecastDate + params$forecastHorizon))
    
    dateList$training <- list(startTrainingDate, endTrainingDate)
    dateList$forecasting <- list(startForecastDate, endForecastDate)   
  }
  
  names(dateList$training) <- c("startDate", "endDate")
  names(dateList$forecasting) <- c("startDate", "endDate")
  return(dateList)
}




pkgCheckAndInstall <- function(x){
   #  Given the desired package name this function checks whether it has already been installed or not, 
  #  if not then it is installed. 
  # 
  #  Args:
  #    pkgName: Name of the package to install. 
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


checkDF <- function(df){
  # This function checks the given dataframe to see whether it satisfies the required conditions for the model or not.
  # 
  # Args:
  #   df: The dataframe to check
  #
  # Returns:
  #   If the given dataframe pass all the check TRUE, otherwise FALSE
  
  if (!"TSDate" %in%  colnames(df) || all(is.na( df$TSDate )))  #  Check that TSDate should not be NA.
    return(FALSE)
  
  else if (!"y" %in%  colnames(df) ||  all(is.na(df$y))) #  Check that y column is not empty.
    return(FALSE)
  
  else
    return(TRUE)
  
}

fitFMModel <- function(obj, model, trainingData){
  UseMethod("fitFMModel", obj)
}


predictFMModel <- function(obj, fitModel, testData){
  UseMethod("predictFMModel", obj)
}


fitAllFMModels <- function(models, trainingData, ...){
  
  fittedModels <- list()
  allNames <- list()
  
  for(model in models){
    
    result = tryCatch({
      
      obj <- getObject(model)
      fit <- fitFMModel(obj, model, trainingData, ...)
      allNames <- c(allNames, model$name)
      fittedModels <- c(fittedModels,list(fit))
    }, warning = function(w) {
      print(w$message)
    }, error = function(e) {
      #TODO: log error or insert in database 
      print(e$message)
    }) 
  }
  names(fittedModels) <- allNames
  return(fittedModels)
}

getObject <- function(model){
  switch(model$type, 
         FMLinearRegressor={ return (FMLinearRegressor()) },
         FMRandomForests={return (FMRandomForests())},
         FMAdditive={return (FMAdditive())},
         FMBag={return (FMBag())},
         FMClustering={return (FMClustering())},
         FMFuzzySystems={return (FMFuzzySystems())},
         FMGaussian={return (FMGaussian())},
         FMLogicRegressor={return (FMLogicRegressor())},
         FMMars={return (FMMars())},
         FMNeuralNetworks={return (FMNeuralNetworks())},
         FMPartDSA={return (FMPartDSA())},
         FMRegressionTrees={return (FMRegressionTrees())},
         FMRvm={return (FMRvm())},
         FMSOMap={return (FMSOMap())},
         FMSVM={return (FMSVM())},
         FMTimeSeries={return (FMTimeSeries())},
{
  stop("Model Type not found %s", model$type)
}
  )  
}


predictAllFMModels <- function(modelsToRun, fittedModels, testData, ...){
  modelsPredictions <- list()
  
  for(model in modelsToRun){
    pred <- predictFMModel(model, fittedModels, testData)
    modelsPredictions <- c(modelsPredictions,list(pred))
  }
  names(modelsPredictions) <- names(obj$fittedModels)
  
  return(modelsPredictions)
}




getBestPredictionModel <- function(obj, predictions, testData){
  
  for(results in predictions){
    results <- c(results,predictFMRegressorModel(obj, model, testData))
  }
  names(results) <- names(fittedModels)
  
}
getBestTrainingModel <- function(option="AIC"){
  #TO DO return the best based on AIC or Rsquared
  
}
selectModelFeatures <- function(){
  
  #TO DO
  # put option to receive model name that has feature selection
  # we have to get run that model and return the predictors
  # if that model isn't returned than
  # remove correlated
  # remove linear combo
  # remove near zero variance
  # 
}

compareModels <- function(models){
  #TO DO compare only on the forecast
}

compareModelsSignificance <- function(){}

predictRoolingWindow <- function(){}

storeResults <- function(){}
