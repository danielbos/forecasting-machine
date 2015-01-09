FMRegressor <- function(newModels=list()){
  me <- list(models = newModels, 
             fittedModels = list(), 
             modelsPredictions = list(),
             fitControl = trainControl(
               ## 10-fold CV
               method = "repeatedcv",
               number = 10,
               ## repeated ten times
               repeats = 10))  
  ## Set the name for the class
  class(me) <- append(class(me),"FMRegressor")
  return(me)
}

fitFMModel <- function(obj, model, trainingData){
    UseMethod("fitFMModel", obj)
}

# fitAllFMModel <- function(obj, trainingData, fitControl){
#   UseMethod("fitAllFMModel", obj)
# }

predictFMModel <- function(obj, model, trainingData, testData, forecastHorizon){
  UseMethod("predictFMModel", obj)
}

# predictAllFMModel <- function(obj, testData){
#   UseMethod("predictAllFMModel", obj)
# }



fitFMModel.FMRegressor <- function(obj, model, trainingData){
  #browser()
  #if(!(model %in% obj$models)){
    #stop("That model doesn't exist or is not supported in this type!!!")
  #}
  trainingData <- removeDfColumns(trainingData, c("TSDate"))
  print(paste("Running model:", model$name))
  print(Sys.time())
  modelFit <- train(y ~ ., 
                   data = trainingData,
                   method = model$name,
                   trControl = obj$fitControl
                   #preProc = c("center", "scale")
                   )
  print(Sys.time())
  return(modelFit)
}


# fitAllFMModel.FMRegressor <- function(obj, trainingData, ...){
#   results <- list()
#   allNames <- list()
# 
#   for(model in obj$models){
#     fit <- fitFMModel(obj, model, trainingData)
#     allNames <- c(model$name, list(allNames)) 
#     results <- c(results,list(fit))
#   }
#   names(results) <- allNames
#   obj$fittedModels <- results
#   
#   return(obj)
# }


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


predictAllFMModels <- function(models, fittedModels, testData, forecastHorizon,...){
  browser()
  modelsPredictions <- list()
  
  for(model in models){
    obj <- getObject(model)
    pred <- predictFMModel(obj, model, fittedModels, testData, forecastHorizon)
    modelsPredictions <- c(modelsPredictions,list(pred))
  }
  names(modelsPredictions) <- names(obj$fittedModels)
  
  return(modelsPredictions)
}


predictFMModel.FMRegressor <- function(obj, model, fittedModels, testData, forecastHorizon){
  predictions <- predict(fitModel, testData)
  return(predictions)
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