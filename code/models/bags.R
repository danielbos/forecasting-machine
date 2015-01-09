FMBag <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMBag")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMBag <- function(){
  allModels <- list(getModel.bag(), 
                 getModel.bagEarth(), 
                 getModel.logicBag())
  return(allModels)
}

getModel.bag<- function(){
  # Name:       "Bagged Model"
  # Algorithm:  "bag"
  # Type:       "Dual Use"
  # Package:    "caret"
  # Params:     "vars" 
  # Family:     "Non-Linear"
  model <- FMModel()
  model$name <- "bag"
  model$Description <- "Bagged Model"
  model$featureSelection <- FALSE
  model$type <- "FMBag"
  model$params <- c( "vars" )
  model$slowTrain <- FALSE
  model$package <- c("caret")
  return (model)
}
getModel.bagEarth<- function(){
  # Name:        "Bagged MARS"
  # Algorithm:   "bagEarth"
  # Type:        "Dual Use"
  # Package:     "earth"
  # Params:      "nprune degree" 
  # Family:      "Non-Linear"
  model <- FMModel()
  model$name <- "bagEarth"
  model$Description <- "Bagged MARS"
  model$featureSelection <- TRUE
  model$type <- "FMBag"
  model$params <- c( "nprune", "degree" )
  model$slowTrain <- FALSE
  model$package <- c("earth")
  return (model)
}


getModel.logicBag<-function(){
  # Name:       "Bagged Logic Regression"
  # Algorithm:  "logicBag"
  # Type:       "Dual Use"
  # Package:    "logicFS"
  # Params:     "nleaves, ntrees" 
  # Family:     
  model <- FMModel()
  model$name <- "logicBag"
  model$Description <- "Bagged Logic Regression"
  model$featureSelection <- FALSE
  model$type <- "FMBag"
  model$params <- c( "nleaves", "ntrees" )
  model$slowTrain <- FALSE
  model$package <- c("logicFS")
  return (model)
}