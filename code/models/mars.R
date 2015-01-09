
FMMars <- function(newmodels=list()){    
  me <- FMRegressor()
  class(me) <- append(class(me),"FMMars")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  return(me)
}

getFMModels.FMMars <- function(){
  allModels <- list(getModel.earth(),
                    getModel.gcvEarth())
  return (allModels)
}


getModel.earth<-function(){ 
  # Name:       "Multivariate Adaptive Regression Spline"
  # Algorithm:  "earth"
  # Type:       "Dual Use"
  # Package:    "earth"
  # Params:     "nprune degree" 
  # Family:     
  model <- FMModel()
  model$name <- "earth"
  model$Description <- "Multivariate Adaptive Regression Spline"
  model$featureSelection <- TRUE
  model$type <- "FMMars"
  model$params <- c( "nprune", "degree" )
  model$slowTrain <- FALSE
  model$package <- c("earth")
  return (model)
}

getModel.gcvEarth<-function(){
  # Name:       "Multivariate Adaptive Regression Splines"
  # Algorithm:  "gcvEarth"
  # Type:       "Dual Use"
  # Package:    "earth"
  # Params:     "degree" 
  # Family:      model <- FMModel()
  model$name <- "gcvEarth"
  model$Description <- "Multivariate Adaptive Regression Splines"
  model$featureSelection <- TRUE
  model$type <- "FMMars"
  model$params <- c( "degree")
  model$slowTrain <- FALSE
  model$package <- c("earth")
  return (model)
}
