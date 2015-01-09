FMPartDSA <- function(newmodels=list()){  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMPartDSA")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  return(me)
}

getFMModels.FMPartDSA <- function(){
  allModels <- list(getModel.partDSA())
  return(allModels)
}

getModel.partDSA<-function(){
  # Name:       "partDSA"
  # Algorithm:  "partDSA"
  # Type:       "Dual Use"
  # Package:    "partDSA"
  # Params:     "cut.off.growth,MPD" 
  # Family:  
  model <- FMModel()
  model$name <- "partDSA"
  model$Description <- "partDSA"
  model$featureSelection <- FALSE
  model$type <- "FMPartDSA"
  model$params <- c( "cut.off.growth", "MPD" )
  model$slowTrain <- FALSE
  model$package <- c("partDSA")
  return (model)
}