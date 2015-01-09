FMLogicRegressor <- function(newmodels=list()){  
  me <- FMRegressor()  
  class(me) <- append(class(me),"FMLogicRegressor")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)  
  return(me)
}

getFMModels.FMLogicRegressor <- function(){
  allModels <- list(getModel.logreg())
  return(allModels) 
}

getModel.logreg<-function(){
  # Name:       "Logic Regression"
  # Algorithm:  "logreg"
  # Type:       "Dual Use"
  # Package:    "LogicReg"
  # Params:     "treesize, ntrees" 
  # Family:     
  model <- FMModel()
  model$name <- "logreg"
  model$Description <- "Logic Regression"
  model$featureSelection <- FALSE
  model$type <- "FMLogicRegressor"
  model$params <- c("treesize", "ntrees")
  model$slowTrain <- FALSE
  model$package <- c("LogicReg")
  return (model)
  
}

 


