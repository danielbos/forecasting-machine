getFMModels.FMRvm <- function(){
  allModels <- list(getModel.rvmLinear(),
                    getModel.rvmPoly(),
                    getModel.rvmRadial())
  return (allModels)
}

FMRvm <- function(newmodels=list()){
  me <- FMRegressor()  
  class(me) <- append(class(me),"FMRvm")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  return(me)
}

getModel.rvmLinear<-function(){
  # Name:       "Relevance Vector Machines with Linear Kernel"
  # Algorithm:  "rvmLinear"
  # Type:       "Regression"
  # Package:    "kernlab"
  # Params:     "None" 
  # Family:     
  model <- FMModel()
  model$name <- "rvmLinear"
  model$Description <- "Relevance Vector Machines with Linear Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMRvm"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("kernlb")
  return (model)
}
getModel.rvmPoly<-function(){
  # Name:       "Relevance Vector Machines with Polynomial Kernel"
  # Algorithm:  "rvmPoly"
  # Type:       "Regression"
  # Package:    "kernlab"
  # Params:     "scale, degree" 
  # Family:     
  model <- FMModel()
  model$name <- "rvmPoly"
  model$Description <- "Relevance Vector Machines with Polynomial Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMRvm"
  model$params <- c("scale","degree") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.rvmRadial<-function(){
  # Name:        "Relevance Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "rvmRadial"
  # Type:        "Regression"
  # Package:     "kernlab"
  # Params:      "sigma" 
  # Family:     
  model <- FMModel()
  model$name <- "rvmRadial"
  model$Description <- "Relevance Vector Machines with Radial Basis Function Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMRvm"
  model$params <- c("sigma") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}