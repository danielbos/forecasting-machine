FMGaussian <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMGaussian")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}


getFMModels.FMGaussian <- function(){
  allModels <- list(getModel.gaussprLinear(),
                    getModel.gaussprPoly(),
                    getModel.gaussprRadial())
  return(allModels)
}




getModel.gaussprLinear<-function(){
  # Name:        "Gaussian Process"
  # Algorithm:   "gaussprLinear"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "None" 
  # Family:      "Gaussian Process"
  model <- FMModel()
  model$name <- "gaussprLinear"
  model$Description <- "Gaussian Process"
  model$featureSelection <- FALSE
  model$type <- "FMGaussian"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.gaussprPoly<-function(){
  # Name:        "Gaussian Process with Polynomial Kernel"
  # Algorithm:   "gaussprPoly"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "degree, scale" 
  # Family:      "Gaussian Process"
  model <- FMModel()
  model$name <- "gaussprPoly"
  model$Description <- "Gaussian Process with Polynomial Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMGaussian"
  model$params <- c( "degree", "scale" )
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.gaussprRadial<-function(){
  # Name:        "Gaussian Process with Radial Basis Function Kernel"
  # Algorithm:   "gaussprRadial"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "sigma" 
  # Family:      "Gaussian Process"
  model <- FMModel()
  model$name <- "gaussprRadial"
  model$Description <- "Gaussian Process with Radial Basis Function Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMGaussian"
  model$params <- c( "sigma" )
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}