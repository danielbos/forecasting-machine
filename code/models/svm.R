source("models/fmregressor.R")

FMSVM <- function(newmodels=list())    
  me <- FMRegressor()
  class(me) <- append(class(me),"FMSVM")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  return(me)
}

getFMModels.FMSVM <- function(){
  allModels <- list(getgetModel.svmBoundrangeString(),
                    getgetModel.svmExpoString(),
                    getgetModel.svmLinear(),
                    getgetModel.svmPoly(),
                    getgetModel.svmRadial(),
                    getgetModel.svmRadialCost(),
                    getgetModel.svmSpectrumString())
  return(allModels)
}

getModel.svmBoundrangeString<-function(){
  # Name:        "Support Vector Machines with Boundrange String Kernel"
  # Algorithm:   "svmBoundrangeString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "length, C" 
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmBoundrangeString"
  model$Description <- "Support Vector Machines with Boundrange String Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("length", "C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmExpoString<-function(){
  # Name:        "Support Vector Machines with Exponential String Kernel"
  # Algorithm:   "svmExpoString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "lambda, C" 
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmExpoString"
  model$Description <- "Support Vector Machines with Exponential String Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("lambda", "C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmLinear<-function(){
  # Name:        "Support Vector Machines with Linear Kernel"
  # Algorithm:   "svmLinear"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "C" 
  # Family:      "SVM"    
  model <- FMModel()
  model$name <- "svmLinear"
  model$Description <- "Support Vector Machines with Linear Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmPoly<-function(){
  # Name:        "Support Vector Machines with Polynomial Kernel"
  # Algorithm:   "svmPoly"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "degree, scale, C"
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmPoly"
  model$Description <- "Support Vector Machines with Polynomial Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("degree", "scale", "C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmRadial<-function(){
  # Name:        "Support Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "svmRadial"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "sigma, C" 
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmRadial"
  model$Description <- "Support Vector Machines with Radial Basis Function Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("sigma", "C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmRadialCost<-function(){
  # Name:        "Support Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "svmRadialCost"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "C" 
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmRadialCost"
  model$Description <- "Support Vector Machines with Radial Basis Function Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}
getModel.svmSpectrumString<-function(){
  # Name:        "Support Vector Machines with Spectrum String Kernel"
  # Algorithm:   "svmSpectrumString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "length, C" 
  # Family:      "SVM"
  model <- FMModel()
  model$name <- "svmSpectrumString"
  model$Description <- "Support Vector Machines with Spectrum String Kernel"
  model$featureSelection <- FALSE
  model$type <- "FMSVM"
  model$params <- c("length", "C") 
  model$slowTrain <- FALSE
  model$package <- c("kernlab")
  return (model)
}