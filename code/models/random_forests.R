FMRandomForests <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMRandomForests")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMRandomForests <- function(){
  allModels <- list(getModel.rf(),
           getModel.Boruta(),
           getModel.cforest(),
           getModel.extraTrees(),
           getModel.parRF(),
           getModel.qrf(),
           getModel.RRF(),
           getModel.RRFglobal())
  return(allModels)
}


getModel.rf<-function(){
  # Name:       "Random Forest"
  # Algorithm:  "rf"
  # Type:       "Dual Use"
  # Package:    "randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "rf"
  model$Description <- "Random Forest"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry")
  model$slowTrain <- FALSE
  model$package <- c("randomForest")
  return (model)
}

getModel.Boruta<-function(){ 
  # Name:       "Random Forest with Additional Feature Selection"
  # Algorithm:  "Boruta"
  # Type:       "Dual Use"
  # Package:    "Boruta randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "Boruta"
  model$Description <- "Random Forest with Additional Feature Selection"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry")
  model$slowTrain <- FALSE
  model$package <- c("Boruta randomForest")
  return (model)
}

getModel.cforest<-function(){
  # Name:       "Conditional Inference Random Forest"
  # Algorithm:  "cforest"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "mtry" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "cforest"
  model$Description <-  "Conditional Inference Random Forest"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry")
  model$slowTrain <- FALSE
  model$package <- c("party")
  return (model)
}

getModel.extraTrees<-function(){
  # Name:       "Random Forest by Randomization"
  # Algorithm:  "extraTrees"
  # Type:       "Dual Use"
  # Package:    "extraTrees"
  # Params:     "mtry, numRandomCuts" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "extraTrees"
  model$Description <- "Random Forest by Randomization"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry","numRandomCuts")
  model$slowTrain <- FALSE
  model$package <- c("extraTrees")
  return (model)
}

getModel.parRF<-function(){
  # Name:       "Parallel Random Forest"
  # Algorithm:  "parRF"
  # Type:       "Dual Use"
  # Package:    "randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "parRF"
  model$Description <- "Parallel Random Forest"
  model$featureSelection <- FALSE
  model$type <- "FMRandomForests"
  model$params <- c("mtry")
  model$slowTrain <- FALSE
  model$package <- c("randomForest")
  return (model)
}

getModel.qrf<-function(){
  # Name:       "Quantile Random Forest"
  # Algorithm:  "qrf"
  # Type:       "Regression"
  # Package:    "quantregForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "qrf"
  model$Description <- "Quantile Random Forest"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry")
  model$slowTrain <- FALSE
  model$package <- c("quantregForest")
  return (model)
}

getModel.RRF<-function(){
  # Name:       "Regularized Random Forest"
  # Algorithm:  "RRF"
  # Type:       "Dual Use"
  # Package:    "randomForest, RRF"
  # Params:     "mtry, coefReg, coefImp" 
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "RRF"
  model$Description <- "Regularized Random Forest"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry","coefReg","coefImp")
  model$slowTrain <- FALSE
  model$package <- c("randomForest", "RRF")
  return (model)
}
getModel.RRFglobal<-function(){ 
  # Name:       "Regularized Random Forest"
  # Algorithm:  "RRFglobal"
  # Type:       "Dual Use"
  # Package:    "RRF"
  # Params:     "mtry, coefReg"
  # Family:     "Random Forests"
  model <- FMModel()
  model$name <- "RRFglobal"
  model$Description <- "Regularized Random Forest"
  model$featureSelection <- TRUE
  model$type <- "FMRandomForests"
  model$params <- c("mtry","coefReg")
  model$slowTrain <- FALSE
  model$package <- c("RRF")
  return (model)
}