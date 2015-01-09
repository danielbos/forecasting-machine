FMAdditive <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMAdditive")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMAdditive <- function(){
  allModels <- list(getModel.gam(), 
           getModel.gamboost(),
           getModel.gamLoess(),
           getModel.gamSpline(),
           getModel.ppr())
  return(allModels)
}


getModel.gam<-function(){
  # Name:       "Generalized Additive Model using Splines"
  # Algorithm:  "gam"
  # Type:       "Dual Use"
  # Package:    "mgcv"
  # Params:     "select, method" 
  # Family:     "Additive Model"
  model <- FMModel()
  model$name <- "gam"
  model$Description <- "Generalized Additive Model using Splines"
  model$featureSelection <- FALSE
  model$type <- "FMAdditive"
  model$params <- c( "select", "method" )
  model$slowTrain <- FALSE
  model$package <- c("mgcv")
  return (model)
}
getModel.gamboost<-function(){
  # Name:        "Boosted Generalized Additive Model"
  # Algorithm:   "gamboost"
  # Type:        "Dual Use"
  # Package:     "mboost"
  # Params:      "mstop, prune" 
  # Family:      "Additive Model"
  model <- FMModel()
  model$name <- "gamboost"
  model$Description <- "Boosted Generalized Additive Model"
  model$featureSelection <- TRUE
  model$type <- "FMAdditive"
  model$params <- c( "mstop", "prune" )
  model$slowTrain <- FALSE
  model$package <- c("mboost")
  return (model)
}
getModel.gamLoess<-function(){
  # Name:        "Generalized Additive Model using LOESS"
  # Algorithm:   "gamLoess"
  # Type:        "Dual Use"
  # Package:     "gam"
  # Params:      "span, degree" 
  # Family:      "Additive Model"
  model <- FMModel()
  model$name <- "gamLoess"
  model$Description <- "Generalized Additive Model using LOESS"
  model$featureSelection <- FALSE
  model$type <- "FMAdditive"
  model$params <- c( "span", "degree" )
  model$slowTrain <- FALSE
  model$package <- c("gam")
  return (model)
}
getModel.gamSpline<-function(){
  # Name:        "Generalized Additive Model using Splines"
  # Algorithm:   "gamSpline"
  # Type:        "Dual Use"
  # Package:     "gam"
  # Params:      "df" 
  # Family:      "Additive Model"
  model <- FMModel()
  model$name <- "gamSpline"
  model$Description <- "Generalized Additive Model using Splines"
  model$featureSelection <- FALSE
  model$type <- "FMAdditive"
  model$params <- c( "df" )
  model$slowTrain <- FALSE
  model$package <- c("gam")
  return (model)
}

getModel.ppr<-function(){
  # Name:       "Projection Pursuit Regression"
  # Algorithm:  "ppr"
  # Type:       "Regression"
  # Package:    ""
  # Params:     "nterms" 
  # Family:     
  model <- FMModel()
  model$name <- "ppr"
  model$Description <- "Projection Pursuit Regression"
  model$featureSelection <- FALSE
  model$type <- "FMAdditive"
  model$params <- c( "nterms" )
  model$slowTrain <- FALSE
  model$package <- NA
  return (model)
}