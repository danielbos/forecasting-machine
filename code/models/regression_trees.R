source("models/fmregressor.R")

FMRegressionTrees<- function(newmodels=list()){
  me <- FMRegressor()
  class(me) <- append(class(me),"FMRegressionTrees")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMRegressionTrees <- function(obj){
  allModels <- list(getModel.blackboost(),
                 getModel.bstTree(),
                 getModel.ctree(),
                 getModel.ctree2(),
                 getModel.evtree(),
                 getModel.M5(),
                 getModel.M5Rules(),
                 getModel.nodeHarvest(),
                 getModel.rpart(),
                 getModel.rpart2(),
                 getModel.gbm(),
                 getModel.treebag(),
                 getModel.cubist())
  
  return(allModels)
} 

getModel.blackboost <- function(){ 
  # Name:       "Boosted Tree"
  # Algorithm:  "blackboost"
  # Type:       "Dual Use"
  # Package:    "party mboost plyr"
  # Params:     "mstop maxdepth" 
  # Family:     
  model <- FMModel()
  model$name <- "blackboost"
  model$description <- "Boosted Tree"
  model$featureSelection <- FALSE
  model$type <- "FMRegressionTrees"
  model$params <- c("mstop", "maxdepth")
  model$slowTrain <- FALSE
  model$package <- c("party", "mboost", "plyr")
  return (model)
}


getModel.bstTree<-function(){ 
  # Name:       "Boosted Tree"
  # Algorithm:  "bstTree"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop maxdepth nu" 
  # Family:     
  model <- FMModel()
  model$name <- "bstTree"
  model$description <- "Boosted Tree"
  model$featureSelection <- FALSE
  model$type <- "FMRegressionTrees"
  model$params <- c("mstop", "maxdepth", "nu") 
  model$slowTrain <- FALSE
  model$package <- c("bst", "plyr")
  return (model)
}

getModel.ctree<-function(){ 
  # Name:       "Conditional Inference Tree"
  # Algorithm:  "ctree"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "mincriterion" 
  # Family:     
  model <- FMModel()
  model$name <- "ctree"
  model$description <- "Conditional Inference Tree"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("mincriterion")
  model$slowTrain <- FALSE
  model$package <- c("party")
  return (model)
}
getModel.ctree2<-function(){ 
  # Name:       "Conditional Inference Tree"
  # Algorithm:  "ctree2"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "maxdepth" 
  # Family:     
  model <- FMModel()
  model$name <- "ctree2"
  model$description <- "Conditional Inference Tree"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("maxdepth")
  model$slowTrain <- FALSE
  model$package <- c("party")
  return (model)
}

getModel.evtree<-function(){ 
  # Name:       "Tree Models from Genetic Algorithms"
  # Algorithm:  "evtree"
  # Type:       "Dual Use"
  # Package:    "evtree"
  # Params:     "alpha" 
  # Family:     
  model <- FMModel()
  model$name <- "evtree"
  model$description <- "Tree Models from Genetic Algorithms"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("alpha")
  model$slowTrain <- FALSE
  model$package <- c("evtree")
  return (model)
}

getModel.M5<-function(){
  # Name:       "Model Tree"
  # Algorithm:  "M5"
  # Type:       "Regression"
  # Package:    "RWeka"
  # Params:     "pruned, smoothed, rules" 
  # Family:     
  model <- FMModel()
  model$name <- "M5"
  model$description <- "Model Tree"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("pruned", "smoothed", "rules")
  model$slowTrain <- FALSE
  model$package <- c("RWeka")
  return (model)
}

getModel.M5Rules<-function(){ 
  # Name:       "Model Rules"
  # Algorithm:  "M5Rules"
  # Type:       "Regression"
  # Package:    "RWeka"
  # Params:     "pruned, smoothed" 
  # Family:     
  model <- FMModel()
  model$name <- "M5Rules"
  model$description <- "Model Rules"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("pruned", "smoothed")
  model$slowTrain <- FALSE
  model$package <- c("RWeka")
  return (model)
}


getModel.nodeHarvest<-function(){
  # Name:       "Tree-Based Ensembles"
  # Algorithm:  "nodeHarvest"
  # Type:       "Dual Use"
  # Package:    "nodeHarvest"
  # Params:     "maxinter, mode" 
  # Family:     
  model <- FMModel()
  model$name <- "nodeHarvest"
  model$description <- "Tree-Based Ensembles"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("maxinter", "mode")
  model$slowTrain <- FALSE
  model$package <- c("nodeHarvest")
  return (model)
}             

getModel.rpart<-function(){
  # Name:       "CART"
  # Algorithm:  "rpart"
  # Type:       "Dual Use"
  # Package:    "rpart"
  # Params:     "cp" 
  # Family:     
  model <- FMModel()
  model$name <- "rpart"
  model$description <- "CART"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("cp")
  model$slowTrain <- FALSE
  model$package <- c("rpart")
  return (model)
}

getModel.rpart2<-function(){
  # Name:       "CART"
  # Algorithm:  "rpart2"
  # Type:       "Dual Use"
  # Package:    "rpart"
  # Params:     "maxdepth" 
  # Family:     
  model <- FMModel()
  model$name <- "rpart2"
  model$description <- "CART"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("maxdepth")
  model$slowTrain <- FALSE
  model$package <- c("rpart")
  return (model)
}


getModel.gbm<-function(){
  # Name:       "Stochastic Gradient Boosting"
  # Algorithm:  "gbm"
  # Type:       "Dual Use"
  # Package:    "gbm, plyr"
  # Params:     "n.trees, interaction.depth, shrinkage" 
  # Family:     
  model <- FMModel()
  model$name <- "gbm"
  model$description <- "Stochastic Gradient Boosting"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("n.trees", "interaction.depth", "shrinkage") 
  model$slowTrain <- FALSE
  model$package <- c("gbm", "plyr")
  return (model)
}

getModel.treebag<-function(){
  # Name:        "Bagged CART"
  # Algorithm:   "treebag"
  # Type:        "Dual Use"
  # Package:     "ipred plyr"
  # Params:      "None" 
  # Family:     
  model <- FMModel()
  model$name <- "treebag"
  model$description <- "Bagged CART"
  model$featureSelection <- FALSE
  model$type <- "FMRegressionTrees"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("ipred", "plyr")
  return (model)
}

getModel.cubist<-function(){ 
  # Name:       "Cubist"
  # Algorithm:  "cubist"
  # Type:       "Regression"
  # Package:    "Cubist"
  # Params:     "committees neighbors" 
  # Family:     
  model <- FMModel()
  model$name <- "cubist"
  model$description <-  "Cubist"
  model$featureSelection <- TRUE
  model$type <- "FMRegressionTrees"
  model$params <- c("committees", "neighbors")
  model$slowTrain <- FALSE
  model$package <- c("Cubist")
  return (model)
}