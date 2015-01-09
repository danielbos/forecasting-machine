getFMModels.SOMap <- function(){
  allModels <- list(getModel.xyf(),
       getModel.bdk())
  return(allModels)
}

FMSOMap <- function(newmodels=list()){
  me <- FMRegressor()
  class(me) <- append(class(me),"FMSOMap")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getModel.xyf<-function(){
  # Name:        "Self-Organizing Maps"
  # Algorithm:   "xyf"
  # Type:        "Dual Use"
  # Package:     "kohonen"
  # Params:      "xdim, ydim, xweight, topo"
  # Family:   
  model <- FMModel()
  model$name <- "xyf"
  model$Description <- "Self-Organizing Maps"
  model$featureSelection <- FALSE
  model$type <- "FMSOMap"
  model$params <- c("xdim", "ydim", "xweight", "topo")
  model$slowTrain <- FALSE
  model$package <- c("kohonen")
  return (model)
}



getModel.bdk <- function(){ 
  # Name:       "Self-Organizing Map"
  # Algorithm:  "bdk"
  # Type:       "Dual Use"
  # Package:    "kohonen"
  # Params:     "xdim ydim xweight topo" 
  # Family:     
  model <- FMModel()
  model$name <- "bdk"
  model$Description <- "Self-Organizing Map"
  model$featureSelection <- FALSE
  model$type <- "FMSOMap"
  model$params <- c("xdim", "ydim", "xweight", "topo")
  model$slowTrain <- FALSE
  model$package <- c("kohonen")
  return (model)
}