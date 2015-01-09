FMModel <- function( newName = "",
                     newDescription = "",
                     newFeatureSelection = FALSE,
                     newType = "",
                     newParams = list(),
                     newSlowTrain = FALSE,
                     newPackage = list()){
  me <- list( name <- newName,
              description <- newDescription,
              featureSelection <- newFeatureSelection,
              type <- newType,
              params <- newParams,
              slowTrain <- newSlowTrain,
              package <- newPackage) 
  ## Set the name for the class
  class(me) <- append(class(me),"FMModel")
  return(me)
}

getFMModels <- function(obj){ UseMethod("getFMModels", obj)}