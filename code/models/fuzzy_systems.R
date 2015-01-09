FMFuzzySystems <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMFuzzySystems")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMFuzzySystems <- function(){
  allModels <- list(getModel.GFS.FR.MOGAL(),
                    getModel.GFS.LT.RS(), 
                    getModel.GFS.Thrift(),
                    getModel.HYFIS(),
                    getModel.WM(),
                    getModel.FIR.DM(),
                    getModel.FS.HGD())
  return(allModels)
}

getModel.GFS.FR.MOGAL<-function(){
  # Name:       "Fuzzy Rules via MOGUL"
  # Algorithm:  "GFS.FR.MOGAL"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "max.gen, max.iter, max.tune" 
  # Family:     
  model <- FMModel()
  model$name <-  "GFS.FR.MOGAL"
  model$Description <- "Fuzzy Rules via MOGUL"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("max.gen", "max.iter", "max.tune" )
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}        
getModel.GFS.LT.RS<-function(){
  # Name:       "Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems"
  # Algorithm:  "GFS.LT.RS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "popu.size, num.labels, max.gen" 
  # Family:     
  model <- FMModel()
  model$name <- "GFS.LT.RS"
  model$Description <- "Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("popu.size", "num.labels", "max.gen") 
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}       
getModel.GFS.Thrift<-function(){
  # Name:       "Fuzzy Rules via Thrift"
  # Algorithm:  "GFS.Thrift"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "popu.size, num.labels, max.gen" 
  # Family:     
  model <- FMModel()
  model$name <- "GFS.Thrift"
  model$Description <- "Fuzzy Rules via Thrift"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("popu.size", "num.labels", "max.gen") 
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}       

getModel.HYFIS<-function(){
  # Name:       "Hybrid Neural Fuzzy Inference System"
  # Algorithm:  "HYFIS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels, max.iter" 
  # Family:     
  model <- FMModel()
  model$name <- "HYFIS"
  model$Description <- "Hybrid Neural Fuzzy Inference System"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("num.labels", "max.iter" )
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}

getModel.WM<-function(){
  # Name:        "Wang and Mendel Fuzzy Rules"
  # Algorithm:   "WM"
  # Type:        "Regression"
  # Package:     "frbs"
  # Params:      "num.labels, type.mf" 
  # Family:     
  model <- FMModel()
  model$name <- "WM"
  model$Description <- "Wang and Mendel Fuzzy Rules"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("num.labels", "type.mf" )
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}


getModel.FIR.DM<-function(){
  # Name:       "Fuzzy Inference Rules by Descent Method"
  # Algorithm:  "FIR.DM"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels, max.iter" 
  # Family:     
  model <- FMModel()
  model$name <- "FIR.DM"
  model$Description <- "Fuzzy Inference Rules by Descent Method"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <- c("num.labels", "max.iter" )
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}

getModel.FS.HGD<-function(){
  # Name:       "Simplified TSK Fuzzy Rules"
  # Algorithm:  "FS.HGD"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels max.iter" 
  # Family:    
  model <- FMModel()
  model$name <- "FS.HGD"
  model$Description <- "Simplified TSK Fuzzy Rules"
  model$featureSelection <- FALSE
  model$type <- "FMFuzzySystems"
  model$params <-  c("num.labels", "max.iter" )
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}