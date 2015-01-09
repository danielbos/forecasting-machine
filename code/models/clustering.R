FMClustering <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMClustering")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMClustering <- function(){
  allModels <- list( getModel.rknn(),
            getModel.rknnBel(),
            getModel.kknn(),
            getModel.pcr(),
            getModel.knn(),
            getModel.SBC(),
            getModel.superpc(),
            getModel.pcr())
  return(allModels)  
}


getModel.rknn<-function(){
  # Name:       "Random k-Nearest Neighbors"
  # Algorithm:  "rknn" 
  # Type:       "Dual Use"
  # Package:    "rknn"
  # Params:     "k, mtry" 
  # Family:     "Nearest Neighbors"
  model <- FMModel()
  model$name <- "rknn"
  model$Description <-  "Random k-Nearest Neighbors"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("k", "mtry") 
  model$slowTrain <- FALSE
  model$package <- c("rknn")
  return (model)
}
getModel.rknnBel<-function(){
  # Name:       "Random k-Nearest Neighbors with Feature Selection"
  # Algorithm:  "rknnBel"
  # Type:       "Dual Use"
  # Package:    "rknn, plyr"
  # Params:     "k, mtry, d" 
  # Family:     "Nearest Neighbors"
  model <- FMModel()
  model$name <- "rknnBel"
  model$Description <- "Random k-Nearest Neighbors with Feature Selection"
  model$featureSelection <- TRUE
  model$type <- "FMRegressorCluster"
  model$params <- c("k", "mtry", "d") 
  model$slowTrain <- FALSE
  model$package <- c("rknn", "plyr")
  return (model)
}
getModel.kknn<-function(){
  # Name:       "k-Nearest Neighbors"
  # Algorithm:  "kknn"
  # Type:       "Dual Use"
  # Package:    "kknn"
  # Params:     "kmax, distance, kernel" 
  # Family:     "Nearest Neighbors"
  model <- FMModel()
  model$name <- "kknn"
  model$Description <- "k-Nearest Neighbors"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("kmax", "distance", "kernel") 
  model$slowTrain <- FALSE
  model$package <- c("kknn")
  return (model)
}

getModel.knn<-function(){
  # Name:        "k-Nearest Neighbors"
  # Algorithm:  "knn"
  # Type:       "Dual Use"
  # Package:    ""
  # Params:     "k" 
  # Family:     "Nearest Neighbors"
  model <- FMModel()
  model$name <- "knn"
  model$Description <-  "k-Nearest Neighbors"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("k") 
  model$slowTrain <- FALSE
  model$package <- NA
  return (model)
}


getModel.SBC<-function(){
  # Name:        "Subtractive Clustering and Fuzzy c-Means Rules"
  # Algorithm:   "SBC"
  # Type:        "Regression"
  # Package:     "frbs"
  # Params:      "r.a, eps.high, eps.low" 
  # Family:     
  model <- FMModel()
  model$name <- "SBC"
  model$Description <- "Subtractive Clustering and Fuzzy c-Means Rules"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("r.a", "eps.high", "eps.low") 
  model$slowTrain <- FALSE
  model$package <- c("frbs")
  return (model)
}

getModel.superpc<-function(){
  # Name:        "Supervised Principal Component Analysis"
  # Algorithm:   "superpc"
  # Type:        "Regression"
  # Package:     "superpc"
  # Params:      "threshold n.components" 
  # Family:      "PCA"
  model <- FMModel()
  model$name <- "superpc"
  model$Description <- "Supervised Principal Component Analysis"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("threshold", "n.components") 
  model$slowTrain <- FALSE
  model$package <- c("superpc")
  return (model)
}

getModel.pcr<-function(){
  # Name:       "Principal Component Analysis"
  # Algorithm:  "pcr"
  # Type:       "Regression"
  # Package:    "pls"
  # Params:     "ncomp"
  # Family:     
  model <- FMModel()
  model$name <- "pcr"
  model$Description <- "Principal Component Analysis"
  model$featureSelection <- FALSE
  model$type <- "FMRegressorCluster"
  model$params <- c("ncomp") 
  model$slowTrain <- FALSE
  model$package <- c("pls")
  return (model)
}