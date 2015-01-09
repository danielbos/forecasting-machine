FMLinearRegressor <- function(newmodels=list()){
  
  me <- FMRegressor()
  class(me) <- append(class(me),"FMLinearRegressor")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)
  
  return(me)
}

getFMModels.FMLinearRegressor <- function(){
  allModels <- list(
          getModel.bayesglm(),
          getModel.bstLs(),
          getModel.enet(),
          getModel.foba(),
          getModel.glm(),
          getModel.glmboost(),
          getModel.glmnet(),
          getModel.glmStepAIC(),
          getModel.leapBackward(),
          getModel.krlsPoly(),
          getModel.icr(),
          getModel.bstSm(),
          getModel.krlsRadial(),
          getModel.widekernelpls(),
          getModel.spls(),
          getModel.simpls(),
          getModel.rlm(),
          getModel.ridge(),
          getModel.relaxo(),
          getModel.plsRglm(),
          getModel.pls(),
          getModel.penalized(),
          getModel.lmStepAIC(),
          getModel.lm(),
          getModel.leapSeq(),
          getModel.leapForward(),
          getModel.lasso(),
          getModel.lars2(),
          getModel.lars(),
          getModel.kernelpls())
  
  return(allModels)
  
}


getModel.bayesglm <- function(){
  # Name:        "Bayesian Generalized Linear Model"
  # Algorithm:   "bayesglm"
  # Type:        "Dual Use"
  # Package:     "arm"
  # Params:      "None" 
  # Family:  
  model <- FMModel()
  model$name <- "bayesglm"
  model$Description <- "Bayesian Generalized Linear Model"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- c("arm")
  return (model)
}


getModel.bstLs <- function(){ 
  # Name:       "Boosted Linear Model"
  # Algorithm:  "bstLs"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop nu" 
  # Family:     
  model <- FMModel()
  model$name <- "bstLs"
  model$Description <- "Boosted Linear Model"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("mstop", "nu")
  model$slowTrain <- FALSE
  model$package <- c("bst", "plyr")
  
  return (model)
}

getModel.enet<-function(){ 
  # Name:       "Elasticnet"
  # Algorithm:  "enet"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "fraction lambda" 
  # Family:     "Linear Regression"
  model$name <- "enet"
  model$Description <- "Elasticnet"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("fraction", "lambda")
  model$slowTrain <- FALSE
  model$package <- c("elasticnet")
  
  return (model)
}

getModel.foba<-function(){
  # Name:       "Ridge Regression with Variable Selection"
  # Algorithm:  "foba"
  # Type:       "Regression"
  # Package:    "foba"
  # Params:     "k, lambda" 
  # Family:     "Linear Regression"
  model$name <- "foba"
  model$Description <- "Ridge Regression with Variable Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("k", "lambda")
  model$slowTrain <- FALSE
  model$package <- c("foba")
  
  return (model)
}

getModel.glm<-function(){
  # Name:       "Generalized Linear Model"
  # Algorithm:  "glm"
  # Type:       "Dual Use"
  # Package:    ""
  # Params:     "None" 
  # Family:     "Linear Regression"
  model$name <- "glm"
  model$Description <- "Generalized Linear Model"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- NA
  
  return (model)
}       
getModel.glmboost<-function(){
  # Name:       "Boosted Generalized Linear Model"
  # Algorithm:  "glmboost"
  # Type:       "Dual Use"
  # Package:    "mboost"
  # Params:     "mstop, prune" 
  # Family:     "Linear Regression"
  model$name <- "glmboost"
  model$Description <- "Boosted Generalized Linear Model"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("mstop","prune")
  model$slowTrain <- FALSE
  model$package <- c("mboost")
  return(model)
}


getModel.glmnet<-function(){
  # Name:       "glmnet"
  # Algorithm:  "glmnet"
  # Type:       "Dual Use"
  # Package:    "glmnet"
  # Params:     "alpha, lambda" 
  # Family:     "Linear Regression"
  model$name <- "glmnet"
  model$Description <- "glmnet"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("alpha","lambda")
  model$slowTrain <- FALSE
  model$package <- c("glmnet")
  return(model)
}             
              
getModel.glmStepAIC<-function(){
  # Name:       "Generalized Linear Model with Stepwise Feature Selection"
  # Algorithm:  "glmStepAIC"
  # Type:       "Dual Use"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:     "Linear Regression"
  model$name <- "glmStepAIC"
  model$Description <- "Generalized Linear Model with Stepwise Feature Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- TRUE
  model$package <- c("MASS")
  return(model)
}              
 
getModel.leapBackward<-function(){
  # Name:       "Linear Regression with Backwards Selection"
  # Algorithm:  "leapBackward"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax" 
  # Family:     "Linear Regression"
  model$name <- "leapBackward"
  model$Description <- "Linear Regression with Backwards Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("nvmax")
  model$slowTrain <- FALSE
  model$package <- c("leaps")
  return(model)
}

getModel.kernelpls<-function(){
  # Name:       "Kernel Partial Least Squares"
  # Algorithm:  "kernelpls"
  # Type:       "Dual Use"
  # Package:    "pls"
  # Params:     "ncomp" 
  # Family:     "Linear Regression"
  model$name <- "kernelpls"
  model$Description <- "Kernel Partial Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("ncomp")
  model$slowTrain <- FALSE
  model$package <- c("pls")
  return(model)
}

getModel.lars<-function(){
  # Name:       "Least Angle Regression"
  # Algorithm:  "lars"
  # Type:       "Regression"
  # Package:    "lars"
  # Params:     "fraction" 
  # Family:    
  model$name <- "lars"
  model$Description <- "Least Angle Regression"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("fraction")
  model$slowTrain <- FALSE
  model$package <- c("lars")
  return(model)
}



getModel.lars2<-function(){
  # Name:       "Least Angle Regression"
  # Algorithm:  "lars2"
  # Type:       "Regression"
  # Package:    "lars"
  # Params:     "step" 
  # Family:    
  model$name <- "lars2"
  model$Description <- "Least Angle Regression"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("step")
  model$slowTrain <- FALSE
  model$package <- c("lars")
  return(model)
}

getModel.lasso<-function(){
  # Name:       "The lasso"
  # Algorithm:  "lasso"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "fraction" 
  # Family:    
  model$name <- "lasso"
  model$Description <- "The lasso"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("fraction")
  model$slowTrain <- FALSE
  model$package <- c("elasticnet")
  return(model)
}


getModel.leapForward<-function(){
  # Name:       "Linear Regression with Forward Selection"
  # Algorithm:  "leapForward"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax"
  # Family:     "Linear Regression"
  model$name <- "leapForward"
  model$Description <- "Linear Regression with Forward Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("nvmax")
  model$slowTrain <- FALSE
  model$package <- c("leaps")
  return(model)
}


getModel.leapSeq<-function(){
  # Name:       "Linear Regression with Stepwise Selection"
  # Algorithm:  "leapSeq"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax"
  # Family:     "Linear Regression"
  model$name <- "leapSeq"
  model$Description <- "Linear Regression with Stepwise Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("nvmax")
  model$slowTrain <- FALSE
  model$package <- c("leaps")
  return(model)
}

getModel.lm<-function(){
  # Name:       "Linear Regression"
  # Algorithm:  "lm"
  # Type:       "Regression"
  # Package:    ""
  # Params:     "None" 
  # Family:     "Linear Regression"
  model$name <- "lm"
  model$Description <- "Linear Regression"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- FALSE
  model$package <- NA
  return(model)
}

getModel.lmStepAIC<-function(){
  # Name:       "Linear Regression with Stepwise Selection"
  # Algorithm:  "lmStepAIC"
  # Type:       "Regression"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:     "Linear Regression"
  model$name <- "lmStepAIC"
  model$Description <- "Linear Regression with Stepwise Selection"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- TRUE
  model$package <- c("MASS")
  return(model)
}

getModel.penalized<-function(){
  # Name:       "Penalized Linear Regression"
  # Algorithm:  "penalized"
  # Type:       "Regression"
  # Package:    "penalized"
  # Params:     "lambda1, lambda2" 
  # Family:     "Linear Regression"
  model$name <- "penalized"
  model$Description <- "Penalized Linear Regression"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("lambda1, lambda2")
  model$slowTrain <- TRUE
  model$package <- c("penalized")
  return(model)
}

getModel.pls<-function(){
  # Name:       "Partial Least Squares"
  # Algorithm:  "pls"
  # Type:       "Dual Use"
  # Package:    "pls"
  # Params:     "ncomp" 
  # Family:    
  model$name <- "pls"
  model$Description <- "Partial Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("ncomp")
  model$slowTrain <- FALSE
  model$package <- c("pls")
  return(model)
}

getModel.plsRglm<-function(){
  # Name:       "Partial Least Squares Generalized Linear Models "
  # Algorithm:  "plsRglm"
  # Type:       "Dual Use"
  # Package:    "plsRglm"
  # Params:     "nt, alpha.pvals.expli" 
  # Family:    
  model$name <- "plsRglm"
  model$Description <- "Partial Least Squares Generalized Linear Models "
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("nt", "alpha.pvals.expli")
  model$slowTrain <- TRUE
  model$package <- c("plsRglm")
  return(model)
}

getModel.relaxo<-function(){
  # Name:       "Relaxed Lasso"
  # Algorithm:  "relaxo"
  # Type:       "Regression"
  # Package:    "relaxo plyr"
  # Params:     "lambda phi" 
  # Family:     "Linear Regression"
  model$name <- "relaxo"
  model$Description <- "Relaxed Lasso"
  model$featureSelection <- TRUE
  model$type <- "FMLinearRegressor"
  model$params <- c("lambda","phi")
  model$slowTrain <- FALSE
  model$package <- c("relaxo", "plyr")
  return(model)
}

getModel.ridge<-function(){
  # Name:       "Ridge Regression"
  # Algorithm:  "ridge"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "lambda" 
  # Family:     "Linear Regression"
  model$name <- "ridge"
  model$Description <- "Ridge Regression"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("lambda")
  model$slowTrain <- FALSE
  model$package <- c("elasticnet")
  return(model)
}

getModel.rlm<-function(){
  # Name:       "Robust Linear Model"
  # Algorithm:  "rlm"
  # Type:       "Regression"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:    
  model$name <- "rlm"
  model$Description <- "Robust Linear Model"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- NA
  model$slowTrain <- TRUE
  model$package <- c("MASS")
  return(model)
}

getModel.simpls<-function(){
  # Name:        "Partial Least Squares"
  # Algorithm:   "simpls"
  # Type:        "Dual Use"
  # Package:     "pls"
  # Params:      "ncomp" 
  # Family:    
  model$name <- "simpls"
  model$Description <- "Partial Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("ncomp")
  model$slowTrain <- TRUE
  model$package <- c("pls")
  return(model)
}
 

getModel.spls<-function(){
  # Name:        "Sparse Partial Least Squares"
  # Algorithm:   "spls"
  # Type:        "Dual Use"
  # Package:     "spls"
  # Params:      "K, eta, kappa" 
  # Family:    
  model$name <- "spls"
  model$Description <- "Sparse Partial Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("K", "eta", "kappa")
  model$slowTrain <- FALSE
  model$package <- c("spls")
  return(model)
}

getModel.widekernelpls<-function(){
  # Name:        "Partial Least Squares"
  # Algorithm:   "widekernelpls"
  # Type:        "Dual Use"
  # Package:     "pls"
  # Params:      "ncomp" 
  # Family:    
  model$name <- "widekernelpls"
  model$Description <- "Wide Kernel Partial Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("ncomp")
  model$slowTrain <- TRUE
  model$package <- c("pls")
  return(model)
}

getModel.krlsRadial<-function(){
  # Name:       "Radial Basis Function Kernel Regularized Least Squares"
  # Algorithm:  "krlsRadial"
  # Type:       "Regression"
  # Package:    "KRLS kernlab"
  # Params:     "lambda, sigma" 
  # Family:    
  model$name <- "krlsRadial"
  model$Description <- "Radial Basis Function Kernel Regularized Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("lambda", "sigma")
  model$slowTrain <- TRUE
  model$package <- c("KRLS", "kernlab")
  return(model)
}


getModel.krlsPoly<-function(){
  # Name:       "Polynomial Kernel Regularized Least Squares"
  # Algorithm:  "krlsPoly"
  # Type:       "Regression"
  # Package:    "KRLS"
  # Params:     "lambda, degree" 
  # Family:     ""
  model$name <- "krlsPoly"
  model$Description <- "Polynomial Kernel Regularized Least Squares"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("lambda", "degree")
  model$slowTrain <- TRUE
  model$package <- c("KRLS")
  return(model)
}

getModel.bstSm<-function(){ 
  # Name:       "Boosted Smoothing Spline"
  # Algorithm:  "bstSm"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop nu" 
  # Family:    
  model$name <- "bstSm"
  model$Description <-"Boosted Smoothing Spline"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("mstop","nu")
  model$slowTrain <- TRUE
  model$package <- c("bst", "plyr")
  return(model)
}

getModel.icr<-function(){
  # Name:       "Independent Component Regression"
  # Algorithm:  "icr"
  # Type:       "Regression"
  # Package:    "fastICA"
  # Params:     "n.comp" 
  # Family:
  model$name <- "icr"
  model$Description <- "Independent Component Regression"
  model$featureSelection <- FALSE
  model$type <- "FMLinearRegressor"
  model$params <- c("n.comp")
  model$slowTrain <- FALSE
  model$package <- c("fastICA")
  return(model)
}





