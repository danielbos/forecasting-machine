FMNeuralNetworks <- function(newmodels=list()){
  me <- FMRegressor()
  class(me) <- append(class(me),"FMNeuralNetworks")
  me$models <- if(length(newmodels) != 0) newmodels else getFMModels(me)  
  return(me)
}

getFMModels.FMNeuralNetworks <- function(){
  allModels <- list(getModel.avNNet(),
                    getModel.brnn(),
                    getModel.dnn(),
                    getModel.mlp(),
                    getModel.mlpWeightDecay(),
                    getModel.neuralnet(),
                    getModel.nnet(),
                    getModel.pcaNNet(),
                    getModel.qrnn(),
                    getModel.ANFIS(),
                    getModel.DENFIS(),
                    getModel.rbfDDA(),
                    getModel.elm())
  return (allModels)
}
getModel.avNNet<- function(){
  # Name:       "Model Averaged Neural Network"
  # Algorithm:  "avNNet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size decay bag" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "avNNet"
  model$Description <- "Model Averaged Neural Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("size", "decay", "bag") 
  model$slowTrain <- FALSE
  model$package <- c("nnet")
  return (model)
}


getModel.brnn<-function(){ 
  # Name:       "Bayesian Regularized Neural Networks"
  # Algorithm:  "brnn"
  # Type:       "Regression"
  # Package:    "brnn"
  # Params:     "neurons" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "brnn"
  model$Description <- "Bayesian Regularized Neural Networks"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("neurons")
  model$slowTrain <- FALSE
  model$package <- c("brnn")
  return (model)
}

getModel.dnn<-function(){ 
  # Name:       "Stacked AutoEncoder Deep Neural Network"
  # Algorithm:  "dnn"
  # Type:       "Dual Use"
  # Package:    "deepnet"
  # Params:     "layer1 layer2 layer3 hidden_dropout visible_dropout" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "dnn"
  model$Description <- "Stacked AutoEncoder Deep Neural Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("layer1", "layer2", "layer3", "hidden_dropout", "visible_dropout") 
  model$slowTrain <- FALSE
  model$package <- c("deepnet")
  return (model)
}

getModel.mlp<-function(){
  # Name:       "Multi-Layer Perceptron"
  # Algorithm:  "mlp"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "size" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "mlp"
  model$Description <- "Multi-Layer Perceptron"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("size")
  model$slowTrain <- TRUE
  model$package <- c("RSNNS")
  return (model)
}

getModel.mlpWeightDecay<-function(){
  # Name:       "Multi-Layer Perceptron"
  # Algorithm:  "mlpWeightDecay"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "size, decay" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "mlpWeightDecay"
  model$Description <- "Multi-Layer Perceptron"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("size", "decay") 
  model$slowTrain <- TRUE
  model$package <- c("RSNNS")
  return (model)
}

getModel.neuralnet<-function(){
  # Name:       "Neural Network"
  # Algorithm:  "neuralnet"
  # Type:       "Regression"
  # Package:    "neuralnet"
  # Params:     "layer1, layer2, layer3" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "neuralnet"
  model$Description <- "Neural Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <-  c("layer1", "layer2", "layer3")
  model$slowTrain <- TRUE
  model$package <- c("neuralnet")
  return (model)
}

getModel.nnet<-function(){
  # Name:       "Neural Network"
  # Algorithm:  "nnet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size, decay" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "nnet"
  model$Description <- "Neural Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <-c("size","decay")
  model$slowTrain <- TRUE
  model$package <- c("nnet")
  return (model)
}

getModel.pcaNNet<-function(){
  # Name:       "Neural Networks with Feature Extraction"
  # Algorithm:  "pcaNNet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size, decay" 
  # Family:     "Neural Networks"
  model <- FMModel()
  model$name <- "pcaNNET"
  model$Description <- "Neural Networks with Feature Extraction"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("size","decay")
  model$slowTrain <- FALSE
  model$package <- c("nnet")
  return (model)
}

getModel.qrnn<-function(){
  # Name:       "Quantile Regression Neural Network"
  # Algorithm:  "qrnn"
  # Type:       "Regression"
  # Package:    "qrnn"
  # Params:     "n.hidden, penalty, bag" 
  # Family:     "Neural Network"
  model <- FMModel()
  model$name <- "qrnn"
  model$Description <- "Quantile Regression Neural Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("n.hidden", "penalty", "bag") 
  model$slowTrain <- TRUE
  model$package <- c("qrnn")
  return (model)
}

getModel.ANFIS<- function(){ 
  # Name:      "Adaptive-Network-Based Fuzzy Inference System"
  # Algorithm: "ANFIS"
  # Type:      "Regression"
  # Package:   "frbs"
  # Params:    "num.labels max.iter"
  # Family:    "Non-Linear"
  model <- FMModel()
  model$name <- "ANFIS"
  model$Description <- "Adaptive-Network-Based Fuzzy Inference System"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("num.labels", "max.iter")
  model$slowTrain <- TRUE
  model$package <- c("frbs")
  return (model)
}

getModel.DENFIS<-function(){ 
  # Name:       "Dynamic Evolving Neural-Fuzzy Inference System "
  # Algorithm:  "DENFIS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "Dthr max.iter" 
  # Family:     
  model <- FMModel()
  model$name <- "DENFIS"
  model$Description <- "Dynamic Evolving Neural-Fuzzy Inference System "
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("Dthr", "max.iter") 
  model$slowTrain <- TRUE
  model$package <- c("frbs")
  return (model)
}

getModel.rbfDDA<-function(){
  # Name:       "Radial Basis Function Network"
  # Algorithm:  "rbfDDA"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "negativeThreshold" 
  # Family:     
  model <- FMModel()
  model$name <- "rbfDDA"
  model$Description <- "Radial Basis Function Network"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("negativeThreshold")
  model$slowTrain <- TRUE
  model$package <- c("RSNNS")
  return (model)
}
getModel.elm<-function(){ 
  # Name:       "Extreme Learning Machine"
  # Algorithm:  "elm"
  # Type:       "Dual Use"
  # Package:    "elmNN"
  # Params:     "nhid actfun" 
  # Family:     
  model <- FMModel()
  model$name <- "elmNN"
  model$Description <- "Extreme Learning Machine"
  model$featureSelection <- FALSE
  model$type <- "FMNeuralNetworks"
  model$params <- c("nhid", "actfun") 
  model$slowTrain <- TRUE
  model$package <- c("elmNN")
  return (model)
}
