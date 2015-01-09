runModel <- function(data option){

  
  switch(option){
    #time series
    case "naive": break
    case "mean":  break
    case "snaive": break
    case "rwf": break
    case "arima": break
    case "hw": break
    #linear regression
    case "lm": break
    case "tslm": break
    case "elasticnet": break
    case "ridge": break
    #non linear
    case "rf": break
    case "nn": break
    case "svm": break
    case "gbm": break
    case "anfis":  break
    case "avNNet": break
    case "bag": break 
    case "bagEarth": break    
    case "bayesglm": break
    case "bdk": break
    case "blackboost": break
    case "Boruta": break
    case "brnn": break
    case "bstLs": break
    case "bstSm": break
    case "bstTree": break
    case "cforest": break
    case "ctree": break
    case "ctree2": break   
    case "cubist": break
    case "DENFIS": break
    case "dnn": break
    case "earth": break
    case "elm": break
    case "enet": break
    case "evtree": break
    case "extraTrees": break  
    case "FIR.DM": break
    case "foba": break       
    case "FS.HGD": break       
    case "gam": break       
    case "gamboost" : break      
    case "gamLoess": break       
    case "gamSpline": break
    case "gaussprLinear": break       
    case "gaussprPoly": break       
    case "gaussprRadial": break       
    case "gbm": break       
    case "gcvEarth": break       
    case "GFS.FR.MOGAL": break        
    case "GFS.LT.RS": break       
    case "GFS.Thrift": break       
    case "glm": break       
    case "glmboost": break       
    case "glmnet": break       
    case "glmStepAIC": break              
    case "HYFIS": break       
    case "icr": break       
    case "kernelpls": break       
    case "kknn": break       
    case "knn": break       
    case "krlsPoly": break       
    case "krlsRadial": break              
    case "lars": break       
    case "lars2": break              
    case "lasso": break
    case "leapBackward": break       
    case "leapForward": break       
    case "leapSeq": break              
    case "lm": break       
    case "lmStepAIC": break              
    case "logicBag": break              
    case "logreg": break              
    case "M5": break       
    case "M5Rules": break              
    case "mlp": break       
    case "mlpWeightDecay": break
    case "neuralnet": break
    case "nnet": break
    case "nodeHarvest": break                     
    case "parRF": break       
    case "partDSA": break       
    case "pcaNNet": break       
    case "pcr": break       
    case "penalized": break       
    case "pls": break       
    case "plsRglm": break       
    case "ppr": break              
    case "qrf": break       
    case "qrnn": break       
    case "rbfDDA": break       
    case "relaxo": break       
    case "rf": break       
    case "ridge": break       
    case "rknn": break       
    case "rknnBel": break       
    case "rlm": break              
    case "rpart": break       
    case "rpart2": break       
    case "RRF": break       
    case "RRFglobal": break              
    case "rvmLinear": break       
    case "rvmPoly": break       
    case "rvmRadial": break       
    case "SBC": break              
    case "simpls": break       
    case "spls": break       
    case "superpc": break       
    case "svmBoundrangeString": break       
    case "svmExpoString": break       
    case "svmLinear": break       
    case "svmPoly": break       
    case "svmRadial": break       
    case "svmRadialCost": break       
    case "svmSpectrumString": break       
    case "treebag": break       
    case "widekernelpls": break       
    case "WM": break       
    case "xyf": break
             
    
    
  }
  
}


model.ts.naive <- function(){
}


model.ts.snaive <- function(){
}


model.ts.rwf <- function(){
}

model.ts.mean <- function(){
}

model.ts.arima <- function(){}
model.ts.hw<- function(){}
model.lm<- function(){}
model.lm.tslm<- function(){}
model.lm.elasticnet<- function(){}
model.lm.ridge<- function(){}
model.rf<- function(){}
model.anfis<- function(){ 
  # Name:      "Adaptive-Network-Based Fuzzy Inference System"
  # Algorithm: "ANFIS"
  # Type:      "Regression"
  # Package:   "frbs"
  # Params:    "num.labels max.iter"
  # Family:    "Non-Linear"
}
model.avNNet<- function(){
  # Name:       "Model Averaged Neural Network"
  # Algorithm:  "avNNet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size decay bag" 
  # Family:     "Neural Networks"
}
model.bag<- function(){
  # Name:       "Bagged Model"
  # Algorithm:  "bag"
  # Type:       "Dual Use"
  # Package:    "caret"
  # Params:     "vars" 
  # Family:     "Non-Linear"
}
model.bagEarth<- function(){
  # Name:        "Bagged MARS"
  # Algorithm:   "bagEarth"
  # Type:        "Dual Use"
  # Package:     "earth"
  # Params:      "nprune degree" 
  # Family:      "Non-Linear"
}
model.bayesglm<- function(){ 
  # Name:        "Bayesian Generalized Linear Model"
  # Algorithm:   "bayesglm"
  # Type:        "Dual Use"
  # Package:     "arm"
  # Params:      "None" 
  # Family:      
}
model.bdk <- function(){ 
  # Name:       "Self-Organizing Map"
  # Algorithm:  "bdk"
  # Type:       "Dual Use"
  # Package:    "kohonen"
  # Params:     "xdim ydim xweight topo" 
  # Family:      
}
model.blackboost <- function(){ 
  # Name:       "Boosted Tree"
  # Algorithm:  "blackboost"
  # Type:       "Dual Use"
  # Package:    "party mboost plyr"
  # Params:     "mstop maxdepth" 
  # Family:     
}
model.Boruta<-function(){ 
  # Name:       "Random Forest with Additional Feature Selection"
  # Algorithm:  "Boruta"
  # Type:       "Dual Use"
  # Package:    "Boruta randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
}
model.brnn<-function(){ 
  # Name:       "Bayesian Regularized Neural Networks"
  # Algorithm:  "brnn"
  # Type:       "Regression"
  # Package:    "brnn"
  # Params:     "neurons" 
  # Family:     "Neural Networks"
}
model.bstLs<-function(){ 
  # Name:       "Boosted Linear Model"
  # Algorithm:  "bstLs"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop nu" 
  # Family:     
}
model.bstSm<-function(){ 
  # Name:       "Boosted Smoothing Spline"
  # Algorithm:  "bstSm"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop nu" 
  # Family:     
}
model.bstTree<-function(){ 
  # Name:       "Boosted Tree"
  # Algorithm:  "bstTree"
  # Type:       "Dual Use"
  # Package:    "bst plyr"
  # Params:     "mstop maxdepth nu" 
  # Family:     
}
model.cforest<-function(){
  # Name:       "Conditional Inference Random Forest"
  # Algorithm:  "cforest"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "mtry" 
  # Family:     "Random Forests"
}
model.ctree<-function(){ 
  # Name:       "Conditional Inference Tree"
  # Algorithm:  "ctree"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "mincriterion" 
  # Family:     
}
model.ctree2<-function(){ 
  # Name:       "Conditional Inference Tree"
  # Algorithm:  "ctree2"
  # Type:       "Dual Use"
  # Package:    "party"
  # Params:     "maxdepth" 
  # Family:     
}
model.cubist<-function(){ 
  # Name:       "Cubist"
  # Algorithm:  "cubist"
  # Type:       "Regression"
  # Package:    "Cubist"
  # Params:     "committees neighbors" 
  # Family:     
}
model.DENFIS<-function(){ 
  # Name:       "Dynamic Evolving Neural-Fuzzy Inference System "
  # Algorithm:  "DENFIS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "Dthr max.iter" 
  # Family:     
}
model.dnn<-function(){ 
  # Name:       "Stacked AutoEncoder Deep Neural Network"
  # Algorithm:  "dnn"
  # Type:       "Dual Use"
  # Package:    "deepnet"
  # Params:     "layer1 layer2 layer3 hidden_dropout visible_dropout" 
  # Family:     "Neural Networks"
}
model.earth<-function(){ 
  # Name:       "Multivariate Adaptive Regression Spline"
  # Algorithm:  "earth"
  # Type:       "Dual Use"
  # Package:    "earth"
  # Params:     "nprune degree" 
  # Family:     
}
model.elm<-function(){ 
  # Name:       "Extreme Learning Machine"
  # Algorithm:  "elm"
  # Type:       "Dual Use"
  # Package:    "elmNN"
  # Params:     "nhid actfun" 
  # Family:     
}
model.enet<-function(){ 
  # Name:       "Elasticnet"
  # Algorithm:  "enet"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "fraction lambda" 
  # Family:     "Linear Regression"
}
model.evtree<-function(){ 
  # Name:       "Tree Models from Genetic Algorithms"
  # Algorithm:  "evtree"
  # Type:       "Dual Use"
  # Package:    "evtree"
  # Params:     "alpha" 
  # Family:     
}
model.extraTrees<-function(){
  # Name:       "Random Forest by Randomization"
  # Algorithm:  "extraTrees"
  # Type:       "Dual Use"
  # Package:    "extraTrees"
  # Params:     "mtry, numRandomCuts" 
  # Family:     "Random Forests"
}
model.FIR.DM<-function(){
  # Name:       "Fuzzy Inference Rules by Descent Method"
  # Algorithm:  "FIR.DM"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels, max.iter" 
  # Family:     
}
model.foba<-function(){
  # Name:       "Ridge Regression with Variable Selection"
  # Algorithm:  "foba"
  # Type:       "Regression"
  # Package:    "foba"
  # Params:     "k, lambda" 
  # Family:     "Linear Regression"
}
model.FS.HGD<-function(){
  # Name:       "Simplified TSK Fuzzy Rules"
  # Algorithm:  "FS.HGD"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels max.iter" 
  # Family:     
}
model.gam<-function(){
  # Name:       "Generalized Additive Model using Splines"
  # Algorithm:  "gam"
  # Type:       "Dual Use"
  # Package:    "mgcv"
  # Params:     "select, method" 
  # Family:     "Additive Model"
}
model.gamboost<-function(){
  # Name:        "Boosted Generalized Additive Model"
  # Algorithm:   "gamboost"
  # Type:        "Dual Use"
  # Package:     "mboost"
  # Params:      "mstop, prune" 
  # Family:      "Additive Model"
}
model.gamLoess<-function(){
  # Name:        "Generalized Additive Model using LOESS"
  # Algorithm:   "gamLoess"
  # Type:        "Dual Use"
  # Package:     "gam"
  # Params:      "span, degree" 
  # Family:      "Additive Model"
}
model.gamSpline<-function(){
  # Name:        "Generalized Additive Model using Splines"
  # Algorithm:   "gamSpline"
  # Type:        "Dual Use"
  # Package:     "gam"
  # Params:      "df" 
  # Family:      "Additive Model"
}
model.gaussprLinear<-function(){
  # Name:        "Gaussian Process"
  # Algorithm:   "gaussprLinear"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "None" 
  # Family:      "Gaussian Process"
}
model.gaussprPoly<-function(){
  # Name:        "Gaussian Process with Polynomial Kernel"
  # Algorithm:   "gaussprPoly"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "degree, scale" 
  # Family:      "Gaussian Process"
}
model.gaussprRadial<-function(){
  # Name:        "Gaussian Process with Radial Basis Function Kernel"
  # Algorithm:   "gaussprRadial"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "sigma" 
  # Family:      "Gaussian Process"
}
model.gbm<-function()
  {# Name:      "Stochastic Gradient Boosting"
  # Algorithm:  "gbm"
  # Type:       "Dual Use"
  # Package:    "gbm, plyr"
  # Params:     "n.trees, interaction.depth, shrinkage" 
  # Family:     
}
model.gcvEarth<-function(){
  # Name:       "Multivariate Adaptive Regression Splines"
  # Algorithm:  "gcvEarth"
  # Type:       "Dual Use"
  # Package:    "earth"
  # Params:     "degree" 
  # Family:     
}
model.GFS.FR.MOGAL<-function(){
  # Name:       "Fuzzy Rules via MOGUL"
  # Algorithm:  "GFS.FR.MOGAL"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "max.gen, max.iter, max.tune" 
  # Family:     
}        
model.GFS.LT.RS<-function(){
  # Name:       "Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems"
  # Algorithm:  "GFS.LT.RS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "popu.size, num.labels, max.gen" 
  # Family:     
}       
model.GFS.Thrift<-function(){
  # Name:       "Fuzzy Rules via Thrift"
  # Algorithm:  "GFS.Thrift"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "popu.size, num.labels, max.gen" 
  # Family:     
}       
model.glm<-function(){
  # Name:       "Generalized Linear Model"
  # Algorithm:  "glm"
  # Type:       "Dual Use"
  # Package:    ""
  # Params:     "None" 
  # Family:     "Linear Regression"
}       
model.glmboost<-function(){
  # Name:       "Boosted Generalized Linear Model"
  # Algorithm:  "glmboost"
  # Type:       "Dual Use"
  # Package:    "mboost"
  # Params:     "mstop, prune" 
  # Family:     "Linear Regression"
}
model.glmnet<-function(){
  # Name:       "glmnet"
  # Algorithm:  "glmnet"
  # Type:       "Dual Use"
  # Package:    "glmnet"
  # Params:     "alpha, lambda" 
  # Family:     "Linear Regression"
}
model.glmStepAIC<-function(){
  # Name:       "Generalized Linear Model with Stepwise Feature Selection"
  # Algorithm:  "glmStepAIC"
  # Type:       "Dual Use"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:     "Linear Regression"
}
model.HYFIS<-function(){
  # Name:       "Hybrid Neural Fuzzy Inference System"
  # Algorithm:  "HYFIS"
  # Type:       "Regression"
  # Package:    "frbs"
  # Params:     "num.labels, max.iter" 
  # Family:     
}
model.icr<-function(){
  # Name:       "Independent Component Regression"
  # Algorithm:  "icr"
  # Type:       "Regression"
  # Package:    "fastICA"
  # Params:     "n.comp" 
  # Family:
}
model.kernelpls<-function(){
  # Name:       "Partial Least Squares"
  # Algorithm:  "kernelpls"
  # Type:       "Dual Use"
  # Package:    "pls"
  # Params:     "ncomp" 
  # Family:     "Linear Regression"
}    
model.kknn<-function(){
  # Name:       "k-Nearest Neighbors"
  # Algorithm:  "kknn"
  # Type:       "Dual Use"
  # Package:    "kknn"
  # Params:     "kmax, distance, kernel" 
  # Family:     "Nearest Neighbors"
}
  
model.knn<-function(){
  # Name:        "k-Nearest Neighbors"
  # Algorithm:  "knn"
  # Type:       "Dual Use"
  # Package:    ""
  # Params:     "k" 
  # Family:     "Nearest Neighbors"
}
model.krlsPoly<-function(){
  # Name:       "Polynomial Kernel Regularized Least Squares"
  # Algorithm:  "krlsPoly"
  # Type:       "Regression"
  # Package:    "KRLS"
  # Params:     "lambda, degree" 
  # Family:     ""
}
model.krlsRadial<-function(){
  # Name:       "Radial Basis Function Kernel Regularized Least Squares"
  # Algorithm:  "krlsRadial"
  # Type:       "Regression"
  # Package:    "KRLS kernlab"
  # Params:     "lambda, sigma" 
  # Family:     
}
model.lars<-function(){
  # Name:       "Least Angle Regression"
  # Algorithm:  "lars"
  # Type:       "Regression"
  # Package:    "lars"
  # Params:     "fraction" 
  # Family:     
}
model.lars2<-function(){
  # Name:       "Least Angle Regression"
  # Algorithm:  "lars2"
  # Type:       "Regression"
  # Package:    "lars"
  # Params:     "step" 
  # Family:     
}
model.lasso<-function(){
  # Name:       "The lasso"
  # Algorithm:  "lasso"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "fraction" 
  # Family:     
}
model.leapBackward<-function(){
  # Name:       "Linear Regression with Backwards Selection"
  # Algorithm:  "leapBackward"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax" 
  # Family:     "Linear Regression"
}
model.leapForward<-function(){
  # Name:       "Linear Regression with Forward Selection"
  # Algorithm:  "leapForward"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax"
  # Family:     "Linear Regression"
}
model.leapSeq<-function(){
  # Name:       "Linear Regression with Stepwise Selection"
  # Algorithm:  "leapSeq"
  # Type:       "Regression"
  # Package:    "leaps"
  # Params:     "nvmax"
  # Family:     "Linear Regression"
}
model.lm<-function(){
  # Name:       "Linear Regression"
  # Algorithm:  "lm"
  # Type:       "Regression"
  # Package:    ""
  # Params:     "None" 
  # Family:     "Linear Regression"
}
model.lmStepAIC<-function(){
  # Name:       "Linear Regression with Stepwise Selection"
  # Algorithm:  "lmStepAIC"
  # Type:       "Regression"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:     "Linear Regression"
}
model.logicBag<-function(){
  # Name:       "Bagged Logic Regression"
  # Algorithm:  "logicBag"
  # Type:       "Dual Use"
  # Package:    "logicFS"
  # Params:     "nleaves, ntrees" 
  # Family:     
}
model.logreg<-function(){
  # Name:       "Logic Regression"
  # Algorithm:  "logreg"
  # Type:       "Dual Use"
  # Package:    "LogicReg"
  # Params:     "treesize, ntrees" 
  # Family:     
}
model.M5<-function(){
  # Name:       "Model Tree"
  # Algorithm:  "M5"
  # Type:       "Regression"
  # Package:    "RWeka"
  # Params:     "pruned, smoothed, rules" 
  # Family:     
}
model.M5Rules<-function(){ 
  # Name:       "Model Rules"
  # Algorithm:  "M5Rules"
  # Type:       "Regression"
  # Package:    "RWeka"
  # Params:     "pruned, smoothed" 
  # Family:     
}
model.mlp<-function(){
  # Name:       "Multi-Layer Perceptron"
  # Algorithm:  "mlp"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "size" 
  # Family:     "Neural Networks"
}
model.mlpWeightDecay<-function(){
  # Name:       "Multi-Layer Perceptron"
  # Algorithm:  "mlpWeightDecay"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "size, decay" 
  # Family:     "Neural Networks"
}
model.neuralnet<-function(){
  # Name:       "Neural Network"
  # Algorithm:  "neuralnet"
  # Type:       "Regression"
  # Package:    "neuralnet"
  # Params:     "layer1, layer2, layer3" 
  # Family:     "Neural Networks"
}
model.nnet<-function(){
  # Name:       "Neural Network"
  # Algorithm:  "nnet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size, decay" 
  # Family:     "Neural Networks"
}
model.nodeHarvest<-function(){
  # Name:       "Tree-Based Ensembles"
  # Algorithm:  "nodeHarvest"
  # Type:       "Dual Use"
  # Package:    "nodeHarvest"
  # Params:     "maxinter, mode" 
  # Family:     
}             
model.parRF<-function(){
  # Name:       "Parallel Random Forest"
  # Algorithm:  "parRF"
  # Type:       "Dual Use"
  # Package:    "randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
}
model.partDSA<-function(){
  # Name:       "partDSA"
  # Algorithm:  "partDSA"
  # Type:       "Dual Use"
  # Package:    "partDSA"
  # Params:     "cut.off.growth,MPD" 
  # Family:     
}
model.pcaNNet<-function(){
  # Name:       "Neural Networks with Feature Extraction"
  # Algorithm:  "pcaNNet"
  # Type:       "Dual Use"
  # Package:    "nnet"
  # Params:     "size, decay" 
  # Family:     "Neuwral Networks"
}
model.pcr<-function(){
  # Name:       "Principal Component Analysis"
  # Algorithm:  "pcr"
  # Type:       "Regression"
  # Package:    "pls"
  # Params:     "ncomp"
  # Family:     
}
model.penalized<-function(){
  # Name:       "Penalized Linear Regression"
  # Algorithm:  "penalized"
  # Type:       "Regression"
  # Package:    "penalized"
  # Params:     "lambda1, lambda2" 
  # Family:     "Linear Regression"
}
model.pls<-function(){
  # Name:       "Partial Least Squares"
  # Algorithm:  "pls"
  # Type:       "Dual Use"
  # Package:    "pls"
  # Params:     "ncomp" 
  # Family:     
}
model.plsRglm<-function(){
  # Name:       "Partial Least Squares Generalized Linear Models "
  # Algorithm:  "plsRglm"
  # Type:       "Dual Use"
  # Package:    "plsRglm"
  # Params:     "nt, alpha.pvals.expli" 
  # Family:     
}
model.ppr<-function(){
  # Name:       "Projection Pursuit Regression"
  # Algorithm:  "ppr"
  # Type:       "Regression"
  # Package:    ""
  # Params:     "nterms" 
  # Family:     
}
model.qrf<-function(){
  # Name:       "Quantile Random Forest"
  # Algorithm:  "qrf"
  # Type:       "Regression"
  # Package:    "quantregForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
}
model.qrnn<-function(){
  # Name:       "Quantile Regression Neural Network"
  # Algorithm:  "qrnn"
  # Type:       "Regression"
  # Package:    "qrnn"
  # Params:     "n.hidden, penalty, bag" 
  # Family:     "Neural Network"
}
model.rbfDDA<-function(){
  # Name:       "Radial Basis Function Network"
  # Algorithm:  "rbfDDA"
  # Type:       "Dual Use"
  # Package:    "RSNNS"
  # Params:     "negativeThreshold" 
  # Family:     
}
model.relaxo<-function(){
  # Name:       "Relaxed Lasso"
  # Algorithm:  "relaxo"
  # Type:       "Regression"
  # Package:    "relaxo plyr"
  # Params:     "lambda phi" 
  # Family:     "Linear Regression"
}
model.rf<-function(){
  # Name:       "Random Forest"
  # Algorithm:  "rf"
  # Type:       "Dual Use"
  # Package:    "randomForest"
  # Params:     "mtry" 
  # Family:     "Random Forests"
}
model.ridge<-function(){
  # Name:       "Ridge Regression"
  # Algorithm:  "ridge"
  # Type:       "Regression"
  # Package:    "elasticnet"
  # Params:     "lambda" 
  # Family:     "Linear Regression"
}
model.rknn<-function(){
  # Name:       "Random k-Nearest Neighbors"
  # Algorithm:  "rknn"
  # Type:       "Dual Use"
  # Package:    "rknn"
  # Params:     "k, mtry" 
  # Family:     "Nearest Neighbors"
}
model.rknnBel<-function(){
  # Name:       "Random k-Nearest Neighbors with Feature Selection"
  # Algorithm:  "rknnBel"
  # Type:       "Dual Use"
  # Package:    "rknn, plyr"
  # Params:     "k, mtry, d" 
  # Family:     "Nearest Neighbors"
}
model.rlm<-function(){
  # Name:       "Robust Linear Model"
  # Algorithm:  "rlm"
  # Type:       "Regression"
  # Package:    "MASS"
  # Params:     "None" 
  # Family:     
}
model.rpart<-function(){
  # Name:       "CART"
  # Algorithm:  "rpart"
  # Type:       "Dual Use"
  # Package:    "rpart"
  # Params:     "cp" 
  # Family:     
}
model.rpart2<-function(){
  # Name:       "CART"
  # Algorithm:  "rpart2"
  # Type:       "Dual Use"
  # Package:    "rpart"
  # Params:     "maxdepth" 
  # Family:     
}
model.RRF<-function(){
  # Name:       "Regularized Random Forest"
  # Algorithm:  "RRF"
  # Type:       "Dual Use"
  # Package:    "randomForest, RRF"
  # Params:     "mtry, coefReg, coefImp" 
  # Family:     "Random Forests"
}
model.RRFglobal<-function(){ 
  # Name:       "Regularized Random Forest"
  # Algorithm:  "RRFglobal"
  # Type:       "Dual Use"
  # Package:    "RRF"
  # Params:     "mtry, coefReg"
  # Family:     "Random Forests"
}
model.rvmLinear<-function(){
  # Name:       "Relevance Vector Machines with Linear Kernel"
  # Algorithm:  "rvmLinear"
  # Type:       "Regression"
  # Package:    "kernlab"
  # Params:     "None" 
  # Family:     
}
model.rvmPoly<-function(){
  # Name:       "Relevance Vector Machines with Polynomial Kernel"
  # Algorithm:  "rvmPoly"
  # Type:       "Regression"
  # Package:    "kernlab"
  # Params:     "scale, degree" 
  # Family:     
}
model.rvmRadial<-function(){
  # Name:        "Relevance Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "rvmRadial"
  # Type:        "Regression"
  # Package:     "kernlab"
  # Params:      "sigma" 
  # Family:     
}
model.SBC<-function(){
  # Name:        "Subtractive Clustering and Fuzzy c-Means Rules"
  # Algorithm:   "SBC"
  # Type:        "Regression"
  # Package:     "frbs"
  # Params:      "r.a, eps.high, eps.low" 
  # Family:     
}
model.simpls<-function(){
  # Name:        "Partial Least Squares"
  # Algorithm:   "simpls"
  # Type:        "Dual Use"
  # Package:     "pls"
  # Params:      "ncomp" 
  # Family:     
}
model.spls<-function(){
  # Name:        "Sparse Partial Least Squares"
  # Algorithm:   "spls"
  # Type:        "Dual Use"
  # Package:     "spls"
  # Params:      "K, eta, kappa" 
  # Family:     
}
model.superpc<-function(){
  # Name:        "Supervised Principal Component Analysis"
  # Algorithm:   "superpc"
  # Type:        "Regression"
  # Package:     "superpc"
  # Params:      "threshold n.components" 
  # Family:      "PCA"
}
model.svmBoundrangeString<-function(){
  # Name:        "Support Vector Machines with Boundrange String Kernel"
  # Algorithm:   "svmBoundrangeString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "length, C" 
  # Family:      "SVM"
}
model.svmExpoString<-function(){
  # Name:        "Support Vector Machines with Exponential String Kernel"
  # Algorithm:   "svmExpoString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "lambda, C" 
  # Family:      "SVM"
}
model.svmLinear<-function(){
  # Name:        "Support Vector Machines with Linear Kernel"
  # Algorithm:   "svmLinear"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "C" 
  # Family:      "SVM"    
}
model.svmPoly<-function(){
  # Name:        "Support Vector Machines with Polynomial Kernel"
  # Algorithm:   "svmPoly"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "degree, scale, C"
  # Family:      "SVM"
}
model.svmRadial<-function(){
  # Name:        "Support Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "svmRadial"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "sigma, C" 
  # Family:      "SVM"
}
model.svmRadialCost<-function(){
  # Name:        "Support Vector Machines with Radial Basis Function Kernel"
  # Algorithm:   "svmRadialCost"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "C" 
  # Family:      "SVM"
}
model.svmSpectrumString<-function(){
  # Name:        "Support Vector Machines with Spectrum String Kernel"
  # Algorithm:   "svmSpectrumString"
  # Type:        "Dual Use"
  # Package:     "kernlab"
  # Params:      "length, C" 
  # Family:      "SVM"
}
model.treebag<-function(){
  # Name:        "Bagged CART"
  # Algorithm:   "treebag"
  # Type:        "Dual Use"
  # Package:     "ipred plyr"
  # Params:      "None" 
  # Family:     
}
model.widekernelpls<-function(){
  # Name:        "Partial Least Squares"
  # Algorithm:   "widekernelpls"
  # Type:        "Dual Use"
  # Package:     "pls"
  # Params:      "ncomp" 
  # Family:     
}
model.WM<-function(){
  # Name:        "Wang and Mendel Fuzzy Rules"
  # Algorithm:   "WM"
  # Type:        "Regression"
  # Package:     "frbs"
  # Params:      "num.labels, type.mf" 
  # Family:     
}
model.xyf<-function(){
  # Name:        "Self-Organizing Maps"
  # Algorithm:   "xyf"
  # Type:        "Dual Use"
  # Package:     "kohonen"
  # Params:      "xdim, ydim, xweight, topo"
  # Family:     
}






model.gbm <- function(df){
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv"
    number = 10
    ## repeated ten times
    repeats = 10)
  
  model <- train(df$Predict ~ . data = df
                   method = "gbm"
                   trControl = fitControl
                   verbose = FALSE)
  #prediction function as to be elsewhere
  predict(model newdata=)
  
}

combineModels(modelList modelResults){
  
  #Calculate mean and median of both models
}

#with feature selection