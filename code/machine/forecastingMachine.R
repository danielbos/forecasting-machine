library(R6)
source("models/regression_trees.R")
source("models/fmregressor.R")
source("machine/fmmodel.R")
source("utils/global_functions.R")

FM <- R6Class("FM",
                  public = list(
                    runType = "all",
                    name = NA,
                    onlyFeatureSelectionModels = NA, 
                    doAutoFeatureSelection = TRUE, 
                    featureSelectionFromModel = NA, 
                    runSlowTrainModels = FALSE,                                      
                    scaleToAWS = FALSE,
                    multipleModelsCombinations = 1,
                    multipleModelsCombinationsCriteria = "median",
                    modelSelectionCriteria = "train",
                    forecastSelectionCriteria = "accuracy",
                    trainSelectionCriteria = "aic",
                    trainStartPeriod = NA,
                    forecastStartPeriod = NA,
                    forecastHorizon = 14,
                    configFile = NA,
                    trainingData = NA,
                    testData = NA,                    
                    initialize = function(trainingData,
                                          testData,
                                          runType,
                                          name,
                                          onlyFeatureSelectionModels, 
                                          doAutoFeatureSelection, 
                                          featureSelectionFromModel, 
                                          runSlowTrainModels,                                      
                                          scaleToAWS,
                                          multipleModelsCombinations,
                                          multipleModelsCombinationsCriteria,
                                          modelSelectionCriteria,
                                          forecastSelectionCriteria,
                                          trainSelectionCriteria,
                                          trainStartPeriod,
                                          forecastStartPeriod,
                                          forecastHorizon,
                                          configFile) {
                      if (!missing(runType))                            self$setRunType(runType)
                      if (!missing(name))                               self$setName(name)
                      if (!missing(onlyFeatureSelectionModels))         self$setOnlyFeatureSelectionModels(onlyFeatureSelectionModels)
                      if (!missing(doAutoFeatureSelection))             self$setDoAutoFeatureSelection(doAutoFeatureSelection) 
                      if (!missing(featureSelectionFromModel))          self$setFeatureSelectionFromModel(featureSelectionFromModel)
                      if (!missing(runSlowTrainModels))                 self$setRunSlowTrainModels(runSlowTrainModels)                                    
                      if (!missing(scaleToAWS))                         self$setScaleToAWS(scaleToAWS)
                      if (!missing(multipleModelsCombinations))         self$setMultipleModelsCombinations(multipleModelsCombinations)
                      if (!missing(multipleModelsCombinationsCriteria))  self$setMultipleModelsCombinationsCriteria(multipleModelsCombinationsCriteria)
                      if (!missing(modelSelectionCriteria))             self$setModelSelectionCriteria(modelSelectionCriteria)
                      if (!missing(forecastSelectionCriteria))          self$setForecastSelectionCriteria(forecastSelectionCriteria)
                      if (!missing(trainSelectionCriteria))             self$setTrainSelectionCriteria(trainSelectionCriteria)
                      if (!missing(trainStartPeriod))                   self$setTrainStartPeriod(trainStartPeriod)
                      if (!missing(forecastStartPeriod))                self$setForecastStartPeriod(forecastStartPeriod)
                      if (!missing(forecastHorizon))                    self$setForecastHorizon(forecastHorizon)
                      if (!missing(configFile))                         self$setConfigFile(configFile)
                      if (!missing(trainingData))                       self$setTrainingData(trainingData)
                      if (!missing(testData))                           self$setTestData(testData)
                      private$setAllModelsNames()
                    },
                    
                    run = function(trainingData = self$trainingData,
                                   testData = testData,
                                   runType = self$runType,
                                   name = self$name,
                                   onlyFeatureSelectionModels = self$onlyFeatureSelectionModels, 
                                   doAutoFeatureSelection = self$doAutoFeatureSelection, 
                                   featureSelectionFromModel = self$featureSelectionFromModel, 
                                   runSlowTrainModels = self$runSlowTrainModels,                                      
                                   scaleToAWS = self$scaleToAWS,
                                   multipleModelsCombinations = self$multipleModelsCombinations,
                                   multipleModelsCombinationsCriteria = self$multipleModelsCombinationsCriteria,
                                   modelSelectionCriteria = self$modelSelectionCriteria,
                                   forecastSelectionCriteria = self$forecastSelectionCriteria,
                                   trainSelectionCriteria = self$trainSelectionCriteria,
                                   trainStartPeriod = self$trainStartPeriod,
                                   forecastStartPeriod = self$forecastStartPeriod,
                                   forecastHorizon = self$forecastHorizon,
                                   configFile = self$configFile) {
                      
                      self$setTrainingData(trainingData)
                      self$setTestData(testData)
                      self$setRunType(runType)
                      self$setName(name)
                      self$setOnlyFeatureSelectionModels(onlyFeatureSelectionModels)
                      self$setDoAutoFeatureSelection(doAutoFeatureSelection)
                      self$setFeatureSelectionFromModel(featureSelectionFromModel)
                      self$setRunSlowTrainModels(runSlowTrainModels)                                      
                      self$setScaleToAWS(scaleToAWS)
                      self$setMultipleModelsCombinations(multipleModelsCombinations)
                      self$setMultipleModelsCombinationsCriteria(multipleModelsCombinationsCriteria)
                      self$setModelSelectionCriteria(modelSelectionCriteria)
                      self$setForecastSelectionCriteria(forecastSelectionCriteria)
                      self$setTrainSelectionCriteria(trainSelectionCriteria)
                      self$setTrainStartPeriod(trainStartPeriod)
                      self$setForecastStartPeriod(forecastStartPeriod)
                      self$setForecastHorizon(forecastHorizon)
                      self$setConfigFile(configFile)
                      #self$setTrainingData()
                      #self$setTestData()
                      browser()
                      
                      self$setModelsToRun()
                      self$predictAllFMModels()
                      self$compareModels()
                      
                      
                    },
                    
                    #GETS
                    getModelTypes = function(){
                      return (private$modelTypes)
                    },                    
                    getRunTypes = function (){
                      return (private$runTypes)
                    },                    
                    getModelSelectionCriterias = function(){
                      return (private$modelSelectionCriterias)
                    },                    
                    getMultipleModelsCombinationsCriterias = function(){
                      return (private$multipleModelsCombinationsCriterias)
                    },                    
                    getForecastSelectionCriterias = function (){
                      return (private$forecastSelectionCriterias)
                    },
                    getTrainSelectionCriterias = function(){
                      return (private$trainSelectionCriterias)
                    },
                    getModelsToRun = function(){
                      return (private$modelsToRun)
                    },
                    
                    setModelsToRun = function() {
                      #if(!(is.na(type)) & !( type %in% listModelsType())){
                       # stop("Incorrect Model Type %s. Please check listModelTypes for the possible values", type)
                      #}
                      
                      filteredModels <- self$getAllModels()
                      filteredModels <- if(!is.na(self$onlyFeatureSelectionModels)) self$getModelsByFeatureSelection(filteredModels) else filteredModels
                      filteredModels <- if(!is.na(self$runSlowTrainModels)) self$getModelsBySlowTrain(filteredModels) else filteredModels
                      private$modelsToRun <- filteredModels
                    },
                    
                    #SETS
                    setTrainingData = function(trainingData){ 
                      self$trainingData <- trainingData
                    },
                    setTestData = function(testData){ 
                      self$testData <- testData
                    },
                    setRunType = function(runType){                     
                      if(!(runType %in% self$getRunTypes())){
                        stop("Wrong value for RunType parameter! Check listRunTypes() method!")
                      }
                      self$runType <- runType
                    },
                    
                    setName = function(name){
                      if(!is.na(name)){
                        if(self$runType == "type"){
                         models <- self$getModelsTypes()
                        }
                        if(self$runType =="single"){
                          models <- self$getAllModelsNames() 
                        }
                          if(!(name %in% models)){
                            stop("No model found with name %s", name)
                          }
                        }
                        self$name <- name
                    },
                    
                    setOnlyFeatureSelectionModels = function(onlyFeatureSelectionModels){
                      self$onlyFeatureSelectionModels <- onlyFeatureSelectionModels
                    },
                    setDoAutoFeatureSelection = function(doAutoFeatureSelection){
                      self$doAutoFeatureSelection <- doAutoFeatureSelection
                    },
                    setFeatureSelectionFromModel = function(featureSelectionFromModel){
                      if(featureSelectionFromModel %in% self$getAllModelsNames())
                        self$featureSelectionFromModel <- featureSelectionFromModel 
                    },
                    setRunSlowTrainModels = function(runSlowTrainModels){
                      self$runSlowTrainModels <- runSlowTrainModels
                    },                                      
                    
                    setScaleToAWS = function(scaleToAWS){
                      self$scaleToAWS <- scaleToAWS
                    },
                    
                    setMultipleModelsCombinations = function(multipleModelsCombinations){
                      if(multipleModelsCombinations > private$maxModelsCombinations){
                        stop("Maximum number of models combinations is %d!", private$maxModelsCombinations)
                      } 
                      self$multipleModelsCombinations <- multipleModelsCombinations
                    },
                    
                    setMultipleModelsCombinationsCriteria = function(multipleModelsCombinationsCriteria){
                      if(!(multipleModelsCombinationsCriteria %in% self$getMultipleModelsCombinationsCriterias())){
                        stop("%s is not available for the model combinations criteria! Check listMultipleModelsCombinationsCriteria for the possible options.", multipleModelsCombinationsCriteria)
                      }
                      self$multipleModelsCombinationsCriteria <- multipleModelsCombinationsCriteria
                    },
                  
                    setModelSelectionCriteria = function(modelSelectionCriteria){
                      if(!(modelSelectionCriteria %in% self$getModelSelectionCriterias())){
                        stop("%s is not available for the model selection criteria! Check getModelSelectionCriterias for the possible options.", modelSelectionCriteria)
                      }
                      self$modelSelectionCriteria <- modelSelectionCriteria
                    },
                    setForecastSelectionCriteria = function(forecastSelectionCriteria){
                      if(!(forecastSelectionCriteria %in% self$getForecastSelectionCriterias())){
                        stop("%s is not available for the forecast selection criteria! Check getForecastSelectionCriterias for the possible options.", forecastSelectionCriteria)
                      }
                      self$forecastSelectionCriteria <- forecastSelectionCriteria
                    },
                    setTrainSelectionCriteria = function(trainSelectionCriteria){
                      if(!(trainSelectionCriteria %in% self$getTrainSelectionCriterias())){
                        stop("%s is not available for the train selection criteria! Check getTrainSelectionCriterias for the possible options.", trainSelectionCriteria)
                      }
                      self$trainSelectionCriteria <- trainSelectionCriteria
                      
                    },
                    setTrainStartPeriod = function(trainStartPeriod){
                          self$trainStartPeriod <- trainStartPeriod
                    },
                    setForecastStartPeriod = function(forecastStartPeriod){
                      self$forecastStartPeriod <- forecastStartPeriod
                    },
                    setForecastHorizon = function(forecastHorizon){
                      self$forecastHorizon <- forecastHorizon
                    },
                    
                    setConfigFile = function(configFile){
                      self$configFile <- configFile
                      private$readConfigFile()
                    },
                    #AUXILIARY FUNCTIONS
                    getAllModels = function(){
                      #fmRegressionTrees <- FMRegressionTrees()
                      fmTimeSeries <- FMTimeSeries()
                      return (getFMModels(fmTimeSeries))
                      
                      #return (c(getModelsFMLinearRegressor(),
#                                 getModelsFMRandomForests(),
#                                 getModelsFMAdditive(),
#                                 getModelsFMBag(),
#                                 getModelsFMClustering(),
#                                 getModelsFMFuzzySystems(),
#                                 getModelsFMGaussian(),
#                                 getModelsFMLogicRegressor(),
#                                 getModelsFMMars(),
#                                 getModelsFMNeuralNetworks(),
#                                 getModelsPartDSA(),
#                                 getModelsFMRegressionTrees(),
#                                 getModelsFMRvm(),
#                                 getModelsFMSOMap(),
#                                 getModelsFMSVM(),
#                                 getModelsFMTimeSeries()))  
                    },
                    getAllModelsNames = function(){
                      return (private$allModelsNames)
                    },
                    getModelsByType = function(models){
                      filteredTypeModels <- list()
                      for(model in models){
                        if(model$type == self$name){
                          filteredTypeModels <- c(filteredTypeModels, list(model))
                        }
                      }
                     return(filteredTypeModels)
                    },

                    getModelsByFeatureSelection = function(models){                    
                      filteredFeatureSelectionModels <- list()
                      for(model in models){
                        if(model$featureSelection == self$onlyFeatureSelectionModels){
                          filteredFeatureSelectionModels <- c( filteredFeatureSelectionModels, list(model))
                        }
                      }
                      return( filteredFeatureSelectionModels)
                    },

                    getModelsBySlowTrain = function(models){
                      filteredSlowTrainModels <- list()
                      for(model in models){
                        if(model$slowTrain == self$runSlowTrainModels){
                          filteredSlowTrainModels <- c(filteredSlowTrainModels, list(model))
                        }
                      }
                      return(filteredSlowTrainModels)
                    },

                    getObject = function(model){
                      switch(model$type, 
                             FMLinearRegressor={ return (FMLinearRegressor()) },
                             FMRandomForests={return (FMRandomForests())},
                             FMAdditive={return (FMAdditive())},
                             FMBag={return (FMBag())},
                             FMClustering={return (FMClustering())},
                             FMFuzzySystems={return (FMFuzzySystems())},
                             FMGaussian={return (FMGaussian())},
                             FMLogicRegressor={return (FMLogicRegressor())},
                             FMMars={return (FMMars())},
                             FMNeuralNetworks={return (FMNeuralNetworks())},
                             FMPartDSA={return (FMPartDSA())},
                             FMRegressionTrees={return (FMRegressionTrees())},
                             FMRvm={return (FMRvm())},
                             FMSOMap={return (FMSOMap())},
                             FMSVM={return (FMSVM())},
                             FMTimeSeries={return (FMTimeSeries())},
                    {
                      stop("Model Type not found %s", model$type)
                    }
                      )  
                    },

#                   fitAllFMModels = function(){
#                     allNames <- list()
#                     
#                     for(model in private$modelsToRun){
#                     
#                       tryCatch({
#                          obj <- self$getObject(model)
#                          fit <- fitFMModel(obj, model, self$trainingData)
#                          private$fittedModels <- c(private$fittedModels,list(fit))
#                          allNames <- c(allNames, model$name)
#                       }, error = function(e) {
#                         #TODO: log error or insert in database 
#                         private$modelsWithErrors <- c(private$modelsWithErrors, model$name)
#                         print(e$message)
#                       }) 
#                     }
#                     
#                     names(private$fittedModels) <- allNames
#                     names(private$modelsWithErrors) <-  private$modelsWithErrors
#                   },

                predictAllFMModels = function(){
                      allNames <- list()
                      browser()
                      for(model in private$modelsToRun){
                        tryCatch({
                            obj <- self$getObject(model)
                            pred <- predictFMModel(obj, model, self$trainingData, self$testData, self$forecastHorizon)
                            private$modelsPredictions <- c(private$modelsPredictions,list(pred))
                            allNames <- c(allNames, model$name)
                        }, error = function(e) {
                             #TODO: log error or insert in database 
                             private$modelsWithErrors <- c(private$modelsWithErrors, model$name)
                             print(e$message)                                                 
                        }) 
                      }
                      names(private$modelsPredictions) <- allNames
                      #names(private$modelsWithErrors) <-  private$modelsWithErrors
                }
        ),
        private = list(
          defaultFeatureSelectionModel = "avNNet",
          runTypes = list("all", "type", "single"),
          modelSelectionCriterias = list("forecast", "train"),
          multipleModelsCombinationsCriterias = list("median", "mean"),
          forecastSelectionCriterias = list("mae", "mape", "rmse", "accuracy"),
          trainSelectionCriterias = list("aic", "r2adj"),
          modelTypes = list("FMLinearRegressor", "FMRandomForests", "FMAdditive", "FMBag", "FMClustering",
                            "FMFuzzySystems", "FMGaussian", "FMLogicRegressor", "FMMars", "FMNeuralNetworks",
                            "FMPartDSA", "FMRegressionTrees", "FMRvm", "FMSOMap", "FMSVM", "FMTimeSeries"),
          allModelsNames = NA,
          maxModelsCombinations = 3,
          type = NA,
          featureSelection = NA,
          slowTrain = NA,
          ec2AMI = NA,
          ec2Type = NA,
          ec2NrMachines = 0,
          ec2UseExisting = c("ip1","ip2"),
          resultsFile = NA,
          dbConnectionString = NA,
           #fittedModels = list(),
           modelsToRun = list(),
          modelsWithErrors = list(),
           modelsPredictions = list(),
          readConfigFile = function(){},
          
          setAllModelsNames = function(){
            allModels <- self$getAllModels()           
            for(model in allModels){
              private$allModelsNames <- c(private$allModelsNames, list(model$name))            
            }
          }
        )
)



