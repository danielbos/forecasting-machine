library("caret")

adjustVariables <- function(df, dateField, predictField){
  # Sets the date field and the predictor field.
  #
  # Args:
  #   df: Dataframe with the data.
  #   dateField: Date field that is going to be converted as date.
  #   predictorField: Set the predictor field with the correct name.
  #
  # Returns:
  #   Dataframe with the new updated fields.  
 
  df["y"] <- as.numeric(predictField)
  df["TSDate"] <- as.Date(dateField)
    
  return (df)
}

deleteCorrelated <- function(df, cutoff = .90){
  # Deletes tha variables that are correlated over a certain threshold.
  #
  # Args:
  #   df: Dataframe with the data.
  #   cutoff: cutofff value of the correlation.
  #
  # Returns:
  #   Dataframe without the columns with a correlation value higher than the cutoff. 
  # TO DO - print removed columns by names dimnames(indexesToRemove)
  indexesToRemove <- findCorrelation(cor(df), cutoff = cutoff, verbose = FALSE)
  return (df[, -indexesToRemove])   
}

deleteLinearCombos <- function(df){
  comboInfo <- findLinearCombos(df)
  return (df[, -comboInfo$remove])  
} 


deleteNearZeroVar <- function(df){
  # Deletes nearzero variance variables 
  # (this doesn't work very good for variables that are not so common)
  # 
  # Args:
  #   df: Dataframe with the data.
  #
  # Returns:
  #   Dataframe without the columns with near zero variance. 
  
  indexesToRemove <- nearZeroVar(df)
  return(df[, -indexesToRemove])
}

featureSelection <- function(df){
  
  df <- deleteCorrelated(df)
  df <- deleteLinearCombos(df)
  df <- deleteNearZeroVar(df)
  return(df)
}