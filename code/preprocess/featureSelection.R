featureSelection <- function(model, option="step"){
  
  if(option == "step"){
    #equal to stepAIC in MASS
    return (step(model))
  }
  
  
}