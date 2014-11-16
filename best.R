best<-function(state,outcome)
{
  morbidityColumn <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  columnNumber <- morbidityColumn[[outcome]]
  if (is.null(columnNumber))
  {
    stop("invalid outcome")
  }
  outcomesOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospitalsInAState <- outcomesOfCare[outcomesOfCare$State == state,]
  if (nrow(hospitalsInAState) == 0)
  {
    stop("invalid state")
  }
    
  lowest <- min(as.numeric(hospitalsInAState[,columnNumber]), na.rm=TRUE)
  hospitalsWithLowest <- hospitalsInAState[hospitalsInAState[,columnNumber] == lowest,]
        
  hospitalsWithLowest <- hospitalsWithLowest[order(hospitalsWithLowest[,2]),] 
  
  print(hospitalsWithLowest[,2])
  
}