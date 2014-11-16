rankhospital <- function(state, outcome, num = "best")
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
  
  hospitalsByScore <- hospitalsInAState[hospitalsInAState[,columnNumber] != "Not Available",]
  sortedByNameAndByScore <- hospitalsByScore[order(as.numeric(hospitalsByScore[, columnNumber], na.rm=TRUE),
                                                   hospitalsByScore[,2]),]
  
  ## After the sort, the row numbers are the ranking
  if (num == "best")
  {
    print(sortedByNameAndByScore[1,2])
  }
  else if (num == "worst")
  {
    rowNumber<- nrow(sortedByNameAndByScore)
    print(sortedByNameAndByScore[rowNumber,2])
  }
  else
  {
    
    print(sortedByNameAndByScore[as.numeric(num),2])
  }
}

## Provides a ranking for each row in the hospital data frame
## so that best, worst, and numeric ranking can be determined.
rankScores<-function(sortedByNameAndScore,colNumber)
{
  rowAndRank <- vector("numeric", length=nrow(sortedByNameAndScore))
  ranking <- 1
  rowAndRank[1] <- 1
  for(i in 2:nrow(sortedByNameAndScore)) 
  {
    if (sortedByNameAndScore[i,colNumber] != sortedByNameAndScore[i-1,colNumber]) 
    { 
      ranking <- ranking + 1 
    }
    rowAndRank[i] <- ranking
  }
  return(rowAndRank)
}