rankall <- function(outcome, num = "best")
{
  morbidityColumn <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  columnNumber <- morbidityColumn[[outcome]]
  if (is.null(columnNumber))
  {
    stop("invalid outcome")
  }
  outcomesOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  states<-unique(outcomesOfCare[,7])
  states<-sort(states)
  result<-data.frame(hospital = character(),
                     state = character())
  
  
  for (i in states)
  {
    #print(sprintf("%s %s %s", i, rankhospital(i,outcome,num,outcomesOfCare),i))
    result<-rbind(result,
                  data.frame(hospital=rankhospital(i, outcome, num, outcomesOfCare), 
                             state=i,
                             stringsAsFactors=FALSE))
  }
  
  print(result)
}

# Returns a vector of the name of the hospital that has
# the ranking in the state with the Abbrv of the state
# on either side
rankhospital <- function(state, outcome, num = "best", outcomesOfCare)
{
  morbidityColumn <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  columnNumber <- morbidityColumn[[outcome]]
  if (is.null(columnNumber))
  {
    stop("invalid outcome")
  }
  
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
    sortedByNameAndByScore[1,2]
  }
  else if (num == "worst")
  {
    rowNumber<- nrow(sortedByNameAndByScore)
    sortedByNameAndByScore[rowNumber,2]
  }
  else
  {
    sortedByNameAndByScore[as.numeric(num),2]
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