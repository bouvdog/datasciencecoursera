## Reads a directory full of files and reports the number of completely 
## observed cases in each data file.
##
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332)
{
  completeData <- data.frame(id= numeric(0), nobs= integer(0))
  ids <- vector("numeric")
  nobs <- vector("numeric")
  completeRows<-1
  for (i in id)
  {
    fileName <- sprintf("%03d", i)
    name <- paste(directory, "/", fileName, ".csv", sep="")
    oneMonitorsData <- read.csv(name)
    value <- getNumberOfCompleteRows(oneMonitorsData)
    if (value > 0)
    {
      ids[completeRows] = i
      nobs[completeRows] = value
      completeRows = completeRows + 1
    }
  }
  output<-data.frame(ids, nobs, stringsAsFactors=FALSE)
  print(output)
}

# returns the number of complete observations for any given 
# monitor's data set
getNumberOfCompleteRows<-function(oneMonitorsData)
{
  numberOfRows <- nrow(oneMonitorsData)
  numberOfCompleteRows <- 0
  for (j in 1:numberOfRows)
  {
    row <- oneMonitorsData[j,]
    if(isRowComplete(row))
    {
      numberOfCompleteRows <- numberOfCompleteRows + 1
    }
  }
  return(numberOfCompleteRows)
}

## returns TRUE if values are not NA for both sulfite and nitrate
isRowComplete<-function(row)
{
  complete<-TRUE
  if (is.na(row$sulfate))
  {
    complete <- FALSE
  }
  if (is.na(row$nitrate))
  {
    complete <- FALSE
  }
  return(complete)
}