## Assignment
## Calculates the mean of a pollutant (sulfate or nitrate) across a 
## specified list of monitors. 
##
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332)
{
  ## creating an empty vector to add values to it, one at a time
  values <- vector("numeric", length=0)
  for (i in id)
  {
    fileName <- sprintf("%03d", i)
    valuesFromOneFile <- readOne(
      paste(directory , "/", fileName,".csv", sep=""), pollutant)
    values <- append(values, valuesFromOneFile)
  }
  mean(values, na.rm=TRUE)
}
  
## Given a file name returns the values for the specified pollutant
## Assumes that it will be called by pollutantmean.
readOne<-function(name, pollutant)
{
  oneMonitorsData <- read.csv(name)
  pollutantData <- oneMonitorsData[[pollutant]]
  pollutantData <- pollutantData[!is.na(pollutantData)]
}