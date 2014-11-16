## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
##
## Assumes that the file complete.R has already been sourced into
## the workspace
corr <- function(directory, threshold = 0) 
{
  correlates <- vector("numeric")
  for (i in 1:332)
  {
    fileName <- sprintf("%03d", i)
    name <- paste(directory, "/", fileName, ".csv", sep="")
    oneMonitorsData <- read.csv(name)
    value <- getNumberOfCompleteRows(oneMonitorsData)
    if (value > threshold)
    {
      sulfate <- vector("numeric")
      nitrate <- vector("numeric")
      for (j in 1:nrow(oneMonitorsData))
      {
        row <- oneMonitorsData[j,]
        if(isRowComplete(row))
        {
          sulfateValue <- row[[2]]
          nitrateValue <- row[[3]]
         
          sulfate <- c(sulfate, sulfateValue)
          nitrate <- c(nitrate, nitrateValue)
        }
      }
      correlates <- c(correlates, cor(sulfate,nitrate))
    }
  }
  return (correlates)
  
}
