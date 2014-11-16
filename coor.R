## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
  id = 1
  sulfate <- vector("numeric")
  nitrate <- vector("numeric")
  for (i in id)
  {
    fileName <- sprintf("%03d", i)
    name <- paste(directory, "/", fileName, ".csv", sep="")
    oneMonitorsData <- read.csv(name)
    numberOfRows <- nrow(oneMonitorsData)
    for (j in 1:numberOfRows)
    {
      row <- oneMonitorsData[j,]
      if(isRowComplete(row))
      {
        sulfateValue <- row[sulfate]
        nitrateValue <- row[nitrate]
        if (sulfateValue > threshold && nitrateValue > threshold)
        {
          if (length(sulfate) > 0)
          {
            sulfate <- sulfateValue
            nitrate <- nitrateValue
          }
          else
          {
            append(sulfate, sulfateValue)
            append(nitrate, nitrateValue)
          }
        }
      }
    }
  }
  
  cor(sulfate,nitrate,,"kendall")
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