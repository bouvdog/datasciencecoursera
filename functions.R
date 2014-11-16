above10 <- function(x)
{
  use <- x > 10
  x[use]
}

above <- function(x, n = 10)
{
  use <- x > n
  x[use]
}

columnmean <- function(x, removeNA = TRUE)
{
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc)
  {
    means[i] <- mean[x[,i], na.rm = removeNA]
  }
  means
}

f<-function(x,y)
{
  x^2 + y/z
  
}

test<-function()
{
  for (i in 23)
  {
    print(sprintf("%03d", i))
  }
}

