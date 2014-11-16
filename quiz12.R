quiz12<-function()
{
  
  oneMonitorsData <- read.csv("E:/datascience/hw1_data.csv")
  ozone<-oneMonitorsData[,1]
  ##(hsb6 <- hsb2.small[hsb2.small$ses == 1, ])
  ##(hsb8 <- hsb2.small[with(hsb2.small, ses == 3 & female == 0), ]),
  solar<-subset(oneMonitorsData, Month==5)
  solarr<-solar[,1]
  max(solarr, na.rm=TRUE)
}