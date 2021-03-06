plot2<-function()
{
  setwd("e:/datascience")
  require(dplyr)
  require(lubridate)
  columns<-c("character", "character", "numeric", "numeric", "numeric"
             , "numeric", "numeric", "numeric", "numeric")
  hpc<-read.table("household_power_consumption.txt", sep=";", header=TRUE, 
                  colClasses=columns, na.strings="?")
  hpc<-mutate(hpc,Date=dmy_hms(paste(Date, Time)))
  
  interval<-hpc[hpc$Date > ymd("2007-02-01") & hpc$Date < ymd("2007-02-03"),]
  
  png("plot2.png", width = 480, height = 480)
  plot(interval$Date, interval$Global_active_power, type="l",
       ylab="Global Active Power (kilowatts)")
  dev.off()
}