plot3<-function()
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
  
  png("plot3.png", width = 480, height = 480)
  plot(interval$Date, interval$Sub_metering_1, type="n", xlab="", 
       ylab="Energy sub metering")
  lines(interval$Date, interval$Sub_metering_1, col="black")
  lines(interval$Date, interval$Sub_metering_2, col="red")
  lines(interval$Date, interval$Sub_metering_3, col="blue")
  legend_text<-c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  legend("topright", legend_text, xjust=1, lty=1, col=c("black", "red", "blue"))
  dev.off()
}