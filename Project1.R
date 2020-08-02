Data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

t$Date <- as.Date(t$Date, "%d/%m/%Y")

Data <- subset(Data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
Data <- Data[complete.cases(Data),]
dateTime <- paste(Data$Date, Data$Time)
dateTime <- setNames(dateTime, "DateTime")
 
Data <- Data[ ,!(names(Data) %in% c("Date","Time"))]
Data <- cbind(dateTime, Data)
Data$dateTime <- as.POSIXct(dateTime)

#Histogram
hist(Data$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

#Plot2
plot(Data$Global_active_power~Data$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#Plot3
with(Data, {plot(Sub_metering_1~dateTime, type="l",ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')})
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1),c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
#Plot4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  with(Dara, {
    plot(Global_active_power~dateTime, type="l", 
         ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~dateTime, type="l", 
         ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~dateTime, type="l", 
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Global_reactive_power~dateTime, type="l", 
         ylab="Global Rective Power (kilowatts)",xlab="")})
