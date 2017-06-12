#Exploratory Data Analysis Course 1 : PLOT4

rm(list=ls(all=TRUE))

library(lubridate)
library(parallel)
library(dplyr)

# Create a new column "exact_time" with time/date in POSIXlt

data_hpc <- read.table("household_power_consumption.txt",sep=";",header=TRUE)
data_hpc <- mutate(data_hpc,exact_time=paste(as.character(Date),as.character(Time)))
data_hpc$exact_time <- strptime(data_hpc$exact_time, "%d/%m/%Y %H:%M:%S",tz = "US")

# Subset the time serie from the dates 2007-02-01 and 2007-02-02
sub_data_hpc <- subset(data_hpc, (data_hpc$exact_time>=strptime("2007/02/01 00:00:00","%Y/%m/%d %H:%M:%S",tz = "US")) & (data_hpc$exact_time<=strptime("2007/02/03 00:00:00","%Y/%m/%d %H:%M:%S",tz = "US")))

# Tranform the ? in NA 
for (i in 3:9) {sub_data_hpc[,i][sub_data_hpc[,i] %in% "?"]=NA}

# Convert the variables in numerical values 
sub_data_hpc$Global_active_power <- as.numeric(as.character(sub_data_hpc$Global_active_power))
sub_data_hpc$Global_reactive_power <- as.numeric(as.character(sub_data_hpc$Global_reactive_power))
sub_data_hpc$Voltage <- as.numeric(as.character(sub_data_hpc$Voltage))
sub_data_hpc$Sub_metering_1 <- as.numeric(as.character(sub_data_hpc$Sub_metering_1))
sub_data_hpc$Sub_metering_2 <- as.numeric(as.character(sub_data_hpc$Sub_metering_2))
sub_data_hpc$Sub_metering_3 <- as.numeric(as.character(sub_data_hpc$Sub_metering_3))

#Draw Plot#4

par(mfrow=c(2,2))
with(sub_data_hpc, plot(exact_time,Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab=""))

with(sub_data_hpc, plot(exact_time,Voltage,type="l",ylab="Voltage",xlab="datetime"))

with(sub_data_hpc, plot(exact_time,Sub_metering_1,type="l",ylab="Energy sub metering",xlab=""))
with(sub_data_hpc, points(exact_time,Sub_metering_2,type="l",col="red"))
with(sub_data_hpc, points(exact_time,Sub_metering_3,type="l",col="blue"))
legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), 
       col = c("black","red","blue"), 
       lty = 1, 
       bty = "n", 
       pt.cex = 2, 
       text.col = "black", 
       horiz = F  
)

with(sub_data_hpc, plot(exact_time,Global_reactive_power,type="l",ylab="Global_reactive_power",xlab="datetime"))

dev.copy(png,file = "plot4.png", bg = "transparent",width=480,height=480)
dev.off()






