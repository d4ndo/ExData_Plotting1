# Usage:
#
# To download the dataset to your working directory call the function:
# getDataSet()
#
# plot2("household_power_consumption.txt")
plot2 <- function(file) {
  stopifnot(is.character(file))
  
  if(!file.exists(file)) {
    stop("File not found. PLease call the function getDataSet() to download 
         the Dataset house_power_consumption.zip or alternative download it from here
         https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip 
         and try again.")
  }
  
  #Set locale to US
  Sys.setlocale('LC_TIME', 'C')
  
  power_consumption <- read.csv(file, sep = ";", 
                                na.strings = "?", 
                                colClasses = c("character", "character", 
                                               "numeric", "numeric", 
                                               "numeric", "numeric",
                                               "numeric", "numeric", "numeric"))
  
  #Coercing $Date to Date POSIXct.
  power_consumption[,1] <- as.Date(power_consumption[,1], "%d/%m/%Y")
  
  #Add new column with POSIXlt to keep date and time.
  datetime <- strptime(paste(power_consumption[,1], power_consumption[,2]), "%Y-%m-%d %H:%M:%S")
  power_consumption <- cbind(datetime, power_consumption)
  
  #Get subset of data from (2007-02-01 to 2007-02-02).
  power_consumption <- subset(power_consumption,
                              Date >= as.Date(" 2007-02-01", "%Y-%m-%d") &
                              Date <= as.Date("2007-02-02", "%Y-%m-%d"))
  
  #Clean up NA's here. No NA's allowed at Global_active_power
  power_consumption <- power_consumption[!is.na(power_consumption$Global_active_power),]
  
  #Open Graphic Device with res = 480 x 480 px
  png(filename = "plot2.png", width = 480, height = 480, bg = "transparent")
  
  #Plot here
  plot(power_consumption$datetime, 
       power_consumption$Global_active_power, 
       type = "l", xlab="", ylab = "Global Active Power (kilowatts)")
  
  dev.off()
  print("done")
}

# getDataSet
# Download and verify dataset
# Depends on package (tools) and (utils)
getDataSet <- function() {
  library(tools)
  library(utils)
  
  download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                paste0(getwd(), "/", "household_power_consumption.zip"))
  hashdigest <- md5sum("household_power_consumption.zip")
  
  stopifnot(identical(as.character(hashdigest), "41f51806846b6b567b8ae701a300a3de"))
  unzip("household_power_consumption.zip")
}