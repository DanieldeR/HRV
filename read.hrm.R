#This function reads and parses the .hrm file format.

read.hrm <- function(file){
  data <- read.table(file)
  
  HRData <- data[(match("[HRData]", data$V1)+1):(match("[HRData_unscaled]", data$V1)-1),]
  HRDataUnscaled <- data[(match("[HRData_unscaled]", data$V1)+1):nrow(data),]
  
  date <- strsplit(as.character(data[4,]),"[=]")[[1]][2]
  date <- as.Date(gsub('^(.{7})(.*)$', '\\1-\\2', gsub('^(.{4})(.*)$', '\\1-\\2', date)))
  startTime <- strsplit(as.character(data[5,]),"[=]")[[1]][2]
  length <- strsplit(as.character(data[6,]),"[=]")[[1]][2] 
  
  DF <- data.frame(NN=as.integer(as.character(HRData)), NNUnscaled=as.integer(as.character(HRDataUnscaled)))
  attr(DF, "date") <- date
  attr(DF, "startTime") <- startTime
  attr(DF, "length") <- length
  
  DF
}