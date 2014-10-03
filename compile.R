
#This function compiles all of the data from a specified location (typically "Data") using the specified series
compile <- function(dir = "Data/", series = "time", filter = T, url = "https://drive.google.com/open?id=0Bwgdzc7ItEK3c0dIYm14UGFJZmc&authuser=0", local = F){
  source("analyze.R")
  source("getGoogle.R")
  
  #If we do not want to analyze cloud data, the flag can be set so that only local data is analyzed. This can
  #be used in combination with the dir flag to specify another folder with .hrm data for testing purposes
  if(local == F){
    getGoogle(url)
  }
  
  #Iterates through the list of files in the specified directory and performs an analysis on the data within
  #it will then append a line to the output dataframe
  n = 1
  for(i in list.files(dir, full.names = T)){
    if(series == "time"){
      if(n > 1){
        data <- rbind(data, tSeries(i, filter))
      }else{
        data <- tSeries(i, filter)
      }
    }else if(series == "frequency"){
      if(n > 1){
        data <- rbind(data, fSeries(i, filter))
      }else{
        data <- fSeries(i, filter)
      }
    }
    
    n <- n + 1
  }
  
  #The data frame is returned to the operator
  data
}