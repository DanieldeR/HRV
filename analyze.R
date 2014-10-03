# maxMin <- function(data){
#   
#   data$minMax <- rep(NA, nrow(data))
#   data$delta <- rep(NA, nrow(data))
#   
#   data$minMax[1] <- data[1,1]
#   last <- 1
#   
#   for(i in 2:(length(data$NN)-1)){
#     if(data[i+1,1] > data[i,1] & data[i-1,] > data[i,1] | data[i+1,1] < data[i,1] & data[i-1,1] < data[i,1]){
#       data$minMax[i] <- data[i,1]
#       data$delta[i] <- data[i,2] - data[last,2]
#       last <- i
#     }else if(data[i+1,1] > data[i,1] | data[i+1,1] < data[i,1]){
#      next() 
#     }
#   }
#   
#   return(data)
# }

#NN filter smooths the data using a moving window average with a rejection factor
NNfilter <- function(data, window = 20, rejectFac = .2){
  library(caTools)
  
  #Reject all values that are smaller than 400 ms or larger than 2000ms
  for(i in 1:length(data$NN)){
    if(data$NN[i] < 400 | data$NN[i] > 2000){
      data$NN[i] <- 0
    }
  }
  
  #Compute a moving window mean for each value
  data$mean <- runmean(data$NN, window)
  
  #determine the percentage away a given value is away from its nearby means
  data$reject <- abs(data$mean - data$NN)/data$NN
  
  #for each value, if the percentage away from the nearby means if greater than the nearby means by a factor of rejectFac, it will be rejected
  for(i in 1:length(data$NN)){
    if(data$reject[i] > rejectFac){
      data$NN[i] <- NA
    }
  }
  
  #return a cleaned dataset
  return(data[!is.na(data$NN),])
}

#This fuction creates and outputs a frequency analysis of the HRM data. This is unreliable are we're using
#the spectrum command and have little control over it. It could be re-written using a manually implemented
#FFT but this is a to-do
fSeries <- function(file, filter = T){
  source("read.hrm.R")
  
  #get the data from the .hrm file
  data <- read.hrm(file)
  
  if(filter == T){
    #Clean the data as suggested
    data <- NNfilter(data)
  }
  
  #Create a spectrum density estimate of the data. This has parameters that can be fine tuned
  spec <- spectrum(data$NN)
  
  #Approximate the spectrum density with a function. This has parameters that can be fine tuned
  f <- approxfun(spec$freq,spec$spec)
  
  #Integrate the total
  total <- integrate(f, min(spec$freq), 0.4, subdivisions = 2000)$val
  
  #Compute VLF, LF, HF
  VLF <- integrate(f, min(spec$freq), 0.04, subdivisions = 2000)$val
  LF <- integrate(f, 0.04, 0.15, subdivisions = 2000)$val
  HF <- integrate(f, 0.15, 0.4, subdivisions = 2000)$val
  
  #Compute LF norm and HF norm
  LFnorm <- LF/(total-VLF)*100
  HFnorm <- HF/(total-HF)*100
  
  #Compute the ratio of LF over HF
  LFoverHF <- LF/HF
  
  #Create a data frame to return
  DF <- data.frame(total=total, VLF=VLF, LF=LF, HF=HF, LFnorm=LFnorm, HFnorm = HFnorm, LFoverHF = LFoverHF)
  
  return(DF)
}

#This function performs time series analyses on the data and outputs a summary table
tSeries <- function(file, filter = T){
  source("read.hrm.R")
  
  #get the data from the .hrm file
  data <- read.hrm(file)
  
  if(filter == T){
    #Clean the data as suggested
    data <- NNfilter(data)
  }
  
  #Calculate mean of NN
  mean <- mean(data$NN)
  
  #Calculate sd of NN
  sd <- sd(data$NN)
  
  #Calculate the mean of the BPM
  data$BPM <- 60000/data$NN
  meanBPM <- mean(data$BPM)
  
  #Calculate RMSSD & pNN50
  MS <- 0
  pNN <- 0
  for(i in 1:(length(data$NN)-1)){
    MS <- MS + (data$NN[i+1] - data$NN[i])^2
    
    if(abs(data$NN[i+1] - data$NN[i]) > 50){
      pNN <- pNN + 1
    }
  }
  RMSSD <- sqrt(MS/(length(data$NN)-1))
  pNN50 <- pNN/length(data$NN)
  
  ithlete <- log(RMSSD)*20
  
  #Build the DF to be returned
  DF <- data.frame(date = attr(data, "date"), time = attr(data, "startTime"), length = attr(data, "length"),RRmean = mean, RRsd = sd, pNN50 = pNN50, RMSSD = RMSSD, meanBPM = meanBPM, ithlete = ithlete)
  
  return(DF) 
}