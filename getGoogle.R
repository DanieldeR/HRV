#This function will download all of the files in a shared google folder to a local one. It will also check to see
#if any of the local files match those in the cloud and will not download them if they exist locally already

getGoogle <-function(googleURL = "https://drive.google.com/open?id=0Bwgdzc7ItEK3c0dIYm14UGFJZmc&authuser=0"){
  library(XML)
  library(httr)
  
  #Get the content from the google URL which happens to be the folder
  folderView <- htmlParse(content(GET(googleURL), as="text"), asText = T)
  
  #Parse the HTML and select only the flip-entries (These correspond to the file ids)
  filesXML <- xpathApply(folderView, "//div[@class='flip-entry']/@id")
  fileName <- xpathSApply(folderView, "//div[@class='flip-entry-title']", xmlValue)
  
  fileGet <- character()
  
  #Load the file names from the parsed file IDs
  for(i in 1:length(filesXML)){
    fileGet <- c(fileGet, as.character(filesXML[[i]][1]))
  }
  
  #Remoce then unwanted "entry-" for each file id
  fileGet <- gsub("entry-", "", fileGet)
  
  #Build the download url for each file
  fileGet <- paste("https://docs.google.com/uc?export=download&id=", fileGet, sep="")
  
  #Check to see if the number of files in the local directory match those in the download location
  if(length(fileGet) != length(dir("Data/"))){
    #Initialize some variables
    fileNameDownload <- character()
    fileGetDownload <- character()
    
    #Create a factor vector which indicates which local files match the remote ones
    downloadFactor <- is.element(fileName, dir("Data/"))
    
    for(i in 1:length(downloadFactor)){
      
      #For each element in the factor vector, if the remote file is not also local it will add it to the list of
      #files to to download.
      if(!downloadFactor[i]){
        fileNameDownload <- c(fileNameDownload, fileName[i])
        fileGetDownload <- c(fileGetDownload, fileGet[i])
      }
    }
    
    #Download each of the files in the list to the local directory
    for(i in 1:length(fileGetDownload)){
      download.file(fileGetDownload[i], paste("Data/", fileNameDownload[i], sep=""), method="wget")
    }
  }
}