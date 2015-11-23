getAlbCrimeData <- function() {
  try(c(require(ggmap),
  require(ggvis),
  require(ggplot2),
  require(lubridate),
  require(dplyr),
  require(gridExtra),
  require(devtools),
  require(leaflet)),
  silent=TRUE)
  
  ##CRIME DATA##
  ##Load in crime data as obtained periodically from https://data.albanyny.gov/
  dir <- paste(getwd(),'/AlbanyCrime/',sep='')
  crime <- read.csv(paste(dir,'albcrime2.csv',sep=''),header=TRUE,stringsAsFactors=TRUE)#Feb-Apr
  crime2 <- read.csv(paste(dir,'albcrime3.csv',sep=''),header=TRUE,stringsAsFactors=TRUE)#Apr-June
  crime3 <- read.csv(paste(dir,'albcrime4.csv',sep=''),header=TRUE,stringsAsFactors=TRUE)#May-July
  crime2$INCIDENT.NUMBER <- NULL
  crime3$INCIDENT.NUMBER <- NULL
  #Bind together April data from crime2 with data from May-July from crime3
  dummy <- rbind(crime2,crime2)
  
  #Bind the dummy to the first crime data set, omitting the now-outdated April data in 'crime'
  crime <- rbind(crime,dummy)
  
  #Convert to a data table for use with dplyr down the road
  crime <- tbl_df(unique(crime))
  
  #Separate out latitude
  crime$lat <- sapply(crime$ADDRESS.TYPE, FUN=function(x) {strsplit(as.character(x), '[,]')[[1]][1]}) 
  
  #Remove the pesky open paranthesis
  crime$lat <- sub("(", '', crime$lat, fixed=TRUE) 
  
  #Separate out longitude
  crime$lon <- sapply(crime$ADDRESS.TYPE, FUN=function(x) {strsplit(as.character(x), '[,]')[[1]][2]}) 
  
  #Remove the pesky close paranthesis
  crime$lon <- sub(")", '', crime$lon, fixed=TRUE) 
  
  #Var no longer needed!
  crime$ADDRESS.TYPE <- NULL 
  
  #Let's clean up the names of our vars/cols
  names(crime) <- c('Date', 'Day', 'Time', 'CrimeType', 'LocationType', 
                    'ReportCrimeType', 'HundBlock', 'Latitude', 'Longitude')
  
  #Convert the dates to character 
  crime$Date <- as.character(crime$Date)
  
  #Add '0' to month for cleaner format to use as.Date function
  for(i in 1:length(crime$CrimeType)) {
    crime$Date[i] <- paste('0',crime$Date[i],sep='') 
  }
  
  #Re-format into Date class before merging with crime data set
  crime$Date <- as.Date(crime$Date,format='%m/%d/%y')
  
  #Convert the crime data frame to a data table to use with dplyr functions
  crime <- tbl_df(crime)
  
  #Create a new Month variable
  crime <- mutate(crime, Month = month(crime$Date, label=TRUE))
  
  #Convert the coordinates to numerics
  crime$Latitude <- as.numeric(crime$Latitude)
  crime$Longitude <- as.numeric(crime$Longitude)
  
  #We don't need this crime type variable
  crime <- select(crime,-CrimeType)
  
  #Re-create the crime type variable with the reported crime type
  crime$Type <- crime$ReportCrimeType
  
  #Re-format the data variable
  crime$Date <- as.Date(crime$Date)
  
  crime$ReportCrimeType <- gsub(crime$ReportCrimeType, pattern='_', rep=' ')
  
  return(crime)
}