city <- 'Chicago'
require(ggmap)
require(ggplot2)
require(lubridate)
require(dplyr)
require(gridExtra)


#CHICAGO#
if(city=='Chicago') {
  folder <- paste(city,'Crime',sep='')
  setwd(paste("C:/Users/dylan_000/OneDrive/Documents/",folder,sep=''))
  
  weather <- tbl_df(read.csv('ChiWeather.csv',header=TRUE, stringsAsFactors=FALSE))
  weather <- filter(weather, STATION_NAME == 'CHICAGO OHARE INTERNATIONAL AIRPORT IL US')
  weather$DATE <- as.Date(as.character(weather$DATE), format='%Y%m%d')
  
  #Convert from tenths of centigrade to centigrade
  #weather$TMAX <- weather$TMAX / 10.0
  
  #Convert to Fahrenheit
  #weather$TMAX <- (weather$TMAX * 1.8) + 32
  
  #High temps over time
  ggplot(data=weather, aes(DATE,TMAX)) + geom_point()
  
  
  #Read in crime data
  crime <- tbl_df(read.csv('crime.csv', header=TRUE, stringsAsFactors=FALSE))
  
  #Clean up date format
  crime$Date <- as.Date(crime$Date, format='%m/%d/%Y %I:%M:%S %p')
  dates <- weather$DATE
  crime <- arrange(crime, Date)
  length(crime$Date) == length(weather$DATE)
  crime <- select(crime, Date, Block, IUCR, Primary.Type, Description, 
                  Location.Description, Arrest, Domestic, Beat, 
                  District, Ward, Community.Area, FBI.Code, Latitude, Longitude)
  crime$Primary.Type[crime$Primary.Type %in% c('NON-CRIMINAL (SUBJECT SPECIFIED)',
                                               'NON - CRIMINAL', 'NON-CRIMINAL')] <- 'NON-CRIMINAL'

  countsByDay <- as.data.frame(table(crime$Date, crime$Primary.Type))
  
  totals <- countsByDay %>% group_by(Var1) %>% summarize(sum(Freq))
  names(totals) <- c('Date', 'CrimeTotal')
  totals$Date <- as.Date(totals$Date)
  totals <- merge(totals, weather, by.x='Date', by.y='DATE', all=TRUE)
  
  ggplot(totals[totals$PRCP==0,], aes(TMAX, CrimeTotal)) + geom_point() + geom_smooth(method=lm)
  
  ggplot(totals[totals$PRCP==0,]) + geom_point(aes(Date, CrimeTotal,fill='red')) +
    geom_point(aes(Date, TMAX))
  chiModel <- lm(data=totals[totals$PRCP==0,], CrimeTotal ~ TMAX)
  summary(chiModel)
}

#ALBANY#
if(city == 'Albany') {
  setwd("C:/Users/dylan_000/OneDrive/Documents/AlbanyCrime")
  
  ##CRIME DATA##
  ##Load in crime data as obtained periodically from https://data.albanyny.gov/
  crime <- read.csv('albcrime.csv',header=TRUE,stringsAsFactors=TRUE)#Feb-Apr
  crime2 <- read.csv('albcrime2.csv',header=TRUE,stringsAsFactors=TRUE)#Apr-June
  crime3 <- read.csv('albcrime3.csv',header=TRUE,stringsAsFactors=TRUE)#May-July
  
  #Bind together April data from crime2 with data from May-July from crime3
  dummy <- rbind(crime2[(grepl(pattern='^4',crime2$REPORT.DATE)),],crime3[,2:length(names(crime3))])
  
  #Bind the dummy to the first crime data set, omitting the now-outdated April data in 'crime'
  crime <- rbind(crime[!(grepl(pattern='^4',crime$REPORT.DATE)),2:length(names(crime))],dummy)
  
  #Convert to a data table for use with dplyr down the road
  crime <- tbl_df(crime)
  
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
  
  ##WEATHER DATA##
  
  #Get a vector of the months we have crime data for
  months <- as.character(unique(crime$Month)) 
  
  #Read in messy data as copied,pasted,saved as csv from http://www.cbs6albany.com/weather/features/historical-data/#.VeHbf_ZVikp
  for(i in months) {
    print(i)
    if(i == 'Feb') {
      weather <- read.csv(paste('AlbWeather',i,'2015.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)
      weather <- weather[!is.na(weather$High),] #Filter out the garbage rows
    }
    else {
      dummy <- read.csv(paste('AlbWeather',i,'2015.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)
      dummy <- dummy[!is.na(dummy$High),] #Filter out garbage rows
      weather <- rbind(weather, dummy) #Bind onto main data frame
    }
  }
  
  #Get rid of row.names
  row.names(weather) <- NULL 
  
  #Make the col names better
  names(weather) <- c('Date','HighTemp','LowTemp','Rain','Snow') 
  
  #Add '0' to month for cleaner format to use as.Date function
  for(i in 1:length(weather$Date)) {
    weather$Date[i] <- paste('0',weather$Date[i],sep='') 
  }
  
  #Re-format into Date class before merging with crime data set
  weather$Date <- as.Date(weather$Date,format='%m/%d/%Y') 
  
  #Assume all trace amounts are 0 for our modeling purposes
  weather$Rain[weather$Rain == 'Trace'] <- 0 
  weather$Snow[weather$Snow == 'Trace'] <- 0
  
  #Remove inches symbol
  weather$Rain <- as.numeric(sapply(weather$Rain, function(x) gsub(x=x,pattern='"',replacement='')))
  weather$Snow <- as.numeric(sapply(weather$Rain, function(x) gsub(x=x,pattern='"',replacement=''))) 
  
  #Remove asterisks
  weather$LowTemp <- as.numeric(sapply(weather$LowTemp,function(x) gsub(x=x,"*",'')))
  
  #Add weather data to crime data set
  crime <- merge(weather, crime, by.x = 'Date', by.y = 'Date') 
  
  #Create dummy data table which holds frequency of each crime by date
  dum <- tbl_df(data.frame(table(crime$Date,crime$Type))) 
  
  #Caclulate total number of crimes per day
  dum <- dum %>% group_by(Var1) %>% summarise(sum(Freq))
  
  #Add col names
  names(dum) <- c('Date','CrimeTotal')
  
  #Convert date in crime total dummy set
  dum$Date <- as.Date(dum$Date)
  
  #Merge crime total dummy set with weather data for analysis
  totals <- tbl_df(merge(weather, dum, by.x = 'Date', by.y = 'Date'))
  
  #Create new feature for daily temperature differential 
  totals <- mutate(totals, TempRange = HighTemp-LowTemp)
  
  #Create new feature for total precipitation
  totals <- mutate(totals, Precip = Snow + Rain)
  
  #Linear regression model of daily crime total against daily temperature high
  model1 <- lm(totals$CrimeTotal ~ totals$HighTemp)
  
  #Scatterplot of crime total vs. high temperature with line fit
  qplot(HighTemp, CrimeTotal, data=totals, geom='point',fill=I('black')) + geom_smooth(method=lm)
  
  ggplot(totals[totals$Precip == 0,], aes(HighTemp,CrimeTotal)) + 
    geom_point() +
    geom_smooth(method=lm)
  
  ggplot(totals) + geom_point(aes(Date,log(CrimeTotal))) +
    geom_point(aes(Date, log(HighTemp)))
  
  
  
  summary(lm(data=totals, CrimeTotal ~ HighTemp))
}

if(city=='Seattle') {
  folder <- paste(city,'Crime',sep='')
  setwd(paste("C:/Users/dylan_000/OneDrive/Documents/",folder,sep=''))
  
  crime <- read.csv('seattle.csv', header=TRUE, stringsAsFactors=FALSE)
  crime$Date <- as.Date(crime$Date.Reported, format='%m/%d/%Y %I:%M:%S %p')
  crime <- arrange(crime, Date)
  crime <- crime[crime$Summarized.Offense.Description %in% c('HOMICIDE','PROPERTY DAMAGE',
                                                             'NARCOTICS','CAR PROWL','ASSAULT','WARRANT ARREST','OTHER PROPERTY','DISTURBANCE',
                                                             'VEHICLE THEFT','BURGLARY','SHOPLIFTING','TRESPASS','STAY OUT OF AREA OF DRUGS',
                                                             'TRAFFIC','ROBBERY','PROSTITUTION','STOLEN PROPERTY','OBSTRUCT','INJURY','BIKE THEFT',
                                                             'WEAPON','LOST PROPERTY','DISPUTE','HARBOR CALLS','LIQUOR VIOLATION','HARBOR CALLS',
                                                             'PICKPOCKET','ILLEGAL DUMPING','ANIMAL COMPLAINT','FIREWORK','PUBLIC NUISANCE',
                                                             'RECOVERED PROPERTY','PURSE SNATCH','DISORDERLY CONDUCT','RECKLESS BURNING',
                                                             'STAY OUT OF AREA OF PROSTITUTION','LOITERING','ELUDING','ESCAPE','Shoplifting',
                                                             'Other Property','HARBOR CALLs','Car Prowl','BIAS INCIDENT','Purse Snatch',
                                                             'BURGLARY-SECURE PARKING-RES','Bike Theft','MAIL THEFT','METRO'),]
  dailyTotals <- as.data.frame(table(crime$Date,crime$Summarized.Offense.Description))
  names(dailyTotals) <- c('Date', 'Type', 'Total')
  dailyTotals$Date <- as.Date(dailyTotals$Date)
  
  dailyTotals <- dailyTotals %>% group_by(Date) %>% summarize(sum(Total))
  names(dailyTotals) <- c('Date', 'Total')
  dates <- unique(crime$Date)
  weather <- tbl_df(read.csv('seaWeather.csv', header=TRUE, stringsAsFactors=FALSE))
  weather$DATE <- as.Date(as.character(weather$DATE), format='%Y%m%d')
  weather <- weather[weather$DATE %in% dates,]
  #Convert from tenths of centigrade to centigrade
  weather$TMAX <- weather$TMAX / 10.0
  
  #Convert to Fahrenheit
  weather$TMAX <- (weather$TMAX * 1.8) + 32
  
  totals <- merge(dailyTotals, weather, by.x='Date', by.y='DATE',all=TRUE)
  totals <- totals[!is.na(totals$TMAX),]
  totals <- totals[!(is.na(totals$Date)),]
  
  ggplot(totals, aes(Date, Total)) + geom_point() + geom_smooth()
  ggplot(totals[year(totals$Date) %in% c('2011','2012','2013', '2014'),]) +
    geom_point(aes(Date, log(TMAX))) + 
    geom_point(aes(Date, log(Total)))
  
  ggplot(totals, aes(TMAX, Total)) + geom_point() + geom_smooth(method=lm)
  
  summary(lm(data=totals[year(totals$Date) %in% c('2011','2012','2013', '2014'),], 
             Total ~ TMAX))
  
  str(crime)
}
