albCrime <- function() {
  require(ggmap)
  require(ggvis)
  require(ggplot2)
  require(lubridate)
  require(dplyr)
  require(gridExtra)
  require(caret)
  require(devtools)
  library(shiny)
  library(leaflet)
  library(rCharts)
  library(rMaps)
  setwd("C:/Users/dylan_000/OneDrive/Documents/AlbanyCrime")
  
  ##CRIME DATA##
  ##Load in crime data as obtained periodically from https://data.albanyny.gov/
  crime <- read.csv('AlbanyCrimeData.csv',header=TRUE,stringsAsFactors=TRUE)#Feb-Apr
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
  crime$Date <- as.Date(crime$Date,format='%m/%e/%Y')
  
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

  #Query Google Maps for a map of Albany
  
  map <- get_map(location=c(location$lon,location$lat), zoom=13)
  
  ggmap(location) + 
    geom_point(aes(x=Longitude,y=Latitude), data=crime, shape=factor(crime$ReportCrimeType))
  
  ##Let's look at the totals for each type of crime using a barplot
  qplot(Type, data=crime, geom='bar', fill=factor(Month)) + coord_flip()
  
  
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
  
  #Linear regression model of daily crime total against daily precipitation
  model2 <- lm(totals$CrimeTotal ~ totals$HighTemp)
  
  #Scatterplot of crime total vs. high temperature with line fit
  qplot(HighTemp, CrimeTotal, data=totals, geom='point',fill=I('black')) + geom_smooth(method=lm)
  
  ggplot(totals[totals$Precip == 0,], aes(HighTemp,CrimeTotal)) + 
    geom_point() +
    geom_smooth(method=lm)
  
  #Scatterplot of crime total vs daily precipitation with line fit
  ggplot(totals[totals$Precip != 0,], aes(Precip, CrimeTotal)) + geom_point(shape=2) + geom_smooth(method=lm)
  
  #Horizontal barplot of the number of crimes commited in each location type, color-coded by month
  qplot(LocationType, data=crime, geom='bar', fill=factor(Month)) + coord_flip()
  
  ##LOAD HISTORICAL UCR DATA##
  histCrime <- read.csv('HistoricalAlbCrimeData.csv',head=T,stringsAsFactors=F)
  histCrime <- tbl_df(histCrime)
  ggplot(histCrime[histCrime$Year %in% c('2007','2008','2009','2010','2011','2012'),], aes(Year, Violent.Crime.rate)) + geom_point() + geom_line()
  
  ##PERSONNEL TOTALS##
  
  #Read in historical personnel/staffing data for Albany P.D.
  albPersonnel <- read.csv('AlbPolicePersonnelTotals.csv',head=T,stringsAsFactors=F)

  #Reverse the data frame to make it chronological
  albPersonnel <- albPersonnel[10:1,]
  
  #Merge the Albany historical crime data with the Albany P.D. personnel data for simpler analysis
  crimeAndPersonnel <- merge(albPersonnel, histCrime)
  
  #Generate plot of total sworn officers of Albany P.D. by year
  p1 <- ggplot(crimeAndPersonnel, aes(Year, Sworn.Total)) + geom_point() + geom_line() 
  
  #Generate plot of the MV theft rate by year
  p2 <- ggplot(crimeAndPersonnel, aes(Year, Motor.vehicle.theft.rate)) + geom_point() + geom_line()
  
  #Generate plot of the burg rate by year
  p3 <- ggplot(crimeAndPersonnel, aes(Year, Burglary.rate)) + geom_point() + geom_line()
  
  #Show the three above plots in one pane
  grid.arrange(p1,p2,p3)
  
  
  #Generate plot of the violent crime rate by year from 2005-2015 
  p4 <- ggplot(histCrime[histCrime$Year %in% c('2005','2006','2007','2008','2009','2010','2011','2012'),], aes(Year, Violent.Crime.rate)) + geom_point() + geom_line()
  
  #Generate plot of the property crime rate by year from 2005-2015
  p5 <- ggplot(histCrime[histCrime$Year %in% c('2005','2006','2007','2008','2009','2010','2011','2012'),], aes(Year, Property.crime.rate)) + geom_point() + geom_line()
  
  #Show the two above plots in one pane
  grid.arrange(p4,p5)
  
  ##EMPLOYMENT DATA##
  empData <- read.csv('empData.csv',header=T)
  empData <- tbl_df(empData)
  empData <- empData %>% group_by(Year) %>% 
    mutate(Average.UR=mean(unemployment.rate))
  p6 <- ggplot(empData[empData$Year %in% 
                         c('2005','2006','2007','2008','2009','2010','2011','2012'),], 
               aes(Year, Average.UR)) + geom_point() + geom_line()
  grid.arrange(p1,p4,p5,p6)
  
  
  histArrs <- tbl_df(read.csv('AdultArrByCounty.csv',header=T))
  histArrs <- histArrs %>% filter(County == 'Albany')
  histArrs <- histArrs[length(histArrs$Y):1,]
  
  p7 <- ggplot(filter(histArrs, Year >= 1985 & Year < 2013), aes(Year, Total)) + 
    geom_point() + geom_line()
  
  p8 <- ggplot(histCrime, aes(Year, Violent.crime.total)) + geom_point() + geom_line()
  grid.arrange(p7,p8)
  

  shinyUI(fluidPage(
    titlePanel('Crime in Albany'),
    sidebarLayout(
      sidebarPanel(
      selectInput('month','Choose Month',choices=unique(as.character(month(totals$Date,TRUE,FALSE))))
      
      ),
      
      mainPanel(
        plotOuput('crimeOutput')
      )
    )
  ))
  
  shinyServer(function(input,output){
    output$crimeOutput <- renderPlot(
      ggplot(totals[totals$Month==input$month,]) + geom_points(aes(HighTemp,CrimeTotal)) + geom_smooth(method='lm')
      )
  })


}




