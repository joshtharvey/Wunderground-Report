library(robotstxt)   #paths_allowed
library(rvest) # read_html; html_nodes
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)
library(XML) #readHTMLTable
library(RCurl) # Certificate file
library(httr) #GET(); content(); http_status
library(RSelenium)
#https://texaset.tamu.edu/DataSummary/Daily/81?daysInSummary=7

#paths_allowed --> robotstxt
paths_allowed(paths = c("https://texaset.tamu.edu/DataSummary/Daily/81?daysInSummary=7"), force=T)


# Define certificate file
cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")

page <- GET(
  "https://texaset.tamu.edu/", 
  path="DataSummary/Daily/81", 
  query="daysInSummary=7",
  config(cainfo = cafile)
)

x <- content(page)
tab <- sub('.*(<div id="detailedCropSummary".*?>.*</table>).*', '\\1', x)
readHTMLTable(tab)
tab.2 <- sub('.*(<div id="generalWeatherSummary".*?>.*</table>).*', '\\1', x)
readHTMLTable(tab)

#*****************************************************************************************
paths_allowed("https://www.wunderground.com/history/daily/us/tx/uvalde/KUVA/date/2020-5-9")

wunder <- GET(
  "https://www.wunderground.com/", 
  path="history/daily/us/tx/uvalde/KUVA/date/2020-5-9", 
  config(cainfo = cafile)
)
wunder<-read_html(content(wunder, as='text'))
html_nodes(wunder, ".mat-button-wrapper")

readHTMLTable(wunder, stringsAsFactors = FALSE)

remDr <- remoteDriver(
  remosteServerAddr = "localhost",
  port=4445L,
  browserName = 'Firefox')

remDr$navigate("http://www.google.com/")


url <- 'https://www.fidelity.com/fund-screener/evaluator.shtml#!&ft=BAL_all&ntf=N&expand=%24FundType&rsk=5'
page <- GET(url)

print(http_status(page))


checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
startServer() # run Selenium Server binary


remDr <- rsDriver(port=4444L, browser = 'chrome')

remDr <- remoteDriver(browserName="brave", port=4444L) # instantiate remote driver to connect to Selenium Server
remDr$open(silent=T) # open web browser




# PT 1 - DATA SCRAPING ///////////////////////////////////////////////////////////////////////////////////////

#Get current date and last week's dates

todays_date <- Sys.Date()-1
weeks_dates <- Sys.Date()-c(6:0)


# Start Selenium Remote Driver
# tcp port 4444L or 4567L

rD<-rsDriver(port = 4444L, browser = 'firefox', check=TRUE)
remDr <- rD$client

# Define webpages of interest

weekly.site <- paste0("https://www.wunderground.com/history/weekly/us/tx/uvalde/KUVA", "/", todays_date)


# Extract Weekly Summary Table

remDr$navigate(weekly.site)
Sys.sleep(5)
html <- read_html(remDr$getPageSource()[[1]])
tables <- html_nodes(html, "table")
weekly.summary <- html_table(tables[2], trim=T)


bb<-as.data.frame(weekly.summary)
#Extract Daily Summary Tables

rain.total<-bb[grep('Total', bb[,1]):nrow(bb),]
met.vals<-bb[1:(grep('Total', bb[,1])-1),]


met.vals<-met.vals[(which(grepl('Max', met.vals[,1]))[1]):nrow(met.vals),]
test<-t(met.vals[,1:3])

test<-as.data.frame(rbind(rep(NA, ncol(test)), rep(NA, ncol(test)), test))
colnames(test)<-NULL; rownames(test)<-NULL


test[1,which(grepl('Max',test[,c(1:ncol(test))]))+1] <- c('Temperature (°F)','Dew Point (°F)',
                                                          'Humidity (%)','Wind Speed (mph)',
                                                          'Precipitation (in.)')

test<-test[,-1]

for(i in 1:ncol(test)){
  if(!is.na(test[1,i])){counter=1; test[2,i]<-as.character(weeks_dates[counter])}
  if(is.na(test[1,i])){counter=counter+1;test[2,i]<-as.character(weeks_dates[counter])}
}

rownames(test)<-c('Variable', 'Date', 'Max', 'Avg', 'Min')



for(i in 1:7){
  remDr$navigate(paste0("https://www.wunderground.com/history/daily/us/tx/uvalde/KUVA", "/", weeks_dates[i]))
  Sys.sleep(5)
  html<-read_html(remDr$getPageSource()[[1]])
  tables <- html_nodes(html, "table")
  assign(paste0("daily_table.",weeks_dates[i]), html_table(tables[1], fill=T))
  assign(paste0("hourly_table.",weeks_dates[i]), html_table(tables[2], fill=T))
  Sys.sleep(5)
}


#stop the server (R environment)
rD$server$stop()
rm(remDr)
rm(rD)
gc()


# Scrape 2 Weather Tables from Uvalde Weather Station

html <- read_html("https://texaset.tamu.edu/DataSummary/Daily/81?daysInSummary=7")
tables <- html_nodes(html, "table")

tamu.1<- html_table(tables[1], fill=T)
tamu.2<- html_table(tables[2], fill=T)

tamu.1<-as.data.frame(tamu.1[1])
colnames(tamu.1)<-gsub("\\s+", " ", tamu.1[2,])
tamu.1<-tamu.1[-c(1:2,10),]

tamu.2<-as.data.frame(tamu.2[1])
colnames(tamu.2)<-gsub("\\s+", " ", tamu.2[2,])
tamu.2<-tamu.2[-c(1:2,10),-14]

tamu.1
tamu.2


# PT 2 - PLOTTING ///////////////////////////////////////////////////////////////////////////////////

test<-as.data.frame(`hourly_table.2021-06-07`[[1]])

test<-get(paste0("hourly_table.",weeks_dates[3]), envir = .GlobalEnv)
test<-as.data.frame(test[[1]])

dput(test)







plot(test[2,], xlim=c(0,50))
plot.ts(test[c(1,2),])
tempts <- ts(test[,c(1,2)])
nrow(test)
tempts <-ts(test[,2],frequency = 56)            
plot.ts(tempts)


tst<-`daily_table.2021-06-02`[[1]]

tst<-as.data.frame(tst)

tst$Time <- strptime(tst$Time, '%I:%M %p')

tst[,c(2:4,6:9)]<-apply(tst[,c(2:4,6:9)], MARGIN = 2, function(x) gsub("\\S[A-z]+|\\S[:%:]","",x))
tst[,c(2:4,6:9)]<-lapply(tst[,c(2:4,6:9)], as.numeric)

plot(tst$Time, tst$Temperature, type='l',las=1, ylim=c(50,120))



rD$server$stop() #Close RSelenium server
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #Force quit Java processes

#
