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

# //////////////////////////////  TEST PLOTTING ///////////////////////////////////////////////


test<-structure(list(Time = c("12:15 AM", "12:35 AM", "12:55 AM", "1:15 AM", 
"1:35 AM", "1:55 AM", "2:15 AM", "2:35 AM", "2:55 AM", "3:15 AM", 
"3:35 AM", "3:55 AM", "4:15 AM", "4:35 AM", "4:55 AM", "5:15 AM", 
"5:35 AM", "5:55 AM", "6:15 AM", "6:35 AM", "6:55 AM", "7:15 AM", 
"7:35 AM", "7:55 AM", "8:15 AM", "8:35 AM", "8:55 AM", "9:15 AM", 
"9:35 AM", "9:55 AM", "10:15 AM", "10:35 AM", "10:55 AM", "11:15 AM", 
"11:35 AM", "11:55 AM", "12:15 PM", "12:35 PM", "12:55 PM"), 
Temperature = c("79 F", "79 F", "79 F", "78 F", "78 F", "77 F", 
"77 F", "76 F", "76 F", "76 F", "76 F", "75 F", "75 F", "75 F", 
"75 F", "74 F", "74 F", "74 F", "74 F", "74 F", "74 F", "74 F", 
"74 F", "74 F", "75 F", "75 F", "76 F", "76 F", "78 F", "78 F", 
"79 F", "79 F", "80 F", "80 F", "83 F", "83 F", "84 F", "84 F", 
"85 F"), `Dew Point` = c("79 F", "79 F", "79 F", "78 F", 
"78 F", "77 F", "77 F", "76 F", "76 F", "76 F", "76 F", "75 F", 
"75 F", "75 F", "75 F", "74 F", "74 F", "74 F", "74 F", "74 F", 
"74 F", "74 F", "74 F", "74 F", "75 F", "75 F", "76 F", "76 F", 
"75 F", "76 F", "76 F", "76 F", "76 F", "76 F", "76 F", "76 F", 
"75 F", "75 F", "75 F"), Humidity = c("100 %", "100 %", "100 %", 
"100 %", "100 %", "100 %", "100 %", "100 %", "100 %", "100 %", 
"100 %", "100 %", "100 %", "100 %", "100 %", "100 %", "100 %", 
"100 %", "100 %", "100 %", "100 %", "100 %", "100 %", "100 %", 
"100 %", "100 %", "100 %", "99 %", "92 %", "94 %", "91 %", 
"90 %", "88 %", "87 %", "81 %", "79 %", "74 %", "73 %", "72 %"
), Wind = c("SE", "SE", "SE", "SE", "SE", "SE", "SE", "ESE", 
"ESE", "ESE", "SE", "SE", "ESE", "SE", "ESE", "ESE", "ESE", 
"ESE", "ESE", "ESE", "SE", "SE", "SE", "ESE", "SE", "SE", 
"SSE", "SSE", "SSE", "SE", "SSE", "SSE", "SSE", "S", "SSE", 
"SSE", "SSE", "S", "SSE"), `Wind Speed` = c("9 mph", "10 mph", 
"10 mph", "7 mph", "7 mph", "8 mph", "9 mph", "6 mph", "7 mph", 
"8 mph", "7 mph", "7 mph", "9 mph", "9 mph", "6 mph", "7 mph", 
"7 mph", "8 mph", "6 mph", "9 mph", "9 mph", "7 mph", "5 mph", 
"8 mph", "8 mph", "8 mph", "9 mph", "10 mph", "9 mph", "9 mph", 
"10 mph", "9 mph", "9 mph", "7 mph", "8 mph", "8 mph", "7 mph", 
"9 mph", "8 mph"), `Wind Gust` = c("0 mph", "16 mph", "16 mph", 
"16 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", 
"0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", 
"0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", 
"0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", 
"0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", "0 mph", 
"0 mph"), Pressure = c("28.92 in", "28.91 in", "28.91 in", 
"28.91 in", "28.90 in", "28.90 in", "28.90 in", "28.90 in", 
"28.89 in", "28.90 in", "28.89 in", "28.89 in", "28.89 in", 
"28.89 in", "28.89 in", "28.89 in", "28.89 in", "28.89 in", 
"28.89 in", "28.89 in", "28.89 in", "28.90 in", "28.90 in", 
"28.91 in", "28.91 in", "28.91 in", "28.92 in", "28.92 in", 
"28.92 in", "28.92 in", "28.92 in", "28.92 in", "28.92 in", 
"28.93 in", "28.92 in", "28.93 in", "28.93 in", "28.93 in", 
"28.93 in"), Precip. = c("0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in", 
"0.0 in", "0.0 in", "0.0 in", "0.0 in", "0.0 in"), Condition = c("Mostly Cloudy", 
"Cloudy", "Cloudy", "Cloudy", "Cloudy", "Mostly Cloudy", 
"Mostly Cloudy", "Mostly Cloudy", "Cloudy", "Cloudy", "Cloudy", 
"Cloudy", "Cloudy", "Cloudy", "Cloudy", "Mostly Cloudy", 
"Mostly Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", 
"Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", 
"Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", 
"Cloudy", "Cloudy", "Cloudy", "Cloudy", "Cloudy", "Mostly Cloudy"
)), row.names = c(NA, -39L), class = "data.frame")


test

test$Time <- strptime(test$Time, '%I:%M %p')

test[,c(2:4,6:9)]<-apply(test[,c(2:4,6:9)], MARGIN = 2, function(x) gsub("[A-z]+|[:%:]","",x))
test[,c(2:4,6:9)]<-lapply(test[,c(2:4,6:9)], as.numeric)

plot(test$Time, test$Temperature, type='l',las=1)
hops<-as.data.frame(approx(test$Time, test$Temperature, method='linear', n=200))
hops2<-as.data.frame(approx(test$Time, test$Humidity, method='linear', n=200))


require(scales)
points(hops[1,], col=alpha('red', 0.4), pch=16)



library(animation)



saveGIF({
ani.options(nmax=200)
for(i in 1:ani.options('nmax')){
  plot(test$Time, test$Temperature, type='l',las=1, ylim=c(70,100))
  lines(test$Time, test$Humidity, col='lightblue')
  points(hops[i,], col=alpha('red', 0.4), pch=16)
  points(hops2[i,], col=alpha('blue', 0.4), pch=16)
}
}, interval = 0.01, movie.name ='anitest.gif')

100%%.005

