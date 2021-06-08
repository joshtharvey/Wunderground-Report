library(robotstxt)
library(rvest)
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
library(XML)
library(RCurl)
library(httr)
library(RSelenium)
#https://texaset.tamu.edu/DataSummary/Daily/81?daysInSummary=7


paths_allowed(paths = c("https://texaset.tamu.edu/DataSummary/Daily/81?daysInSummary=7"), force=T)


# Define certicificate file
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
  browserName = 'Chrome')

remDr$navigate("http://www.google.com/")




plot(fridge.prices)
abline(coef(lm(price~cu.ft, fridge.prices)))


new.price <- data.frame(cu.ft = c(15:22))
pm<-predict(lm(price~cu.ft, fridge.prices), new.price)
slopes <- cbind(c(15:22), pm)
slopes
points(19.1, 629, pch='x', col='red')
abline(v=19.1, col='red')

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

url <- 'https://www.fidelity.com/fund-screener/evaluator.shtml#!&ft=BAL_all&ntf=N&expand=%24FundType&rsk=5'
page <- GET(url)

print(http_status(page))


checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
startServer() # run Selenium Server binary


remDr <- rsDriver(port=4444L, browser = 'chrome')

remDr <- remoteDriver(browserName="chrome", port=4444L) # instantiate remote driver to connect to Selenium Server
remDr$open(silent=T) # open web browser