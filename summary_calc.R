library(tidyverse)
library(forecast)
library(tseries)
## library(rio)
## library(readxl)
library(data.table)
library(RSQLite)
set.seed(1234)
##
## Delete old data sets
rm(list=ls())
## Connect to SQLite dB
##
db <- dbConnect(SQLite(),dbname="../db/airquality.sqlite3")
#
oz_daily <- dbGetQuery(db,"SELECT Date,Year,Month,
                  State_Name,AQI, avg(AQI) Mean,median(AQI) Median
                  FROM ozone
                  group by Date ;")
oz_daily$Date <- as.Date(oz_daily$Date)
##
ggplot(data=aqi,aes(x=Date,y=Mean,col="Mean")) + geom_point() +
  geom_point(data=aqi,aes(x=Date,y=Median,col="Median"))

oz_state <- dbGetQuery(db,"SELECT State_name,Date,Year,Month,
                  AQI, avg(AQI) Mean,median(AQI) Median
                  FROM ozone
                  group by State_Name,Date ;")
oz_state$Date <- as.Date(oz_state$Date)
##
 oz_state %>% filter(State_Name=="Ohio" & Year >=2000 & Year <=2010) %>%
  ggplot(aes(x=Date,y=Median,col="Median")) + geom_line() +
   geom_line(aes(x=Date,y=Mean,col="Mean"))
 ##
 ## Added oz_daily an oz_state to aircuality db.
 oz_daily$Date <- as.character(oz_daily$Date)
 dbRemoveTable(db,"oz_daily")
 dbWriteTable(db,"oz_daily",oz_daily,row.names=FALSE,overwrite=TRUE)
 dbSendQuery(db,"CREATE INDEX ozd_Year ON oz_daily(Year)")
 dbSendQuery(db,"CREATE INDEX ozd_State ON oz_daily(State_Name)")
 dbListTables(db)
##
 oz_state$Date <- as.character(oz_state$Date)
 dbRemoveTable(db,"oz_state")
 dbWriteTable(db,"oz_state",oz_state,row.names=FALSE,overwrite=TRUE)
 dbSendQuery(db,"CREATE INDEX ozs_year ON oz_state(Year)")
 dbSendQuery(db,"CREATE INDEX ozs_state ON oz_state(State_Name)")
 dbListTables(db)
 ##
 ## Moving Averges: 50/365
 oz_daily$ma50 <- ma(oz_daily$Mean, order=50)
 oz_daily$ma365 <- ma(oz_daily$Mean, order=365)
 ##
 # Plot oz_daily moving averages.
 oz_daily$Date <- as.Date(oz_daily$Date)
 oz_daily %>% 
   ggplot(aes(x=Date,y=ma50,col="50 Days")) + geom_line() +
   geom_line(aes(x=Date,y=ma365,col="365 Days")) + 
   geom_smooth(aes(x=Date,y=AQI,col="Smoothed"),method="glm") +
   ggtitle("US Daily Air Quality Index: 1989 - 2019") + 
   ylab("Air Quality Index")