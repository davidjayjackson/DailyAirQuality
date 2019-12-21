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
