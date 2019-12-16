## Title: ARMIA Forcasting 
## Subject: Air Quality Index
## YouTube: Tech Know How
## Author: David Jackson (davidjayjackson@gmail.com)
## Date: 2019-12-16



library(tidyverse)
library(forecast)
library(tseries)
library(rio)
library(readxl)
library(data.table)
##
## Delete old data sets
rm(list=ls())
## Load sample data and chang date field to "date" type
data1 <- fread("daily_aqi/daily_aqi_by_county_2010.csv")
data1$Date <- as.Date(data1$Date)
data1$Month <- lubridate::month(data1$Date)
##
data2 <- fread("daily_aqi/daily_aqi_by_county_2011.csv")
data2$Date <- as.Date(data2$Date)
data2$Month <- lubridate::month(data2$Date)
##
##
aqi <- data1 %>% filter(`State Name`=="Maine" & `county Name`=="Penobscot")
aqi <- as.data.table(aqi)
##
aqi2 <- data2 %>% filter(`State Name`=="Maine" & `county Name`=="Penobscot")
aqi2 <- as.data.table(aqi2)
AQI <- rbind(data1,data2)
##
## Plot AQI over time: data1: 2010
##
ggplot(aqi,aes(x=Date,y=AQI)) + geom_line() + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Daily Air Qualitity Index: 2010")
##

## Plot AQI over time: data2: 2011
##
ggplot(aqi2,aes(x=Date,y=AQI)) + geom_line() + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Daily Air Qualitity Index: 2011")
##
## Plot month over month to see the range and outerliers: 2010 - 2011
##
AQI <- AQI %>% filter(`State Name`=="Maine" & `county Name`=="Penobscot")

ggplot(AQI,aes(x=Date,y=AQI)) + geom_point(color="navyblue") + 
  facet_wrap(~Month) + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Combined Daily Air Qualitity Index: 2010 - 2011")
##
## Create time series object: 2010 - 2011
##  tsClean function also
AQI.ts <- ts(AQI[,c("AQI")])
AQI$Count <- tsclean(AQI.ts)
##
## Plot data
ggplot(data=AQI) + geom_point(aes(x=Date,y=Count,col="Clean",size=1)) + ggtitle("First Pass Cleaning[tsclean]") +
ylab("Clean Counts") + geom_point(data=AQI,aes(x=Date,y=AQI,col="AQI"))
