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
## Plot Clean and UnClean  data
ggplot(data=AQI) + geom_point(aes(x=Date,y=Count,col="Clean",size=1)) + ggtitle("First Pass Cleaning[tsclean]") +
ylab("Clean Counts") + geom_point(data=AQI,aes(x=Date,y=AQI,col="AQI"))
##
## Create weekly and Monthly moving average
##
AQI$Weekly <- ma(AQI$Count,order=7)
AQI$Monthly <- ma(AQI$Count,order=30)
## Plot moving averages
##
ggplot(data=AQI) + geom_line(aes(x=Date,y=Count,col="Count")) +
  geom_line(aes(x=Date,y=Weekly,col="Weekly")) +
  geom_line(aes(x=Date,y=Monthly,col="Monthly")) +
  ggtitle("Actual Counts vs 7 & 30 Day Moving Average")

## 
## DECOMPOSTION OF THE DATA (2010 & 2011): 
## take seasonaility , trend and cycle into account
##
count_ma = ts(na.omit(AQI$Weekly),frequency=30)
decomp = stl(count_ma,s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
##
## Test for stationarity = first visual check
## 2nd Augumented Dickey-Fuller Test.
##
adf.test(count_ma,alternative="stationary")
## data:  count_ma
## Dickey-Fuller = -4.3987, Lag order = 8, p-value = 0.01
## alternative hypothesis: stationary
##
##
#################################################################
## AUTOCORRELATIONS AND CHOOSING MODEL ORDER
## ACF Plots display correlation between a series and lags
Acf(count_ma,main="")
## PACF Plots display correlation between series and its lags that
## explained previous lags.
Pacf(count_ma,main="")
##
## difference of 1 is sufficient
count_d1 = diff(deseasonal_cnt,differences = 1)
plot(count_d1)
##
adf.test(count_d1,alternative = "stationary")
























