library(tidyverse)
library(forecast)
library(tseries)
library(rio)
library(readxl)
library(data.table)
##
## Delete old data sets
rm(list=ls())
## Load sample data
data1 <- fread("daily_aqi/daily_aqi_by_county_2010.csv")
data1$Date <- as.Date(data1$Date)
aqi <- data1 %>% filter(`State Name`=="Maine" & `county Name`=="Penobscot")
aqi <- as.data.table(aqi)
ggplot(aqi,aes(x=Date,y=AQI)) + geom_point(color="navyblue") + 
  facet_wrap(~Month) + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Daily Air Qualitity Index: 2010")
  

