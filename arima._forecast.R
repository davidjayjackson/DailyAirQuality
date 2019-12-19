## Title: ARIMA Forcasting Forecasting in R
## Subject: Air Quality Index
## YouTube: Tech Know How
## User: David Jackson (davidjayjackson@gmail.com)
## Date: 2019-12-16
## Version 1.0 (2019-12-17)


library(tidyverse)
library(forecast)
library(tseries)
## library(rio)
## library(readxl)
library(data.table)
##
## Delete old data sets
rm(list=ls())
##
## Import training data:2000 - 2018
data <-dir("aqi_train/",full.names=T) %>% 
    map_df(fread,colClasses=c("State_Code"="character","County_Code"="character"))

data$Date <- as.Date(data$Date)
data$Year <- lubridate::year(data$Date)
data$Month <- lubridate::month(data$Date)
## Plot moving averages for entire data.drame
##
data$ma7 <- ma(data$AQI,order=7)
data$ma14 <- ma(data$AQI,order=14)
A <- data %>% filter(Year ==2017) 
ggplot(data=A,aes(x=Date,y=ma14,col="ma14")) + geom_line() +
  geom_line(data=A,aes(x=Date,y=ma7,col="ma7"))
##
aqi <- data %>% filter(State_Name=="Ohio" & county_Name=="Franklin") %>%
  select(Date,Year,Month,State_Name,county_Name,AQI)
##
AQI <- as.data.table(aqi)
##
## Plot TRAIN: AQI over time: data1:   2011
##
ggplot(AQI,aes(x=Date,y=AQI)) + geom_line() + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Daily (Train) Air Qualitity Index: 2000 - 2018")
##

## Plot month over month to see the range and outerliers: 2011 & 2012
##
ggplot(AQI,aes(x=Date,y=AQI)) + geom_point(color="navyblue") + 
  facet_wrap(~Month) + scale_x_date("month") + 
  ylab("Air Quality Index") + ggtitle(" Combined Daily Air Qualitity Index: 2000 & 2018") +
  theme(axis.text.x = element_text(angle = -90))
  ##
## Create time series object: 2011 - 2012
##  tsClean function also
AQI.ts <- ts(AQI[,c("AQI")])
AQI$Count <- tsclean(AQI.ts)
##
## Plot Clean and UnClean  data
ggplot(data=AQI) + geom_line(aes(x=Date,y=Count,col="Clean",size=0.5)) + ggtitle("First Pass Cleaning[tsclean]") +
ylab("Clean Counts") + geom_line(data=AQI,aes(x=Date,y=AQI,col="AQI"))
##
## Create weekly and Monthly moving average
##
AQI$Weekly <- ma(AQI$Count,order=7)
AQI$biweekly <- ma(AQI$Count,order=14)
## Plot moving averages
##
ggplot(data=AQI) + geom_line(aes(x=Date,y=Count,col="Count")) +
  geom_line(aes(x=Date,y=Weekly,col="Weekly")) +
  geom_line(aes(x=Date,y=biweekly,col="biweekly")) +
  ggtitle("Actual Counts vs 7 & 14 Day Moving Average: 2000 - 2018")

## 
## DECOMPOSTION OF THE DATA (2000 & 2018): 
## take seasonaility , trend and cycle into account
##
count_ma = ts(na.omit(AQI$Weekly),frequency=60)
decomp = stl(count_ma,s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
##
## Test for stationarity = first visual check
## 2nd Augumented Dickey-Fuller Test.
##
adf.test(count_ma,alternative="stationary")
# Augmented Dickey-Fuller Test
# 
# data:  count_ma( 14 day moving average)
# Dickey-Fuller = -5.7194, Lag order = 11, p-value = 0.01
# alternative hypothesis: stationary

##
#################################################################
## Video #3
## AUTOCORRELATIONS AND CHOOSING MODEL ORDER
## ACF Plots display correlation between a series and lags
Acf(count_ma,main="")
## PACF Plots display correlation between series and its lags that
## explained previous lags.
Pacf(count_ma,main="")
##
## difference of 1 is sufficient
count_d1 = diff(deseasonal_cnt,differences = 20)
plot(count_d1)
##
adf.test(count_d1,alternative = "stationary")
##
## Look for spikes at specific lag points of differenced series
Acf(count_d1,main="ACF for Difference Series")
Pacf(count_d1,main="PaCF for Differenced Series")
##
## Part #4 : FITTING AN ARIMA MODEL
## Get auto fit p,d,q values
auto.arima(deseasonal_cnt,seasonal=FALSE)
# Series: deseasonal_cnt 
# ARIMA(3,1,4) with drift 
# 
# Coefficients:
#   ar1     ar2      ar3     ma1      ma2      ma3      ma4    drift
# 0.4748  0.3480  -0.1258  0.9191  -0.2888  -0.3598  -0.0350  -0.0056
# s.e.  0.4888  0.4239   0.1102  0.4902   0.3489   0.2166   0.0586   0.0528
# 
# sigma^2 estimated as 0.2446:  log likelihood=-1030.21
# AIC=2078.42   AICc=2078.54   BIC=2125.9
# 
## EVALUEATE AND ITERATE - does the model make sense?
##
fit1 <- auto.arima(deseasonal_cnt,seasonal = FALSE)
tsdisplay(residuals(fit1), lag.max=45,main='(1,1,1) MOdel Residuals')
##
## SET "q" = 8
fit2 <- arima(deseasonal_cnt,order=c(1,1,11))
tsdisplay(residuals(fit2), lag.max=45,main='(1,1,11) MOdel Residuals')
##
fit3 <- arima(deseasonal_cnt,order=c(1,1,30))
tsdisplay(residuals(fit3), lag.max=45,main='(1,1,30) MOdel Residuals')
##
## Forecast new fit model (fit3)
##
fcast <- forecast(fit3,h=45)
plot(fcast)
########################################################################
## PART #4 : ARIMA FORECASTING IN R
########################################################################
## TEST MODEL PERFORMANCE WITH A HOLDOUT SET
##
hold <- window(ts(deseasonal_cnt),start=461)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(1200:1461)]),order=c(1,1,11))
fcast_no_holdout <- forecast(fit_no_holdout,h=50)
plot(fcast_no_holdout,main="")
lines(ts(deseasonal_cnt))
##
## MOdel needs seasonality added back in
##
fit_w_seasonality = auto.arima(deseasonal_cnt,seasonal=TRUE)
seas_fcast <- forecast(fit_w_seasonality,h=50)
plot(seas_fcast)
lines(ts(count_ma))
lines(ts(deseasonal_cnt))
##
## PART #6: ARIMA Forecasting in R
##
fit5 = arima(deseasonal_cnt,order=c(5,1,2))
tsdisplay(residuals(fit5),lag.max=15,main="Seasonal Model Residuals")
#Final Fit Tested ARIMA forecast
par(mfrow=c(2,2))
fcast <- forecast(fit_w_seasonality,h=45)
plot(fcast)
##
fcast1 <- forecast(fit1,h=45)
plot(fcast1,main="Fit 1")
##
fcast2 <- forecast(fit2,h=45)
plot(fcast2,main="Fit4")
##
fcast5 <- forecast(fit3,h=45)
plot(fcast5,main="Fit3")










