#R code for Quesion 1

install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)
#Read data
data<-read.csv("E:/Downloads/ECONOMETRIC/data.csv")
data

#Convert data to time series
data_ts<-ts(data$PCE, start=c(1959), frequency=1)
data_ts

#Plot the time series
autoplot(data_ts)

#Check for stationarity
adf.test(data_ts)

#Taking 1st difference to make data stationary
data_ts_d1<-diff(data_ts, difference=1)
adf.test(data_ts_d1)

#Taking 2nd difference to make data stationary
data_ts_d2<-diff(data_ts, difference=2)
adf.test(data_ts_d2)

#Plot 
autoplot(data_ts_d2)

#Check for AR term
pacf(data_ts_d2)

#Check for MR term
acf(data_ts_d2)

#Fit model for ARIMA(1,2,1)
arima1<-arima(x=data_ts, order=c(1,2,1))
arima1

##Fit model for ARIMA(1,2,2)
arima2<-arima(x=data_ts, order=c(1,2,2))
arima2

#Auto fit ARIMA model
auto.arima(data_ts)

#Forecasting using ARIMA(1,2,1)
forecast(arima1, h=10)




########Solution for Question 2
install.packages("vars")
install.packages("tseries")
install.packages("urca")

library(vars)
library(tseries)
library(urca)
library(forecast)

#load the built-in Canada data
data("Canada")
head(Canada)

#Check for Stationarity
apply(Canada, 2, function(x)adf.test(x)$p.value)

#Difference the data to make it Stationary
Canada_d1<-diff(Canada)

#Check again for Stationarity
apply(Canada_d1, 2, function(x)adf.test(x)$p.value)

#Difference the data again
Canada_d2<-diff(Canada, difference=2)

#Check again for Stationarity
apply(Canada_d2, 2, function(x)adf.test(x)$p.value)

#Select optimal lag length
data_lag<-VARselect(Canada_d2,lag.max=10, type="const")
data_lag$selection

#Fit the VAR model using selected lag
Var_model<-VAR(Canada_d2, p=data_lag$selection[["AIC(n)"]], type="const")
Var_model

#Check stability (should be<1)
roots(Var_model, modulus=TRUE)

#Diagnostic tests

#Portmanteau test for autocorrelation
serial.test(Var_model, lags.pt=10, type="PT.asym")

#Test for ARCH effectd
arch.test(Var_model, lags.multi=5)

#Test for normality
normality.test(Var_model)

#Forcasting
forecast<-predict(Var_model, n.ahead=8, ci=0.95)
forecast

#Plot Forecasts
plot(forecast)




######Solution for Question 3

library(readr)
library(forecast)

#load the data
data<-read.csv("E:/Downloads/ECONOMETRIC/data_GDP.csv")

##Convert GDP_growth data to time series
gdp_ts<-ts(data$GDP_Growth, start=c(2015,1), frequency=4)

#Plot the time series
plot(gdp_ts, main="Quarterly Gdp growth", ylab= "GDP growth(%)", col="red")

#Decomposition
decomp<-decompose(gdp_ts)
decmp
plot(decomp)

#Apply Holt-Winters exponential smoothing
gdp_hw<-HoltWinters(gdp_ts)
gdp_hw

#Forecast Next 8 Quarters
gdp_forecast<-forecast(gdp_hw, h=8)
gdp_forecast






















