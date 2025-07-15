##Q1.


# Load the required libraries
library(readr)
library(forecast)

# Load the data
data <- read.csv("E:/Downloads/ECONOMETRIC/data_GDP.csv")
data
# Convert data to time series (assuming GDP_Growth is present in your dataset)
gdp_ts <- ts(data$GDP_Growth, start=c(2015, 1), frequency=4) # frequency=4 for quarterly data

# Save the plot to a PNG file
png("gdp_growth_plot.png", width = 800, height = 600)
plot(gdp_ts, main="Quarterly GDP Growth", ylab="GDP Growth (%)", col="red")
dev.off()  # Close the device

getwd()  # This will return the current working directory


# Apply Holt-Winters exponential smoothing
gdp_hw <- HoltWinters(gdp_ts)

# Display the Holt-Winters model summary
gdp_hw

# Forecast the next 8 quarters (2 years)
gdp_forecast <- forecast(gdp_hw, h=8)

# Display the forecast
gdp_forecast

# Plot the forecast
plot(gdp_forecast)

###############################

##Q2.. 


# Install the package if not already installed
install.packages("timeSeriesDataSets")

# Load the package
library(timeSeriesDataSets)

# Load the dataset
data("AirPassengers_ts")

# View the first few rows of the dataset
head(AirPassengers_ts)
print(AirPassengers_ts)

# Check the structure of the AirPassengers_ts data
str(AirPassengers_ts)

dim(AirPassengers_ts)

# Plot the time series
png("gdp_growth_plot.png", width = 800, height = 600)
plot(AirPassengers_ts, main="Air Passengers Over Time", ylab="Number of Passengers", col="blue")
dev.off()  

# Perform ADF test to check stationarity
library(tseries)
adf.test(AirPassengers_ts)

# Fit an ARIMA model (auto selection of best ARIMA model)
library(forecast)
fit <- auto.arima(AirPassengers_ts)

# View the ARIMA model summary
summary(fit)

# Forecast the next 24 months (2 years)
forecast_AirPassengers <- forecast(fit, h=24)
forecast_AirPassengers

# Plot the forecast
png("gdp_growth_plot.png", width = 800, height = 600)
plot(forecast_AirPassengers)
dev.off()  



############################################################
##Q3.

# Load necessary libraries
library(urca)  
library(vars)  

# Load the Denmark dataset
data("denmark")

# View the first few rows of the dataset
head(denmark)

# Convert the data to a time series object (excluding the 'period' column)
denmark_ts <- ts(denmark[, -1], start = c(1974, 1), frequency = 4)

# Check the structure of the time series
str(denmark_ts)

# Check for stationarity using the Augmented Dickey-Fuller (ADF) test
# Apply the ADF test on the first variable (LRM - Logarithm of real money)
adf_test_LRM <- ur.df(denmark_ts[, 1], type = "none", lags = 4)
summary(adf_test_LRM)

# Apply the ADF test on the second variable (LRY - Logarithm of real income)
adf_test_LRY <- ur.df(denmark_ts[, 2], type = "none", lags = 4)
summary(adf_test_LRY)

# Apply the ADF test on the third variable (LPY - Logarithm of price deflator)
adf_test_LPY <- ur.df(denmark_ts[, 3], type = "none", lags = 4)
summary(adf_test_LPY)

# Apply the ADF test on the fourth variable (IBO - Bond rate)
adf_test_IBO <- ur.df(denmark_ts[, 4], type = "none", lags = 4)
summary(adf_test_IBO)

# Apply the ADF test on the fifth variable (IDE - Bank deposit rate)
adf_test_IDE <- ur.df(denmark_ts[, 5], type = "none", lags = 4)
summary(adf_test_IDE)



# Differencing the data to achieve stationarity


denmark_diff1 <- diff(denmark_ts, differences = 1)

# Check if the series has become stationary after first differencing
# Apply the ADF test on each differenced variable

adf_test_LRM_diff1 <- ur.df(denmark_diff1[, 1], type = "none", lags = 4)
summary(adf_test_LRM_diff1)

adf_test_LRY_diff1 <- ur.df(denmark_diff1[, 2], type = "none", lags = 4)
summary(adf_test_LRY_diff1)

adf_test_LPY_diff1 <- ur.df(denmark_diff1[, 3], type = "none", lags = 4)
summary(adf_test_LPY_diff1)

adf_test_IBO_diff1 <- ur.df(denmark_diff1[, 4], type = "none", lags = 4)
summary(adf_test_IBO_diff1)

adf_test_IDE_diff1 <- ur.df(denmark_diff1[, 5], type = "none", lags = 4)
summary(adf_test_IDE_diff1)



# Apply second differencing to LPY
denmark_diff2 <- diff(denmark[, 3], differences = 2)  

# Apply the ADF test after second differencing
adf_test_LPY_diff2 <- ur.df(denmark_diff2, type = "none", lags = 4)
summary(adf_test_LPY_diff2)




# Apply first differencing to each variable in the dataset
LRM_diff1 <- diff(denmark[, 2], differences = 1)  # First differencing for LRM
LRY_diff1 <- diff(denmark[, 3], differences = 1)  # First differencing for LRY
LPY_diff1 <- diff(denmark[, 4], differences = 1)  # First differencing for LPY
IBO_diff1 <- diff(denmark[, 5], differences = 1)  # First differencing for IBO
IDE_diff1 <- diff(denmark[, 6], differences = 1)  # First differencing for IDE

# Combine the differenced variables into a data frame
denmark_diff1 <- data.frame(LRM_diff1 = LRM_diff1, 
                            LRY_diff1 = LRY_diff1, 
                            LPY_diff1 = LPY_diff1, 
                            IBO_diff1 = IBO_diff1, 
                            IDE_diff1 = IDE_diff1)

# Check the structure of the differenced data frame
str(denmark_diff1)


# Select the optimal lag length using AIC (max lag = 10)
library(vars)
lag_selection <- VARselect(denmark_diff_ts, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection[["AIC(n)"]]  # Use AIC to choose the lag
print(optimal_lag)



# Fit the VAR model using the optimal lag length
var_model <- VAR(denmark_diff, p = optimal_lag, type = "const")

# Forecast for 2 years (8 quarters ahead)
forecast_result <- predict(var_model, n.ahead = 8)

# View the forecast results
forecast_result

#Plot
plot(forecast_result)










