library(ggplot2)
library(ggfortify)
library(readxl)
library(forecast)

ada <- read.csv('/Users/romeoleon/Desktop/Python & R/Stock prediction/ADA-USD (1).csv')

#Convert to time series
ada_ts <- ts(ada,frequency = 365)
ada_close <- ada_ts[,'Close']

####BELOW THE INTERMEDIATE STEPS BEFORE AUTO ARIMA####
#Plot data
autoplot(ada_close)

#Plot seasonality
ggseasonplot(ada_close, season.labels = NULL,xlab='year')

#Decompose the data
decompose_ada <- decompose(ada_close)
plot(decompose_ada)

#Select seasonality
ada_seasonal <- decompose_ada$seasonal

#Make data stationary
ada_nos <- ada_close - ada_seasonal

#Differencing the data
ndiffs(ada_nos)
ada_diff <- diff(ada_nos)
plot(ada_diff)

#plot ACF and PACF
Acf(ada_diff,plot=TRUE)
Pacf(ada_diff,plot=TRUE)
##############################

#Do all step automatically
arima_train <- auto.arima(ada_close[1:1002],stationary = FALSE,seasonal = TRUE)
accuracy(arima_train)

arima_test <- Arima(ada_close[1002:1253],model=arima_train)
accuracy(arima_test)

#Create model with all data as training
arima_final <- auto.arima(ada_close,stationary = FALSE,seasonal = TRUE) #Check if AIC is better with full dataset
ada_forecast <- forecast(arima_final,h=30)
plot(ada_forecast)
summary(ada_forecast)