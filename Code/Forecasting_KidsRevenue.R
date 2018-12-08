## Analysis and Forecasting of Kids Revenue

library(forecast)
library(tseries)

##Reading the data
kids_data = read.csv("Kids Revenue.csv")

#Selecting Revenue Column
kids_data <- kids_data[,-1]

#Structure
str(kids_data)

#Summary
summary(kids_data)

class(kids_data) #Class of Data

#Converting to Time series data
kids_ts = ts(kids_data, start = 1992, frequency = 4)

kids_ts

#===================================================================================
#Exploratory Data Analysis
#===================================================================================

start(kids_ts) #Start of Time Series

end(kids_ts) #End of Time Series

frequency(kids_ts)

plot(kids_ts, xlab = "year", ylab = "Kids Revenue", main = "Kids Revenue per each year") #This will plot the time series
abline(reg = lm(kids_ts~time(kids_ts))) #This function adds one or more straight lines through the current plot.


plot(aggregate(kids_ts,FUN = "mean"))


boxplot(kids_ts~cycle(kids_ts), xlab = "Year", ylab = "Kids Revenue", main = "Comparision of Revenue for each year") #Box plot across months will give us a sense on seasonal effect


##Important Inferences
##1. The year on year trend clearly shows that the revenue has been increasing without fail.

kids_decompose <- decompose(kids_ts,"multiplicative")
plot(kids_decompose)

kids_log <- log10(kids_ts)
plot.ts(kids_log)
head(kids_log)


##Multiplcative decomposition
kids_decompose <- decompose(kids_ts,"multiplicative")
plot(kids_decompose)

##Additive decompostion
plot(decompose(kids_ts, "additive"))

#===================================================================================
#BUILDING SMA/ EMA MODELS
#===================================================================================

library(TTR)

#plots of SMA and EMA

par(mfrow=c(2,3))

airlog_sma3 <-SMA(kids_log,2)
plot.ts(airlog_sma3, ylab = "Kids revenue lag2")
title('2 Quarters SMA')


airlog_sma6 <-SMA(kids_log,6)
plot.ts(airlog_sma6,ylab = "Kids revenue lag6")
title('6 Quarters SMA')


airlog_sma12 <-SMA(kids_log,12)
plot.ts(airlog_sma12,ylab = "Kids revenue lag12")
title('12 Quarters SMA')

airlog_ema3 <-EMA(kids_log,2)
plot.ts(airlog_ema3,ylab = "Kids revenue lag2")
title('2 Quarters EMA')

airlog_ema6 <-EMA(kids_log,6)
plot.ts(airlog_ema6,ylab = "Kids revenue lag6")
title('6 Quarters EMA')

airlog_ema12 <-EMA(kids_log,12)
plot.ts(airlog_ema12,ylab = "Kids revenue lag12")
title('12 Quarters EMA')


#error plots for the SMA and EMA methods


SMA_error=c()
EMA_error=c()
period=c(2,3,4,5,6,7,8,9,10)


for (i in period){
  kids_ts_sma2 <-SMA(kids_ts,i)
  SMA_error<-c(SMA_error,sqrt(sum((kids_ts[i:16]-kids_ts_sma2[i:16])^2)/length(kids_ts[i:16])))  
}


for (i in period){
  kids_ts_sma2 <-EMA(kids_ts,i)
  EMA_error<-c(EMA_error,sqrt(sum((kids_ts[i:16]-kids_ts_sma2[i:16])^2)/length(kids_ts[i:16])))  
}


df<-data.frame(period,SMA_error,EMA_error)


library(ggplot2)
g <- ggplot(df, aes(period, color = "Legend"))
g <- g + geom_line(aes(y=SMA_error, color = "SMA Error"),size = 1.2)
g <- g + geom_line(aes(y=EMA_error, color = "EMA Error"),size = 1.2)
g <- g + labs(x='Period',y='Error')
g <- g + labs(title='SMA Vs EMA in terms of Error for different periods') +
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))

g

#===================================================================================
#BUILDING ARIMA MODELS
#===================================================================================

par(mfrow=c(1,1))


#Seasonal Plot for Data
seasonplot(kids_ts,main = "Seasonal Plot for Kids Data", col = c('red', 'blue', 'green', 'brown'), 
           bty = 'l', year.labels = T)

#ggplot - Seasonal plot for Data
ggseasonplot(kids_ts, main = "Seasonal Plot for Kids Data", col = c('red', 'blue', 'green','brown'), year.labels = T)


plot(kids_ts, ylab = "Original Data", main = "Kids Revenue for each year")

# The following is the R code for the same with the output plot. Notice, this series is not 
# stationary on mean since we are using the original data without differencing
plot(log10(kids_ts), ylab = "Log of Original Data", main = "Kids Revenue(Log) for each year")

## The R code and output for plotting the differenced series are displayed below:
plot(diff(kids_ts), ylab='Differenced Sales',main = "Kids Revenue for each year")

# Not stationary on variance i.e. variation in the plot is increasing as we move towards 
# the right of the chart. We need to make the series stationary on variance to produce 
# reliable forecasts through ARIMA models


#Differencing
kids_log_diff = diff(kids_log, lag = 1)
plot(kids_log_diff, ylab='Log of Differenced Sales')

#Transformations such as logarithms can help to stabilise the variance of a time series. 
#Differencing can help stabilise the mean of a time series by removing changes in the level of a 
#time series, and therefore eliminating (or reducing) trend and seasonality.

par(mfrow = c(1,1))
plot(kids_log_diff, ylab='Log of Differenced Sales')
abline(reg = lm(kids_log_diff~time(kids_log_diff)))

#ADF TEST 
adf.test(kids_log_diff)


#KPSS TEST
kpss.test(kids_log_diff)

## pacf :  Degree of association between two variables while adjusting for effect 
## of one or more additional variable
## acf  :  Similarity between values of the same variable across observations 
## pacf is used for p (auto regressive component which is regression of value with lag time) and 
## acf is used for ## q (moving average component which is regression of error with its lag)

## If acf has less bars outside the boundary compared to pacf, then, 
## we will use acf which is q and p =0 

## If pacf has less bars outside the boundary compared to acf, then, 
## we will use pacf which is p and q =0

#BUILDING ARIMA MODELS

#The following is the R code to produce ACF and PACF plots.
tsdisplay(kids_log)

arima1 <- arima(kids_log, order= c(4,1,4))
summary(arima1)

#PREDICTION OF VALUES
pred = predict(arima1,6)

#Converting to Normal scale
10^pred$pred

#Prediction using forecast function
q <- forecast(arima1, h = 6)
q

accuracy(q)

#Plotting the estimation 
plot(q, main = "Forecast from ARIMA for 6 time periods")

#Check Residuals
checkresiduals(arima1)

#Residuals Plot
par(mfrow=c(1,1))
plot.ts(arima1$residuals)
acf(ts(arima1$residuals),main='ACF Residual')
pacf(ts(arima1$residuals),main='PACF Residual')

#===================================================================================
#BUILDING NAVIE MODELS
#===================================================================================
#install.packages("fpp")
library(fpp)

## Mean : Mean of historical data
## Naive : Forecasts equal to last observed value
## Seasonal Naive : Forecasts equal to last value from same season
## drift : Forecasts equal to last value plus average change

plot(kids_ts)

#Meanf Forecasting
forecast_mean <- meanf(kids_ts, h = 8)
plot(forecast_mean)
accuracy(forecast_mean)


#Naive Forecasting
forecast_naive <- naive(kids_ts,h = 8)
plot(forecast_naive)
accuracy(forecast_naive)

#Seasonal native
forecast_seasonalnaive <- snaive(kids_ts, h=8)
plot(forecast_seasonalnaive)
accuracy(forecast_seasonalnaive)

#Forecast drift
forecast_drift <- rwf(kids_ts,drift = TRUE, h=8)
plot(forecast_drift)
accuracy(forecast_drift)

library(ggplot2)

#https://otexts.org/fpp2/simple-methods.html

# Plot some forecasts
autoplot(kids_ts) +
  autolayer(meanf(kids_ts, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(kids_ts, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(kids_ts, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Kids Revenue") +
  xlab("Year") + ylab("Kids Revenue") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))

#===================================================================================
#BUILDING ETS MODELS
#===================================================================================
#https://otexts.org/fpp2/arima-ets.html

# ets plot
kids_ets = ets(kids_ts)
summary(kids_ets)
plot(forecast(kids_ts, h = 8))


# Comparison with seasonal Holt Winters model
plot(hw(kids_ts, h = 8))
summary(hw(kids_ts))
forecast(hw(kids_ts, h = 8))

## Cross Validation of 2 models
kids_ets = ets(kids_ts)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(arima(order = c(4,1,4), x), h = h)
}

etserror = tsCV(kids_ts, forecastets, h=8)
arimaerror = tsCV(kids_ts, forecastarima, h=8)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)


checkresiduals(hw(kids_ts, h = 8))

#Building ETS Model
fit2 <- ets(kids_ts,model="ZAM",damped=FALSE)
plot(forecast(fit2, h = 8))












