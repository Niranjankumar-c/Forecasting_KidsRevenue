#Read the data file
new<-read.csv('Kids Revenue.csv')

#Creation of time series 
new_time=ts(data = new$Revenue.in.million...,frequency = 4,start=1992)
train<-new_time[1:12]
test<-new_time[13:16]


#log for differncing
test<-log(test)

#train and test data
test_new<-ts(test,frequency = 4,start=1995)
train_new<-ts(log(train),frequency = 4,start=1992)

#plots of train and test
plot(train_new)
title('Train data')
plot(test_new)
title('Test Data')

#checking the stationarity of the data
library(tseries)
adf.test(train_new)

#finding the p,q manually
tsdisplay(train_new)

#observing the time series with no lag differencing
plot(train_new)
abline(reg=lm(train_new~time(train_new)))
title('Train Data with no differencing')

#observing the time series with lag=1 differencing
plot(diff(train_new,lag=1))
abline(reg=lm(diff(train_new)~time(diff(train_new))))
title('Train Data differencing with lag 1')

#observing the time series with lag=2 differencing
plot(diff(train_new,lag=2))
abline(reg=lm(diff(train_new,lag=2)~time(diff(train_new,lag=2))))
title('Train Data differencing with lag 2')

#Arima model with p=4,d=1,q=4
arima_model<-arima(train_new,order = c(4,1,4))
summary(arima_model)

#Forecasting for next year 
q=forecast(arima_model,h=4)
plot(q, main = "Forecast from ARIMA for 4 time periods")

#Forecasting for 6 quarters  
f=forecast(arima_model,h=6)
plot(f, main = "Forecast from ARIMA for 6 time periods")


#test RMSE - #Converting to Normal values
predicted<-q$mean
sqrt(sum((2.71828^predicted-2.71828^test)^2)/length(test))


#predicted and test values comparision
pred_test_df<-data.frame(predicted =  2.71828^q$mean,actual_test= 2.71828^test)
View(pred_test_df)


#Residual plots
par(mfrow=c(1,3))
plot.ts(arima_model$residuals)
acf(ts(arima_model$residuals),main='ACF Residual')
pacf(ts(arima_model$residuals),main='PACF Residual')

par(mfrow=c(1,1))
checkresiduals(arima_model)
