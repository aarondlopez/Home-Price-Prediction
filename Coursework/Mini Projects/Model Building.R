library(rpart)
library(rpart.plot)
library(caTools)

# Test models
set.seed(123)

# create test and train data sets
split = sample.split(clean_df$AvgPriceHome, SplitRatio = 0.7)
train = subset(clean_df, split==TRUE)
test = subset(clean_df, split==FALSE)
# create linear regression model
linereg <- lm(AvgPriceHome~AvgAptRent+URateSJ+Rates, data = train)
linereg.pred = predict(linereg, newdata = test)
linereg.rmse = sqrt(mean((linereg.pred - test$AvgPriceHome)^2))
linereg.rmse
# [1] 42637.19
# K-fold cross validation for linear regression model
library(DAAG)
cv.lm(clean_df, linereg, m=3)
# RMSE = 43458

# create regression tree model
treereg <- rpart(AvgPriceHome~AvgAptRent+URateSJ+Rates, data = train)
prp(treereg)
treereg.pred = predict(treereg, newdata = test)
treereg.rmse = sqrt(mean((treereg.pred - test$AvgPriceHome)^2))
treereg.rmse
# [1] 24843.1
# cross validation using complexity parameter (cp)
library(caret)
library(e1071)
# using 10 folds 
tr.control = trainControl(method="cv", number = 10)
# create a grid for all cp values to try
cp.grid = expand.grid(.cp=(0:10)*0.001)
treebest <- train(AvgPriceHome~NumJobs+AvgAptRent+Avg1bdAptRent+Avg2bdAptRent+AvgPriceCondo+TotalJobs+URateSJ+URateSJMetro+Rates, data=train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
treebest
# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.
treebest <- treebest$finalModel
prp(treebest)
best.tree.pred = predict(treebest, newdata = test)
best.tree.rmse = sqrt(mean((best.tree.pred - test$AvgPriceHome)^2))
best.tree.rmse
# [1] 3591.5


#### Time Series Model


# time-series model ARIMA models aim to describe the autocorrelations in the data, housing data is not stationary
library(xts)
clean_df_construct = clean_df[clean_df$JobSector=='Construction',]
#clean_df_construct$Date = as.Date(clean_df_construct$Date, "%m/%d/%Y")
ts <- ts(clean_df_construct$AvgPriceHome, start=c(2009,8), frequency = 12)

ts.train <- window(ts,start = c(2009,8),end = c(2014,12))
ts.test <- window(ts,start = c(2015,1))

# plot the two time series
par(mfrow=c(1,2))
plot(ts.train); plot(ts.test)

# Build Forecast
library(forecast)
#set.seed(3978)
arima1 <- auto.arima(ts.train, trace = TRUE, test = "kpss", ic="bic", seasonal = T ) # kpss is used to perform time series stationarity test
# The system has chosen ARIMA(0,1,0)(1,0,0)[12] as the best model as it has the lowest BIC.
# Thus, the Auto Regressive and Moving Average components are 0 and
# 1st order differentiation is necessary
# for the seasonal component, The auto regressive value is 1 and everything else is 0

summary(arima1)
# training set RMSE = 22741.76

confint(arima1) # 95% CI of the coefficient of auto regressive component of Seasonal
# between 0.17 and 0.63

#plot diagnostic
par(mfrow=c(1,1))
plot.ts(arima1$residuals)

# As you can see from the plot, the residuals are not fairly distributed around 0. There are
# some seasonal trends,  Overall the time series doesn't appears stationary

# Let us plot acfs and pacfs
par(mfrow=c(1,2))
acf(arima1$residuals,lag.max = 24, main = "acf of the model")

pacf(arima1$residuals,lag.max = 24, main = "pacf of the model")

# As you can see the ACFs and PACFs some times cross the peak limits, so our model has
# auto-correlation and partial auto-correlation problem

# we can verify that from Ljung Box test
Box.test(arima1$residuals,lag = 20, type = "Ljung-Box")

# The null hypothesis of the Ljung Box test is there is no autocorrelation.
# But, p-value is 0.0013, so we reject the null hypothesis

# Let us do normality test
library(tseries)
jarque.bera.test(arima1$residuals)

# Null hypothesis of the jarque bera test is residuals are normally distributed. 
# p-value = 0.4256, so we fail to reject the null hypothesis

# So, overall the model doesn't look that good.
# Let us forecast
arima1.forecast = forecast.Arima(arima1,h=12)
arima1.forecast
par(mfrow=c(1,1))
plot(arima1.forecast,xlab = "Years", ylab = "Avg. Price of Homes")

library(TSPred)
plotarimapred(ts.test,arima1,xlim = c(2015.01,2015.12),range.percent = 0.05)
accuracy(arima1.forecast,ts.test)

# While the model isn't identifying seasonality well, the actual data (dotted black) is
# in the (80% & 95%) confidence interval of the forecast




#Get testing set from clean_df that matches our time series testing set (most recent data). 
#Run predict models using the matching #data set, turn results into log then manually calculate RMSE. 
#Find a way to print out prediction results so you can log then #match to time series logged test data set for the calculations.




# Let us assume the model to be multiplicative and take a log of the data
ts <- ts(log10(clean_df_construct$AvgPriceHome), start=c(2009,8), frequency = 12)

ts.train <- window(ts,start = c(2009,8),end = c(2014,12))
ts.test <- window(ts,start = c(2015,1))

# plot the two time series
par(mfrow=c(1,2))
plot(ts.train); plot(ts.test)

# Build Forecast
library(forecast)
#set.seed(3978)
arima2 <- auto.arima(ts.train, trace = TRUE, test = "kpss", ic="bic", seasonal = T ) # kpss is used to perform time series stationarity test
# The system has chosen ARIMA(0,1,0)(1,1,0)[12] as the best model as it has the lowest BIC.
# Thus, the Auto Regressive and Moving Average components are 0 and
# 1st order differentiation is necessary
# for the seasonal component, The auto regressive value is 1 and differentiated value is 1
summary(arima2)
# training set RMSE = 0.01343 # at a log scale



confint(arima2) # 95% CI of the coefficient of auto regressive component of Seasonal
# between -0.81 and -0.41; don't include 0, so significant

#plot diagnostic
par(mfrow=c(1,1))
plot.ts(arima2$residuals)

# As you can see from the plot, the residuals are not fairly distributed around 0. There are
# some seasonal trends,  Overall the time series doesn't appears stationary

# Let us plot acfs and pacfs
par(mfrow=c(1,2))
acf(arima2$residuals,lag.max = 24, main = "acf of the model")

pacf(arima2$residuals,lag.max = 24, main = "pacf of the model")

# As you can see the ACFs and PACFs some times cross the peak limits, but, the model
# seems much better than arima1

# we can verify that from Ljung Box test
Box.test(arima2$residuals,lag = 20, type = "Ljung-Box")

# The null hypothesis of the Ljung Box test is there is no autocorrelation.
# But, p-value is 0.1282, so we fail to reject the null hypothesis

# Let us do normality test
library(tseries)
jarque.bera.test(arima2$residuals)

# Null hypothesis of the jarque bera test is residuals are normally distributed. 
# p-value = 0.3749, so we fail to reject the null hypothesis

# So, overall the model looks that good.
# Let us forecast
arima2.forecast = forecast.Arima(arima2,h=12)
arima2.forecast
par(mfrow=c(1,1))
plot(arima2.forecast,xlab = "Years", ylab = "Avg. Price of Homes")

library(TSPred)
plotarimapred(ts.test,arima2,xlim = c(2015.01,2015.12),range.percent = 0.05)
accuracy(arima2.forecast,ts.test)

# The model is looking good.

# The root mean squared error and mean absolute error can only be compared between models whose errors are measured in the same units

#(e.g., dollars, or constant dollars, or cases of beer sold, or whatever). 
# If one model's errors are adjusted for inflation while those of another or not, 
# or if one model's errors are in absolute units while another's are in logged units,
#their error measures cannot be directly compared. 
#In such cases, you have to convert the errors of both models into comparable units
#before computing the various measures. This means converting the forecasts of one 
#model to the same units as those of the other by unlogging or undeflating 
# (or whatever), then subtracting those forecasts from actual values to obtain 
# errors in comparable units, then computing statistics of those errors. You cannot
# get the same effect by merely unlogging or undeflating the error statistics themselves!

