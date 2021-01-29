
# /Oil Price Forecasting Based on ARIMA Model/
#///////////////////////////////////////////////////////////////////////


# Part 1. Artificial data
#==========================

#1.1. Simulation
#------------------

#1.1.1. For ARIMA(p, d, q) 
p = 2
d = 1
q = 2
phi_1 = .2
phi_2 = -.15
theta_1 = .3
theta_2 = -.1
noise_sd = 0.03
n = 1000


#set seed to 123 to generate same results
set.seed(123)
#1.1.1 create the ARIMA (p,d,q) for a sample size of 1000
ts.sim <- arima.sim(list(order=c(p,d,q), ar=c(phi_1,phi_2), ma=c(theta_1,theta_2)), sd=noise_sd, n=n)
plot (ts.sim, main="ARIMA (2,1,2)", ylab="values")

#1.1.2 Add a linear trend y(t)=b0+b1*t with coefficient b0=-1 and b1=0.0015
time=time(ts.sim)
trend=-1+(0.0015*time)
trended.ts.sim=ts.sim+trend

plot(trend, main="Linear Trend")
plot(trended.ts.sim, main="ARIMA(2,1,2) with Trend", ylab="values")

#1.1.3 Apply an exponential function to the trended model
exp.trended.ts.sim=exp(trended.ts.sim)
plot(exp.trended.ts.sim, main="Exponentiated ARIMA(2,1,2) with Trend", ylab="values")

#1.2.1 Divide the generated set into training set head and test set tail with a 80% for training and 20% for testing
train.ts <- ts(head(exp.trended.ts.sim, round(length(exp.trended.ts.sim)*0.8)))
plot(train.ts,main="Training Set Series", ylab="values")
length(train.ts)

h <- length(exp.trended.ts.sim)-length(train.ts)
test.ts <- ts(tail(exp.trended.ts.sim, h),start = 802)
length(test.ts)

plot(test.ts,main="Testing Set Series", ylab="values")

#1.2.2 Logarithm the training set series
log.train.ts=log(train.ts)
plot(log.train.ts,main="Logged Training Set Series", ylab="values")

#1.2.3 Detect a linear trend by regression and compare the estimate coefficients
#to real ones
fitted.trend <- lm(log.train.ts ~ time(log.train.ts))
coef(fitted.trend)

est_trend_value=fitted.trend$fitted
plot(est_trend_value,main="Detected Linear Trend", xlab="time", ylab="values")

#1.2.4 Detrend the series
detrended.train <- log.train.ts - est_trend_value

plot(detrended.train, main="Detrended Training Series", ylab="values")

#1.2.5 Plot the original ARIMA simulation and current series
plot(ts.sim[1:(round(length(ts.sim))*0.8)], type='l',main='ARIMA(2,1,2) vs. Detrended Training Series', ylab='values', xlab='time', ylim=c(min(detrended.train, ts.sim), max(detrended.train, ts.sim)))
lines(detrended.train, col='red')

#another plot
ts.plot(ts.sim)
ts.plot(detrended.train,trended.ts.sim,log.train.ts,exp.trended.ts.sim,col=c("blue","black","red","green"))
legend("topleft", legend=c("Detrended Training","Trended", "Log.train","Expon.trended"),col=c("blue","black","red","green"), lty=1:1,text.col = "black",horiz = F)


#1.2.6 Fit the ARIMA(2,1,2) model
fit.model <- arima (detrended.train, order=c(2,1,2))

#1.2.7 Compare estimated ARIMA parameters to true ones
fit.model

#1.3 Forecasting the simulated series
#-------------------------------------

#1.3.1 Forecast and plot
fit2=ar.ols(detrended.train,order=2,demean = FALSE,intercept = TRUE)
my_fore=predict(fit2, n.ahead=200)
U = my_fore$pred+my_fore$se 
L = my_fore$pred-my_fore$se
xx = c(time(U), rev(time(U))) 
yy = c(L, rev(U))
ts.plot(detrended.train,my_fore$pred, col=1:2)
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))

#1.3.2 Add trend and plot
fore_trended = my_fore$pred + fitted.trend$coefficients[1] + fitted.trend$coefficients[2]*time(my_fore$pred)
Ut = fore_trended+my_fore$se
Lt = fore_trended-my_fore$se
xt = c(time(Ut), rev(time(Ut))) 
yt = c(Lt, rev(Ut))
ts.plot(log.train.ts,fore_trended, col=1:2)
polygon(xt, yt, border = 8, col = gray(.6, alpha = .2))

#1.3.3 Apply exp and plot
fore_exp = exp(fore_trended)
se_exp = exp(my_fore$se)
Ue = fore_exp+se_exp
Le = fore_exp-se_exp
xe = c(time(Ue), rev(time(Ue))) 
ye = c(Le, rev(Ue))
ts.plot(train.ts, fore_exp, ylim=c(0,3.5),col=1:2, main="Exp Forecast(red) and Training Set(black)")
polygon(xe, ye, border = 8, col = gray(.6, alpha = .2))
ts.plot(test.ts, fore_exp, ylim=c(0,4),col=1:2, main="Exp Forecast(red) and Testing Set(black)")
polygon(xe, ye, border = 8, col = gray(.6, alpha = .2))


#1.4 Evaluation of the results
#--------------------------------

#1.4.1 Plot the forecast and the real training set in the same axes 
#(plots included in 1.3)

#1.4.2 acf and ccf of training set and forecast
acf(train.ts,main="ACF of Exp Training Set")
acf(fore_exp,main="ACF of Exp Forecast")
ccf(ts(train.ts),ts(fore_exp),main="CCF between Training Set and Forecast")

#1.4.3 acf of residual
resi=test.ts-fore_exp
plot(resi,type="p",ylab="Residual")
lines(lowess(resi),col="red")
acf(resi,main="acf of residual")

#1.4.4 forecast error
err=sum(resi^2)/200
err
mean(resi)
var(resi)


# Part 2. Real data
#===================

#2.1. Data sourcing and visualization
#-------------------------------------

#2.1.1. Read the dataset 
install.packages("readxl")
# Loading
library("readxl")

# Prompt to load the csv file
#P.S. Use only with the attached excel file named 'RWTCm_DS2Assign' because it has a prepared sheet #3 
my_data <- read_excel(file.choose(),3)
my_data_ts <-ts(my_data$Price,start =c(1986,1), frequency = 12)


#2.1.2 Plot the series and its acf
plot.ts(my_data_ts,main="WTI Crude Oil Series", ylab="Price", xlab="Years")
acf(my_data_ts,main ="ACF of the WTI Crude Oil Series")


#2.2 Exploratory analysis
#---------------------------------

#2.2.1 Divide the series into a training set (up to 2019 inclusively) and testing set (all the rest)
train_set <- subset(my_data,Date<"2020-01-01")
test_set <- subset(my_data,Date>="2020-01-01")

#Create Train and Test time series
Train_TS <- ts(train_set$Price,start =c(1986,1), frequency = 12)
Test_TS <- ts(test_set$Price,start =c(2020,1), frequency = 12)

plot.ts(Train_TS, main="WTI Crude Oil Training Set Series", ylab="Price", xlab="Years")
plot.ts(Test_TS,main="WTI Crude Oil Testing Set Series", ylab="Price", xlab="Years")


#2.2.2 Logarithm the series
Log_Train_TS <- log(Train_TS)
plot.ts(Log_Train_TS, main="Logged WTI Crude Oil Training Set Series", ylab="Price", xlab="Years")

#2.2.3 Estimate the linear trend by the least squares procedure
fit.log = lm(Log_Train_TS~time(Log_Train_TS))
coef(fit.log)
estimatedTrend =fitted(fit.log)

# 2.2.4 Detrend the series
detrended = Log_Train_TS-estimatedTrend
plot(Log_Train_TS,ylim = c(-1,5), ylab="values", xlab="Years",main="Logged WTI Crude Oil Training Set vs. Detrended Series")
lines(detrended,col="blue")
lines(fit.log$coefficients[1]+fit.log$coefficients[2]*time(Log_Train_TS),col="red",type="l")
legend("topleft", legend=c("Log_Train_TS","Tend line", "Detrended"),col=c("black","red", "blue"), lty=1:1,text.col = "black",horiz = T)

#2.2.5 Fit ARIMA(2, 1, 2) to the training data 
arima_detrendded = arima(detrended , order = c(2,1,2))
coef(arima_detrendded)

#2.3 Forecasting in the order, inverse to the exploratory analysis steps
#-------------------------------------------------------------------------------

#2.3.1 Forecast your ARIMA model for the period of training set
forecast_arima =predict(arima_detrendded, n.ahead = length(Test_TS))


#2.3.2 Extrapolate your linear trend to this period and add it to your ARIMA forecast
trendEq =fit.log$coefficients[1]+fit.log$coefficients[2]*time(forecast_arima$pred)
trended_forecast_arima = forecast_arima$pred + trendEq
plot(trended_forecast_arima)

#2.3.3 Exponentiate the result 
exp_trendedForecast = exp(trended_forecast_arima)
plot(exp_trendedForecast)

#2.4 Evaluation of the forecast quality
#-------------------------------------------------------------------------------

#2.4.1 Plot the forecast and real training set in the same axes
#Zoomed In plot
past_10_sample=ts(tail(Train_TS,10),frequency = 12,start=c(2019,3))
u_CI=exp_trendedForecast+exp(forecast_arima$se)
l_CI=exp_trendedForecast-exp(forecast_arima$se)

plot(Test_TS,xlim=c(min(time(past_10_sample)),max(time(Test_TS))),ylim=c(min(Test_TS,exp_trendedForecast,past_10_sample),max(Test_TS,exp_trendedForecast,past_10_sample)), ylab="values", main ="History and Trended Forecast - Zoom In")
lines(exp_trendedForecast,col="red")
lines(past_10_sample,col="blue")
lines(u_CI,lty=2,lwd=2,col="springgreen4")
lines(l_CI,lty=2,lwd=2,col="springgreen4")
abline(v=min(time(Test_TS)),col="green")
legend("bottomleft", legend=c("Test TS", "Trended Forecast","End Part of Train TS","Confidence Interval"),col=c("black", "red","blue","springgreen4"), lty=c(rep(1,3),2),text.col = "black",horiz = F)

#Zoomed out plot
past_10_sample=ts(tail(Train_TS,500),frequency = 12,start=c(1986,3))
u_CI=exp_trendedForecast+exp(forecast_arima$se)
l_CI=exp_trendedForecast-exp(forecast_arima$se)

plot(Test_TS,xlim=c(min(time(past_10_sample)),max(time(Test_TS))),ylim=c(min(Test_TS,exp_trendedForecast,past_10_sample),max(Test_TS,exp_trendedForecast,past_10_sample)), ylab="values", main ="History and Trended Forecast - Zoom Out")
lines(exp_trendedForecast,col="red")
lines(past_10_sample,col="blue")
lines(u_CI,lty=2,lwd=2,col="springgreen4")
lines(l_CI,lty=2,lwd=2,col="springgreen4")
abline(v=min(time(Test_TS)),col="green")
legend("bottomleft", legend=c("Test TS", "Trended Forecast","End Part of Train TS","Confidence Interval"),col=c("black", "red","blue","springgreen4"), lty=c(rep(1,3),2),text.col = "black",horiz = F)


#2.4.2  Plot acf of the training set and its prediction, and ccf between them
acf(Train_TS, main="ACF of Train TS")
acf(ts(exp_trendedForecast), main="ACF of Forecast")
ccf(ts(Train_TS),ts(exp_trendedForecast),na.action = na.exclude, type="cor",ylab = "cross-correlation",plot = TRUE,main ="CCF of the training set and its prediction")


#2.4.3 Plot the residuals and their acf
resid_part2 = Test_TS - exp_trendedForecast
hist(resid_part2, main="Histogram of Residuals")
plot(resid_part2,type="p",ylab="values", main="Residuals")
lines(lowess(resid_part2),col="red")
acf(resid_part2,main = "ACF of Forecast Residual")

#2.4.4 Estimate the forecast error
mse = sum(resid_part2^2) / length(resid_part2)
mse
var(resid_part2)



