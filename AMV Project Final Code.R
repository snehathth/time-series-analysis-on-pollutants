library(astsa)
library(tseries)
library(lmtest)
library(TSA)
library(fBasics)
library(fUnitRoots)
library(lubridate)
library(dplyr)

# Setting the Data 
setwd('C:/Users/Manav Sanghavi/Downloads')
data <- read.csv('uspollution_pollution_us_2000_2016.csv')
head(data)

#Data Preprocessing

dataSD<-data[data$City == "San Diego",]
head(dataSD)

df <- data.frame(date = dataSD$Date.Local,
                 year = year(dataSD$Date.Local),
                 month = month(dataSD$Date.Local))
names(data)
nrow(df)
summary(df)
nrow(dataSD)
summary(dataSD)

datadate <- cbind(df,dataSD)

summary(datadate)
unique(datadate$year)

#Taking year 2011 for testing 

testdatadate <- datadate[datadate$year == 2011,]
datadate <- datadate[datadate$year != 2011,]
unique(datadate$year)
unique(testdatadate$year)

#Divding the Dataset into 4 for easy analysis

df_no2 <- datadate %>%
  mutate(norm = mean(NO2.Mean)) %>%
  group_by(month,year) %>%
  dplyr::summarize(No2mean =mean(NO2.Mean)) %>%
  arrange(year, month)
head(df_no2)
df_co <- datadate %>%
  mutate(norm = mean(CO.Mean)) %>%
  group_by(month,year) %>%
  dplyr::summarize(Comean =mean(CO.Mean)) %>%
  arrange(year, month)
head(df_co)
df_So2 <- datadate %>%
  mutate(norm = mean(SO2.Mean)) %>%
  group_by(month,year) %>%
  dplyr::summarize(So2mean =mean(SO2.Mean)) %>%
  arrange(year, month)
head(df_So2)
df_o3 <- datadate %>%
  mutate(norm = mean(O3.Mean)) %>%
  group_by(month,year) %>%
  dplyr::summarize(o3mean =mean(O3.Mean)) %>%
  arrange(year, month)
head(df_o3)

#UnivaRIATE analysis on No2
#Data Exploration

#Time Series Plot of No2, to understand stationarity and Seasonality
####
ts.plot(df_no2$No2mean, main = 'No2 Mean Time Series')# we have stationarity in the data
ts.plot(df_no2$No2mean[df_no2$year == 2003], main = 'No2 Mean for 2003')
ts.plot(df_no2$No2mean[df_no2$year == 2004])
ts.plot(df_no2$No2mean[df_no2$year == 2005])

adf.test(df_no2$No2mean) #test : there is stationarity

par(mfrow = c(1,2))
acf(df_no2$No2mean,lag=60)
#In ACF there is a pattern, hence there is seasonality
# Since its outside the confidence interval, there is
#Autocorrelation in the data, data is dependent on
#each other
pacf(df_no2$No2mean,lag=60)

# Taking Seasonality Difference
diffseasonal = diff(df_no2$No2mean,12)
plot(diffseasonal,type='l',main='Differenced seasonal data')
acf(diffseasonal,main='ACF for differenced seasonal data',lag.max=60) #MA1
pacf(diffseasonal,main='PACF for differenced seasonal data', lag.max=60) #AR3


#Creating Models

#MA1


out1=arima(df_no2$No2mean,order=c(0,0,0),seasonal=list(order=c(0,1,1),period=12))
out1
acf(out1$residuals,main='ACF for differenced seasonal data',lag.max=60)
#MA7 q=7

pacf(out1$residuals,main='PACF for differenced seasonal data', lag.max=60)
#AR4  p=4
coeftest(out1)
#MA1 and intercept both are significant

eacf(out1$residuals) #ARMA (1,1)

# Trying MA7 model

out1.Ma7=arima(df_no2$No2mean,order=c(0,0,7),seasonal=list(order=c(0,1,1),period=12))
out1.Ma7
coeftest(out1.Ma7)
#we are not taking this model

# Trying Ar4 model
out1.Ar4=arima(df_no2$No2mean,order=c(4,0,0),seasonal=list(order=c(0,1,1),period=12))
out1.Ar4
coeftest(out1.Ar4)
#we are not taking this model

#Trying Arma (1,1) model
out1.arma11=arima(diffseasonal,order=c(1,0,1),seasonal=list(order=c(0,0,1),period=12))
out1.arma11
coeftest(out1.arma11)

#Ar3

out1.1=arima(df_no2$No2mean,order=c(0,0,0),seasonal=list(order=c(3,1,0),period=12))
out1.1
acf(out1.1$residuals,main='ACF for differenced seasonal data',lag.max=60)#Ma7

pacf(out1.1$residuals,main='PACF for differenced seasonal data', lag.max=60)#Ar4

coeftest(out1.1)
#ar3 is not significant

eacf(out1.1$residuals) #ARMA (2,2) and ARMA (1,4)


# Trying MA7 model

out2.1.Ma7=arima(df_no2$No2mean,order=c(0,0,7),seasonal=list(order=c(3,1,0),period=12))
out2.1.Ma7
coeftest(out2.1.Ma7)


# Trying Ar4 model
out2.1.Ar4=arima(df_no2$No2mean,order=c(4,0,0),seasonal=list(order=c(3,1,0),period=12))
out2.1.Ar4
coeftest(out2.1.Ar4)

#Trying Arma (2,2) model
out2.1.arma22=arima(diffseasonal,order=c(2,0,2),seasonal=list(order=c(3,0,0),period=12))
out2.1.arma22
acf(out2.1.arma22$residuals,main='ACF for differenced seasonal differenced data',lag.max=60)
pacf(out2.1.arma22$residuals,main='PACF for differenced seasonal differenced data', lag.max=60)
coeftest(out2.1.arma22)
eacf(out2.1.arma22$residuals)
Box.test(out2.1.arma22$residuals,lag=12,type="Ljung")



#out2.1.arma22.fixed=arima(diffseasonal,order=c(2,0,2),seasonal=list(order=c(3,0,0),period=12), fixed = c(0,NA,0,NA,NA,NA,NA,0))
#out2.1.arma22
#coeftest(out2.1.arma22)
#Trying Arma (1,4) model
out2.1.arma14=arima(diffseasonal,order=c(1,0,4),seasonal=list(order=c(3,0,0),period=12))
out2.1.arma14
coeftest(out2.1.arma14)

#out2.1.arma14.fix=arima(diffseasonal,order=c(1,0,4),seasonal=list(order=c(3,0,0),period=12), fixed = c(NA,NA,0,0,NA,NA,NA,NA,0))
#out2.1.arma14
#coeftest(out2.1.arma14)

out3.1.arma14=arima(df_no2$No2mean,order=c(1,0,4),seasonal=list(order=c(2,1,0),period=12))
out3.1.arma14
acf(out3.1.arma14$residuals,main='ACF for differenced seasonal differenced data',lag.max=60)
pacf(out3.1.arma14$residuals,main='PACF for differenced seasonal differenced data', lag.max=60)
coeftest(out3.1.arma14)
eacf(out3.1.arma14$residuals)
Box.test(out3.1.arma14$residuals,lag=12,type="Ljung")

out3.1.arma14.fixed=arima(df_no2$No2mean,order=c(1,0,4),seasonal=list(order=c(2,1,0),period=12), fixed = (c(NA,NA,0,0,NA,NA,NA)))
out3.1.arma14.fixed
acf(out3.1.arma14.fixed$residuals,main='ACF for differenced seasonal differenced data',lag.max=60)
pacf(out3.1.arma14.fixed$residuals,main='PACF for differenced seasonal differenced data', lag.max=60)
coeftest(out3.1.arma14.fixed)
eacf(out3.1.arma14.fixed$residuals)
Box.test(out3.1.arma14.fixed$residuals,lag=12,type="Ljung")


#Checking for Polyroots
polyroot(c(1,-out3.1.arma14.fixed$coef[0:1]))
abs(polyroot(c(1,-out3.1.arma14.fixed$coef[0:1])))
abs(polyroot(c(1,-out3.1.arma14.fixed$coef[1:5])))






# Testing the Best Model AIC and Rolling Forecasting


print(c(out2.1.Ar4$aic, out1.arma11$aic, out2.1.arma14$aic, out3.1.arma14$aic, out3.1.arma14.fixed$aic))

source("rolling.forecast.R")
par(mfrow=c(1,1))
print(rolling.forecast(diffseasonal,12,40,c(4,0,0),seasonal=list(order=c(3,0,0))))
print(rolling.forecast(diffseasonal,12,40,c(1,0,1),seasonal=list(order=c(0,0,1))))
print(rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(3,0,0))))
print(rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(2,0,0))))
print(rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(2,0,0)), fixed = c(NA,NA,0,0,NA,NA,NA,0)))

error1 = rolling.forecast(diffseasonal,12,40,c(4,0,0),seasonal=list(order=c(3,0,0)))
error2 = rolling.forecast(diffseasonal,12,40,c(1,0,1),seasonal=list(order=c(0,0,1)))
error3 = rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(3,0,0)))
error4 = rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(2,0,0)))
error5 = rolling.forecast(diffseasonal,12,40,c(1,0,4),seasonal=list(order=c(2,0,0)), fixed = c(NA,NA,0,0,NA,NA,NA,0))
error = c(error1,error2,error3,error4,error5)
plot(error1, type='l',ylim=c(min(error), max(error)), main='rolling forcasting',xlab='Forecasting horizon',
     ylab ='error')
lines(error2,col=2)
lines(error3,col=3)
lines(error4,col=4)
lines(error5, col=5)

legend.text=c("ARMA(4,0,0)(3,0,0)12","ARMA(1,0,1)(0,0,1)12","ARMA(1,0,4)(3,0,0)12", "ARMA(1,0,4)(2,0,0)12","ARMA(1,0,4)(2,0,0)12Fixed")
legend("topright",legend.text,col=c(1,2,3,4,5),lty=rep(1,5),pch=1:5)


# Prediction
pp=predict(out3.1.arma14,12)
nn=length(df_no2$No2mean)	#length of your data
nt=12	#forecast horizon
nb=30	#number of data points you want to plot
tt=(nn-nb):nn	#indexes of data points you want to plot
xxx=df_no2$No2mean[tt]		#data you want to plot
rr=range(c(xxx,pp$pred+2*pp$se,pp$pred-2*pp$se))	#find the minimum and maximum y values in your plot
par(mfrow=c(1,1))
#pdf('exxon_pred.pdf',width=8,height=4)
plot(tt,xxx,pch=3,xlim=c(nn-nb,nn+nt),ylim=rr,main='Prediction for 2011',ylab=' ',xlab='Time')	
lines(tt,xxx)	#observed values
points(nn+1:nt,pp$pred,pch=2,col='red',type='o')	#predicted values
lines(nn+1:nt,pp$pred+2*pp$se,lty=2,col='red')	#upper bound of predicted interval
lines(nn+1:nt,pp$pred-2*pp$se,lty=2,col='red')	#lower bound of predicted interval

#Test Data Plotting
df_no2_test <- testdatadate %>%
  mutate(norm = mean(NO2.Mean)) %>%
  group_by(month,year) %>%
  dplyr::summarize(No2mean =mean(NO2.Mean)) %>%
  arrange(year, month)

nrow(df_no2_test)

points(133:138,df_no2_test$No2mean,type='o')
legend.text=c("Actual value","Prediction")
legend("bottomleft",legend.text,col=c("black","red"),lty=rep(1,2),pch=1:2)



#Multivariate Analysis
#Scatterplot to Understand Correlation
pairs(cbind(No2 = df_no2$No2mean,CO = df_co$Comean, So2 = df_So2$So2mean, O3 = df_o3$o3mean ))  #a matrix of scatterplots is produced.

#Understanding of the 4 pollutants - Interactions

par(mfrow=c(2,2))
plot(df_no2$No2mean[df_no2$year == 2003],type = 'l',main='No2', xlab='Month', ylab='Year 2003')
plot(df_co$Comean[df_co$year == 2003],type = 'l',main='Co',xlab='Month',ylab='Year 2003')
plot(df_So2$So2mean[df_So2$year == 2003],type = 'l',main='So2',xlab='Month',ylab='Year 2003')
plot(df_o3$o3mean[df_o3$year == 2003],type = 'l',main='O3',xlab='Month',ylab='Year 2003')


n=length(df_no2$No2mean)
n
data=cbind(df_no2[3], df_co[3], df_So2[3], df_o3[3])
head(data)
nrow(data)
train=as.data.frame(data[1:124,])
nrow(train)
head(train)
new.data=data[125:n,2:4]
head(new.data)
nrow(new.data)
#Linear Regression of the model
lm=lm(No2mean~Comean + So2mean + o3mean,data=train)
summary(lm)
e=lm$residuals
length(e)
par(mfrow=c(1,1))
plot(e,type='l', main = 'LInear Model Residual Time Series Plot')

par(mfrow=c(1,2))
acf(e, lag.max = 60) #MA4 seasonality
pacf(e, lag.max = 60) #AR1
model1=arima(e,order=c(0,0,0),seasonal=list(order=c(1,0,0),period=12))
model1
coeftest(model1)


acf(model1$residuals) #MA1
pacf(model1$residuals) #AR1
Box.test(model1$residuals,lag=12,type="Ljung")
eacf(e) #ARMA43

model2=arima(e,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12))
model2
coeftest(model2)

acf(model2$residuals)
pacf(model2$residuals)
Box.test(model2$residuals,lag=12,type="Ljung")

model3=arima(e,order=c(0,0,1))
model3
coeftest(model3)

acf(model2$residuals)
pacf(model2$residuals)
Box.test(model2$residuals,lag=12,type="Ljung")

print(c(model1$aic, model2$aic, model3$aic))
#choosing model2


##Prediction
pp1=predict(lm,as.data.frame(new.data),interval="prediction")		#prediction with just regression model
pp2=predict(model2,8)		#prediction for e with time series model

nn=length(e)
nn
nt=8	#forecast horizon
nb=30	#number of data points you want to plot
tt=(nn-nb):nn	#indexes of data points you want to plot
tt
xxx=data$No2mean[tt]		#data you want to plot
rr=range(c(xxx,pp1))-1	#find the minimum and maximum y values in your plot

par(mfrow=c(1,1))
plot(tt,xxx,pch=1,xlim=c(nn-nb,nn+nt),ylim=rr,main='No2 Mean',ylab='No2 Mean',xlab='Month')	
lines(tt,xxx)	#observed values
points(nn+1:nt,pp1[,1],pch=2,col='red',type='o')	#predicted values
lines(nn+1:nt,pp1[,2],lty=2,col='red')	#upper bound of predicted interval
lines(nn+1:nt,pp1[,3],lty=2,col='red')	#lower bound of predicted interval


points(nn+1:nt,pp1[,1]+pp2$pred,pch=3,col='blue',type='o')	#predicted values
lines(nn+1:nt,pp1[,1]+pp2$pred+2*pp2$se,lty=3,col='blue')	#upper bound of predicted interval
lines(nn+1:nt,pp1[,1]+pp2$pred-2*pp2$se,lty=3,col='blue')	#lower bound of predicted interval

points(nn+0:nt,data$No2mean[nn+0:nt],type='o')

legend.text=c("Actual value","Prediction-lm","Prediction-ts")
legend("bottomleft",legend.text,col=c("black","red","blue"),lty=rep(1,3),pch=1:3)
dev.off()


#VAR MOdelling - ATTEMPT
install.packages("vars")
library(vars)
x=cbind(df_no2[3], df_co[3], df_So2[3], df_o3[3])
model=VAR(x,p=1)
summary(model)
serial.test(model)

#model selection
VARselect(data[,2:4],lag.max=6)
model=VAR(data,p=4)
summary(model)
#model diagnostics
serial.test(model)

#This is the chosen model. up one



##
ts.plot(df_co$Comean)
ts.plot(df_co$Comean[df_co$year == 2003])
ts.plot(df_co$Comean[df_co$year == 2004])
ts.plot(df_co$Comean[df_co$year == 2005])

adf.test(df_co$Comean)

##

ts.plot(df_o3$o3mean)
ts.plot(df_o3$o3mean[df_o3$year == 2003])
ts.plot(df_o3$o3mean[df_o3$year == 2004])
ts.plot(df_o3$o3mean[df_o3$year == 2005])

adf.test(df_o3$o3mean)

##

ts.plot(df_So2$So2mean)
ts.plot(df_So2$So2mean[df_So2$year == 2006])
ts.plot(df_So2$So2mean[df_So2$year == 2007])
ts.plot(df_So2$So2mean[df_So2$year == 2008])

adf.test(df_So2$So2mean)

##


