library(astsa)
library(tseries)
library(lmtest)
library(TSA)
library(fBasics)
library(fUnitRoots)
library(lubridate)
library(dplyr)


setwd('C:/Users/skyli/Dropbox/Spring 2019/AMV')
data = read.csv('uspollution_pollution_us_2000_2016.csv')
head(data)

dataSD = data[data$City == "San Diego",]
head(dataSD)

df <- data.frame(date = dataSD$Date.Local,
                 year = year(dataSD$Date.Local),
                 month = month(dataSD$Date.Local))

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

par(mfrow=c(2,2))
plot(df_no2$No2mean,main='No2', xlab='', ylab='')
plot(df_co$Comean,main='Co',xlab='',ylab='')
plot(df_So2$So2mean,main='So2',xlab='',ylab='')
plot(df_o3$o3mean,main='O3',xlab='',ylab='')


pairs(cbind(No2 = df_no2$No2mean,CO = df_co$Comean, So2 = df_So2$So2mean, O3 = df_o3$o3mean ))  #a matrix of scatterplots is produced.

n=length(df_no2$No2mean)
n
data=cbind(df_no2[3], df_co[3], df_So2[3], df_o3[3])
head(data)
nrow(data)
train=as.data.frame(data[1:66,])
nrow(train)
head(train)
new.data=data[67:n,2:4]
head(new.data)
nrow(new.data)
lm=lm(No2mean~Comean + So2mean + o3mean,data=train)
summary(lm)
e=lm$residuals

pdf('res.pdf',width=8,height=4)
plot(e,type='l')
dev.off()

pdf('res_acf.pdf',width=8,height=4)
par(mfrow=c(1,2))
acf(e) #MA1
pacf(e) #AR1
dev.off()

eacf(e) #AR0MA1

model1=arima(e,order=c(1,0,0))
model1
coeftest(model1)

acf(model1$residuals)
pacf(model1$residuals)
Box.test(model1$residuals,lag=12,type="Ljung")


model2=arima(e,order=c(1,0,1))
model2
coeftest(model2)

acf(model2$residuals)
pacf(model2$residuals)
Box.test(model2$residuals,lag=12,type="Ljung")

model3=arima(e,order=c(0,0,1))
model3
coeftest(model3)

acf(model3$residuals)
pacf(model3$residuals)
Box.test(model3$residuals,lag=12,type="Ljung")

print(c(model1$aic, model2$aic, model3$aic))
#choosing model1
##Prediction
pp1=predict(lm,as.data.frame(new.data),interval="prediction")		#prediction with just regression model
pp2=predict(model3,8)		#prediction for e with time series model



nn=length(e)
nn
nt=8	#forecast horizon
nb=30	#number of data points you want to plot
tt=(nn-nb):nn	#indexes of data points you want to plot
tt
xxx=data$No2mean[tt]		#data you want to plot
rr=range(c(xxx,pp1))-1	#find the minimum and maximum y values in your plot
head(pp1)
nrow(pp1[,1])
print(pp1[,1])
pdf('pred.pdf',width=8,height=4)
print(c(nn-nb,nn+nt))
par(mfrow=c(1,1))
plot(tt,xxx,pch=1,xlim=c(nn-nb,nn+nt),ylim=rr,main='No2 Mean',ylab='No2 Mean',xlab='Month')	
lines(tt,xxx)	#observed values
points(nn+1:132,pp1[,1],pch=2,col='red',type='o')	#predicted values
lines(nn+1:132,pp1[,2],lty=2,col='red')	#upper bound of predicted interval
lines(nn+1:132,pp1[,3],lty=2,col='red')	#lower bound of predicted interval


points(nn+1:nt,pp1[,1]+pp2$pred,pch=3,col='blue',type='o')	#predicted values
lines(nn+1:nt,pp1[,1]+pp2$pred+2*pp2$se,lty=3,col='blue')	#upper bound of predicted interval
lines(nn+1:nt,pp1[,1]+pp2$pred-2*pp2$se,lty=3,col='blue')	#lower bound of predicted interval

points(nn+0:nt,cmort[nn+0:nt],type='o')

legend.text=c("Actual value","Prediction-lm","Prediction-ts")
legend("bottomleft",legend.text,col=c("black","red","blue"),lty=rep(1,3),pch=1:3)
dev.off()

install.packages("vars")
library(vars)
x=cbind(cmort,tempr,part)
x = data
summary(VAR(x,p=1))

data=read.table("m-ibmsp2608.txt", header=T)
model=VAR(data[,2:4],p=1)
summary(model)


#model selection
VARselect(data[,2:4],lag.max=6)
model=VAR(data[,2:4],p=1)
summary(model)
#model diagnostics
serial.test(model)

model2=VAR(data[,2:4],p=2)
summary(model2)
#model diagnostics
serial.test(model2)

model5=VAR(data[,2:4],p=5)
summary(model5)
#model diagnostics
serial.test(model5)


