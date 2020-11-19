ata(wages)
data(beersales)
wages

##WAGES
##Linear trendmodel
model1w=lm(wages~time(wages) +I(time(wages)^2))
summary(model1w)

par(mfrow=c(2,2))
acf(wages)
acf(rstudent(model1w))

qqnorm(wages)
qqnorm(rstudent(model1w))

##Seasonal Trends (Cyclical)
##january as intercept
par(mfrow=c(1,1))
month.<-season(wages)
model2w<-lm(wages~month. -1)
summary(model2w)

##no intercept
model2.1w<-lm(wages~month.)
summary(model2.1w)
##residuals vs time
plot(y=rstudent(model2.1w), x=as.vector(time(wages)), xlab="time", ylab="standardized residuals", type = "l")
points(y=rstudent(model2.1w), x=as.vector(time(wages)), pch=as.vector(season(wages)))

##standard.res.vs fitted values
plot(y=rstudent(model2.1w), x=as.vector(fitted(model2.1w)), xlab="fitted trend values", ylab="standardized residuals", type = "n")

points(y=rstudent(model2.1w), x=as.vector(fitted(model2.1w)), pch=as.vector(season(wages)))
hist(rstudent(model2.1w), xlab="stand res")    
qqnorm(rstudent(model2.1w))  

acf(rstudent(model2.1w))    
acf(wages)    

##Seasonal Trends (Cyclical)
##Beersales
##january as intercept
month.bs<-season(beersales)
model2.1bs<-lm(beersales~month.bs -1)
summary(model2.1bs)

##no intercept
model2bs<-lm(beersales~month.bs)
summary(model2bs)
##residuals vs time
plot(y=rstudent(model2bs), x=as.vector(time(beersales)), xlab="time", ylab="standardized residuals", type = "l")
points(y=rstudent(model2bs), x=as.vector(time(beersales)), pch=as.vector(season(beersales)))

##standard.res.vs fitted values
plot(y=rstudent(model2bs), x=as.vector(fitted(model2bs)), xlab="fitted trend values", ylab="standardized residuals", type = "n")
points(y=rstudent(model2bs), x=as.vector(fitted(model2bs)), pch=as.vector(season(beersales)))

hist(rstudent(model2bs), xlab="stand res")    
qqnorm(rstudent(model2bs))  

acf(rstudent(model2bs))    
acf(beersales)  


har.<-harmonic(beersales, 1)

model4<-lm(beersales~har.+ month.bs)
acf(rstudent(model4))


har2<-harmonic(wages, 1)
model2w<-lm(wages~month. +har2 -1)
summary(model2w)
acf(model2w)
##no intercept
model2.1w<-lm(wages~ har2+month.)
summary(model2.1w)



##uppgift 3
ar1<-arima.sim(model=list(ar=c(0.6)), n=40000, sd=1)
ar12<-arima.sim(model=list(ar=c(-0.6)), n=40000, sd=1)
plot(ar1)
plot(ar12)
acf(ar1)
acf(ar12)
mean(ar1)
mean(ar12)

ma1<-arima.sim(model=list(ma=-c(0.9)), n=400, sd=1)
ma12<-arima.sim(model=list(ma=-c(-0.9)), n=400, sd=1)

plot(ma1)
plot(ma12)

acf(ma1)
acf(ma12)

pacf(ar1)
pacf(ma1)