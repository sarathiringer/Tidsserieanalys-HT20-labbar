# 2 Trendanalys

### LIB ###
library(TSA)
###--------------

# Data import
data(wages)
data(beersales)

###--------------
# 1. Linjär trendmodel
###--------------

### Wages
###--------------
model1 <- lm(wages ~ time(wages) + I(time(wages)^2))
summary(model1)

plot(wages)
plot(residuals(model1))

acf(wages)
acf(residuals(model1))

qqnorm(wages)
qqnorm(residuals(model1))

### Beersales
###--------------
plot(beersales)

model2 <- lm(beersales ~ time(beersales) + I(time(beersales)^2))
summary(model2)

plot(beersales)
plot(residuals(model2))

acf(beersales)
acf(residuals(model2))

qqnorm(beersales)
qqnorm(residuals(model2))

###--------------
# Periodisk trend
###--------------
# Beersales
har. <- harmonic(beersales, 1)
model4 <- lm(beersales ~ har.)
summary(model4)

plot(ts(fitted(model4), freq = 12, start=c(1975,1)),
     ylab = "Beersales", type = "l",
     ylim = range(c(fitted(model4), beersales))); points(beersales)

plot(beersales, type="o")
plot(residuals(model4))

acf(beersales)
acf(residuals(model4))

model5 <- lm(beersales~ har. + time(beersales) + I(time(beersales)^2))
summary(model5)
acf(residuals(model5))

###--------------
# 3 Sumulering av ARMA(p,q)-processer
###--------------

# Beta1 = 0.1 , Beta2 = -0.1
AR1_pos <- arima.sim(model = list(ar=c(0.5)), n= 400, sd = 1)
AR1_neg <- arima.sim(model = list(ar=c(-0.5)), n= 400, sd = 1)

plot(AR1_pos)
plot(AR1_neg)

MA1_pos <- arima.sim(model = list(ma=-c(0.5)), n= 400, sd = 1)
MA1_neg <- arima.sim(model = list(ma=-c(-0.5)), n= 400, sd = 1)

plot(MA1_pos)
plot(MA1_neg)

acf(AR1_pos)
acf(AR1_neg)

acf(MA1_pos)
acf(MA1_neg)



# Diff wages

delta <- diff(wages, lag = 1)
acf(delta)

delta2 <- diff(wages, lag = 1, differences = 2)
acf(delta2)

delta3 <- diff(wages, lag = 1, differences = 3)
acf(delta3)

model6 <- lm(delta ~ time(delta) + I(time(delta)^2))
summary(model6)

acf(delta)
acf(residuals(model6))

