library(TSA)

# Load data
data(wages)
data(beersales)
data("winnebago")

plot(wages, type = 'o')
plot(winnebago)
plot(beersales, type = 'o')

mean(wages)

mean(beersales)
     
time(wages)

# Linjär modell
lm_wages <- lm(wages ~ time(wages) + I(time(wages)^2))
summary(lm_wages)
Xt <- residuals(lm_wages)
par(mfrow=c(2,2))
acf(wages)
acf(Xt)
qqnorm(wages)
qqnorm(Xt)


pacf()

# Säsongsmodell


# Periodisk modell
har <- harmonic(wages, 1)
har_mod_wages <- lm(wages ~ har)
summary(har_mod_wages)
Xt <- residuals(har_mod_wages)
plot(Xt)
plot(wages, type = 'o')
abline(coef(har_mod_wages)[[1]], coef(har_mod_wages)[[2]])
acf(Xt)
acf(wages)


# Simulation
par(mfrow=c(2,3))

value_ar = 0.01

sim_ar <- arima.sim(model=list(ar=c(value_ar)), n = 400, sd = 1)
plot(sim_ar, type = 'o')
acf(sim_ar)
pacf(sim_ar)
ARMAacf(ar=c(value_ar))

sim_ar_neg <- arima.sim(model=list(ar=c(-value_ar)), n = 400, sd = 1)
plot(sim_ar_neg, type = 'o')
acf(sim_ar_neg)
pacf(sim_ar_neg)
ARMAacf(ar=c(-value_ar))

value_ma = 0.9

sim_ma <- arima.sim(model=list(ar=-c(value_ma)), n = 400, sd = 1)
plot(sim_ma, type = 'o')
acf(sim_ma)
pacf(sim_ma)
ARMAacf(ma=-c(value_ma))

sim_ma_neg <- arima.sim(model=list(ar=-c(-value_ma)), n = 400, sd = 1)
plot(sim_ma_neg, type = 'o')
acf(sim_ma_neg)
pacf(sim_ma_neg)
ARMAacf(ma=-c(value_ma))


ccf(sim_ar, sim_ma)

# 4
# Diff wages
## LINJÄR
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
qqnorm(residuals(model6))

### COSINE
har.W <- harmonic(delta, 1)
model7 <- lm(delta ~ har.W)
summary(model7)
acf(residuals(model7))
qqnorm(residuals(model7))

## SEASON

season.W <- season(delta)
model8 <- lm(delta ~ season.W)
summary(model8)
acf(residuals(model8))
qqnorm(residuals(model8))

## COMBINATION

model9 <- lm(delta ~ har.W + time(delta) + I(time(delta)^2))
summary(model9)
acf(residuals(model9))
qqnorm(residuals(model9))
