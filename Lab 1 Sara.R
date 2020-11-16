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
lin_mod_wages <- lm(wages ~ time(wages) + I(time(wages)^2))
summary(lin_mod_wages)
Xt <- residuals(lin_mod_wages)
qqnorm(Xt)
acf(Xt)
acf(wages)
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
diff()
