# Packages
library(TSA)

# Also load function for eacg plot by running code in function_plot_eacf

# Reproducability
set.seed(1)

##### AR(1)

# Simulating the data
par(mfrow=c(3,1))
AR1_100 <- arima.sim(model = list(ar=c(0.7)), n= 100)
plot(AR1_100)
AR1_200 <- arima.sim(model = list(ar=c(0.7)), n= 200)
plot(AR1_200)
AR1_1000 <- arima.sim(model = list(ar=c(0.7)), n= 1000)
plot(AR1_1000)

# Estimating model order by ACF and PACF
par(mfrow=c(3,2))
acf(AR1_100)
pacf(AR1_100)
acf(AR1_200)
pacf(AR1_200)
acf(AR1_1000)
pacf(AR1_1000)

plot_eacf(AR1_100, 'AR(1)')
plot_eacf(AR1_200, 'AR(1)')
plot_eacf(AR1_1000, 'AR(1)')

##### MA(1)

# Simulating the data
par(mfrow=c(3,1))
MA1_100 <- arima.sim(model = list(ma=c(0.7)), n= 100)
plot(MA1_100)
MA1_200 <- arima.sim(model = list(ma=c(0.7)), n= 200)
plot(MA1_200)
MA1_1000 <- arima.sim(model = list(ma=c(0.7)), n= 1000)
plot(MA1_1000)

# Estimating model order by ACF and PACF
par(mfrow=c(3,2))
acf(MA1_100)
pacf(MA1_100)
acf(MA1_200)
pacf(MA1_200)
acf(MA1_1000)
pacf(MA1_1000)

plot_eacf(MA1_100, 'MA(1)')
plot_eacf(MA1_200, 'MA(1)')
plot_eacf(MA1_1000, 'MA(1)')

##### ARMA(1,1)

# Simulating the data
par(mfrow=c(3,1))
ARMA11_100 <- arima.sim(model = list(ma=c(0.7), ar=c(0.7)), n= 100)
plot(ARMA11_100)
ARMA11_200 <- arima.sim(model = list(ma=c(0.7), ar=c(0.7)), n= 200)
plot(ARMA11_200)
ARMA11_1000 <- arima.sim(model = list(ma=c(0.7), ar=c(0.7)), n= 1000)
plot(ARMA11_1000)

# Estimating model order by ACF and PACF
par(mfrow=c(3,2))
acf(ARMA11_100)
pacf(ARMA11_100)
acf(ARMA11_200)
pacf(ARMA11_200)
acf(ARMA11_1000)
pacf(ARMA11_1000)

plot_eacf(ARMA11_100, 'ARMA(1,1)')
plot_eacf(ARMA11_200, 'ARMA(1,1)')
plot_eacf(ARMA11_1000, 'ARMA(1,1)')
