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

# Skatta med MK och ML

# AR(1)
est_AR1_100_MK <- arima(AR1_100, method = 'CSS', order = c(1,0,0))
est_AR1_200_MK <- arima(AR1_200, method = 'CSS', order = c(1,0,0))
est_AR1_1000_MK <- arima(AR1_1000, method = 'CSS', order = c(1,0,0))
est_AR1_100_ML <- arima(AR1_100, method = 'ML', order = c(1,0,0))
est_AR1_200_ML <- arima(AR1_200, method = 'ML', order = c(1,0,0))
est_AR1_1000_ML <- arima(AR1_1000, method = 'ML', order = c(1,0,0))

# Insert into dataframe
# MK
data_MK <- data.frame(Method = rep('MK', 3),
                        n = as.factor(c(est_AR1_100_MK$nobs, est_AR1_200_MK$nobs, est_AR1_1000_MK$nobs)), 
                        coef = c(est_AR1_100_MK$coef[1], est_AR1_200_MK$coef[1], est_AR1_1000_MK$coef[1]),
                        se = c(sqrt(est_AR1_100_MK$var.coef[1]), 
                               sqrt(est_AR1_200_MK$var.coef[1]), 
                               sqrt(est_AR1_1000_MK$var.coef[1])))
# MK
data_ML <- data.frame(Method = rep('ML', 3),
                        n = as.factor(c(est_AR1_100_ML$nobs, est_AR1_200_ML$nobs, est_AR1_1000_ML$nobs)), 
                        coef = c(est_AR1_100_ML$coef[1], est_AR1_200_ML$coef[1], est_AR1_1000_ML$coef[1]),
                        se = c(sqrt(est_AR1_100_ML$var.coef[1]), 
                               sqrt(est_AR1_200_ML$var.coef[1]), 
                               sqrt(est_AR1_1000_ML$var.coef[1])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = n, y = coef, col = Method)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.1, alpha = 0.6) +
  geom_text(data = data_MK, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = 0.05, show.legend = FALSE) +
  geom_text(data = data_ML, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = -0.05, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = 'MK- och ML-skattningar för AR(1)')


# MA(1)
est_MA1_100_MK <- arima(MA1_100, method = 'CSS', order = c(0,0,1))
est_MA1_200_MK <- arima(MA1_200, method = 'CSS', order = c(0,0,1))
est_MA1_1000_MK <- arima(MA1_1000, method = 'CSS', order = c(0,0,1))
est_MA1_100_ML <- arima(MA1_100, method = 'ML', order = c(0,0,1))
est_MA1_200_ML <- arima(MA1_200, method = 'ML', order = c(0,0,1))
est_MA1_1000_ML <- arima(MA1_1000, method = 'ML', order = c(0,0,1))

# Insert into dataframe
# MK
data_MK <- data.frame(Method = rep('MK', 3),
                        n = as.factor(c(est_MA1_100_MK$nobs, est_MA1_200_MK$nobs, est_MA1_1000_MK$nobs)), 
                        coef = c(est_MA1_100_MK$coef[1], est_MA1_200_MK$coef[1], est_MA1_1000_MK$coef[1]),
                        se = c(sqrt(est_MA1_100_MK$var.coef[1]), 
                               sqrt(est_MA1_200_MK$var.coef[1]), 
                               sqrt(est_MA1_1000_MK$var.coef[1])))
# MK
data_ML <- data.frame(Method = rep('ML', 3),
                        n = as.factor(c(est_MA1_100_ML$nobs, est_MA1_200_ML$nobs, est_MA1_1000_ML$nobs)), 
                        coef = c(est_MA1_100_ML$coef[1], est_MA1_200_ML$coef[1], est_MA1_1000_ML$coef[1]),
                        se = c(sqrt(est_MA1_100_ML$var.coef[1]), 
                               sqrt(est_MA1_200_ML$var.coef[1]), 
                               sqrt(est_MA1_1000_ML$var.coef[1])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = n, y = coef, col = Method)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.2, alpha = 0.6) +
  geom_text(data = data_MK, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = 0.05, show.legend = FALSE) +
  geom_text(data = data_ML, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = -0.05, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = 'MK- och ML-skattningar för MA(1)')


# ARMA(1,1)
est_ARMA11_100_MK <- arima(ARMA11_100, method = 'CSS', order = c(1,0,1))
est_ARMA11_200_MK <- arima(ARMA11_200, method = 'CSS', order = c(1,0,1))
est_ARMA11_1000_MK <- arima(ARMA11_1000, method = 'CSS', order = c(1,0,1))
est_ARMA11_100_ML <- arima(ARMA11_100, method = 'ML', order = c(1,0,1))
est_ARMA11_200_ML <- arima(ARMA11_200, method = 'ML', order = c(1,0,1))
est_ARMA11_1000_ML <- arima(ARMA11_1000, method = 'ML', order = c(1,0,1))

# Insert into dataframe
# MK
data_MK <- data.frame(Method = rep('MK', 3),
                        n = as.factor(c(est_ARMA11_100_MK$nobs, est_ARMA11_200_MK$nobs, est_ARMA11_1000_MK$nobs)), 
                        coef = c(est_ARMA11_100_MK$coef[1], est_ARMA11_200_MK$coef[1], est_ARMA11_1000_MK$coef[1]),
                        se = c(sqrt(est_ARMA11_100_MK$var.coef[1]), 
                               sqrt(est_ARMA11_200_MK$var.coef[1]), 
                               sqrt(est_ARMA11_1000_MK$var.coef[1])))
# MK
data_ML <- data.frame(Method = rep('ML', 3),
                        n = as.factor(c(est_ARMA11_100_ML$nobs, est_ARMA11_200_ML$nobs, est_ARMA11_1000_ML$nobs)), 
                        coef = c(est_ARMA11_100_ML$coef[1], est_ARMA11_200_ML$coef[1], est_ARMA11_1000_ML$coef[1]),
                        se = c(sqrt(est_ARMA11_100_ML$var.coef[1]), 
                               sqrt(est_ARMA11_200_ML$var.coef[1]), 
                               sqrt(est_ARMA11_1000_ML$var.coef[1])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = n, y = coef, col = Method)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.2, alpha = 0.6) +
  geom_text(data = data_MK, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = 0.05, show.legend = FALSE) +
  geom_text(data = data_ML, aes(label = round(coef, 3)), nudge_x = 0.2, nudge_y = -0.05, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = 'MK- och ML-skattningar för ARMA(1,1)')
       