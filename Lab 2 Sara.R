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

p100 <- plot_eacf(AR1_100, 'AR(1)')
p200 <- plot_eacf(AR1_200, 'AR(1)')
p1000 <- plot_eacf(AR1_1000, 'AR(1)')
p100 + p200 + p1000

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

p100 <- plot_eacf(MA1_100, 'MA(1)')
p200 <- plot_eacf(MA1_200, 'MA(1)')
p1000 <- plot_eacf(MA1_1000, 'MA(1)')
p100 + p200 + p1000

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

p100 <- plot_eacf(ARMA11_100, 'ARMA(1,1)')
p200 <- plot_eacf(ARMA11_200, 'ARMA(1,1)')
p1000 <- plot_eacf(ARMA11_1000, 'ARMA(1,1)')
p100 + p200 + p1000
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
# ML
data_ML <- data.frame(Method = rep('ML', 3),
                        n = as.factor(c(est_AR1_100_ML$nobs, est_AR1_200_ML$nobs, est_AR1_1000_ML$nobs)), 
                        coef = c(est_AR1_100_ML$coef[1], est_AR1_200_ML$coef[1], est_AR1_1000_ML$coef[1]),
                        se = c(sqrt(est_AR1_100_ML$var.coef[1]), 
                               sqrt(est_AR1_200_ML$var.coef[1]), 
                               sqrt(est_AR1_1000_ML$var.coef[1])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = n, y = coef)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.2) +
  geom_text(data = data_both, aes(label = round(coef, 3)), nudge_x = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.7, linetype = 'dashed', col = '#666666', alpha = 0.6) +
  labs(title = 'MK- och ML-skattningar för AR(1)') +
  facet_wrap(~Method)


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
# ML
data_ML <- data.frame(Method = rep('ML', 3),
                        n = as.factor(c(est_MA1_100_ML$nobs, est_MA1_200_ML$nobs, est_MA1_1000_ML$nobs)), 
                        coef = c(est_MA1_100_ML$coef[1], est_MA1_200_ML$coef[1], est_MA1_1000_ML$coef[1]),
                        se = c(sqrt(est_MA1_100_ML$var.coef[1]), 
                               sqrt(est_MA1_200_ML$var.coef[1]), 
                               sqrt(est_MA1_1000_ML$var.coef[1])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = n, y = coef)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.2) +
  geom_text(data = data_both, aes(label = round(coef, 3)), nudge_x = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.7, linetype = 'dashed', col = '#666666', alpha = 0.6) +
  labs(title = 'MK- och ML-skattningar för MA(1)') +
  facet_wrap(~Method)


# ARMA(1,1)
est_ARMA11_100_MK <- arima(ARMA11_100, method = 'CSS', order = c(1,0,1))
est_ARMA11_200_MK <- arima(ARMA11_200, method = 'CSS', order = c(1,0,1))
est_ARMA11_1000_MK <- arima(ARMA11_1000, method = 'CSS', order = c(1,0,1))
est_ARMA11_100_ML <- arima(ARMA11_100, method = 'ML', order = c(1,0,1))
est_ARMA11_200_ML <- arima(ARMA11_200, method = 'ML', order = c(1,0,1))
est_ARMA11_1000_ML <- arima(ARMA11_1000, method = 'ML', order = c(1,0,1))

# Insert into dataframe
# MK på AR-koefficienten
data_MK_ar <- data.frame(Method = rep('MK', 3),
                         Coefficient = rep('Phi', 3),
                        n = as.factor(c(est_ARMA11_100_MK$nobs, est_ARMA11_200_MK$nobs, est_ARMA11_1000_MK$nobs)), 
                        coef = c(est_ARMA11_100_MK$coef[1], est_ARMA11_200_MK$coef[1], est_ARMA11_1000_MK$coef[1]),
                        se = c(sqrt(est_ARMA11_100_MK$var.coef[1]), 
                               sqrt(est_ARMA11_200_MK$var.coef[1]), 
                               sqrt(est_ARMA11_1000_MK$var.coef[1])))
# MK på MA-koefficienten
data_MK_ma <- data.frame(Method = rep('MK', 3),
                         Coefficient = rep('Theta', 3),
                        n = as.factor(c(est_ARMA11_100_MK$nobs, est_ARMA11_200_MK$nobs, est_ARMA11_1000_MK$nobs)), 
                        coef = c(est_ARMA11_100_MK$coef[2], est_ARMA11_200_MK$coef[2], est_ARMA11_1000_MK$coef[2]),
                        se = c(sqrt(est_ARMA11_100_MK$var.coef[5]), 
                               sqrt(est_ARMA11_200_MK$var.coef[5]), 
                               sqrt(est_ARMA11_1000_MK$var.coef[5])))

# ML på AR-koefficienten
data_ML_ar <- data.frame(Method = rep('ML', 3),
                         Coefficient = rep('Phi', 3),
                        n = as.factor(c(est_ARMA11_100_ML$nobs, est_ARMA11_200_ML$nobs, est_ARMA11_1000_ML$nobs)), 
                        coef = c(est_ARMA11_100_ML$coef[1], est_ARMA11_200_ML$coef[1], est_ARMA11_1000_ML$coef[1]),
                        se = c(sqrt(est_ARMA11_100_ML$var.coef[1]), 
                               sqrt(est_ARMA11_200_ML$var.coef[1]), 
                               sqrt(est_ARMA11_1000_ML$var.coef[1])))

# ML på MA-koefficienten
data_ML_ma <- data.frame(Method = rep('ML', 3),
                         Coefficient = rep('Theta', 3),
                        n = as.factor(c(est_ARMA11_100_MK$nobs, est_ARMA11_200_MK$nobs, est_ARMA11_1000_MK$nobs)), 
                        coef = c(est_ARMA11_100_MK$coef[2], est_ARMA11_200_MK$coef[2], est_ARMA11_1000_MK$coef[2]),
                        se = c(sqrt(est_ARMA11_100_MK$var.coef[5]), 
                               sqrt(est_ARMA11_200_MK$var.coef[5]), 
                               sqrt(est_ARMA11_1000_MK$var.coef[5])))


# Plot
data_both <- rbind(data_MK_ar, data_ML_ar, data_MK_ma, data_ML_ma)

ggplot(data_both, aes(x = n, y = coef)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), width = 0.2) +
  geom_text(data = data_both, aes(label = round(coef, 3)), nudge_x = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.7, linetype = 'dashed', col = '#666666', alpha = 0.6) +
  labs(title = 'MK- och ML-skattningar för ARMA(1,1)') +
  facet_wrap(~ Method + Coefficient)


# Modell högre ordning: ARMA(2,2)

# Simulering
par(mfrow=c(3,1))
ARMA22_100 <- arima.sim(model = list(ma=c(0.4, -0.5), ar=c(0.7, -0.2)), n = 100)
plot(ARMA22_100)
ARMA22_200 <- arima.sim(model = list(ma=c(0.4, -0.5), ar=c(0.7, -0.2)), n = 200)
plot(ARMA22_200)
ARMA22_1000 <- arima.sim(model = list(ma=c(0.4, -0.5), ar=c(0.7, -0.2)), n = 1000)
plot(ARMA22_1000)

# ACF och PACF
par(mfrow=c(3,2))
acf(ARMA22_100)
pacf(ARMA22_100)
acf(ARMA22_200)
pacf(ARMA22_200)
acf(ARMA22_1000)
pacf(ARMA22_1000)

p100 <- plot_eacf(ARMA22_100, 'ARMA(2,2)')
p200 <- plot_eacf(ARMA22_200, 'ARMA(2,2)')
p1000 <- plot_eacf(ARMA22_1000, 'ARMA(2,2)')
p100 + p200 + p1000

eacf(ARMA22_1000)

# Skatta med MK och ML
ARMA22_1000_MK <- arima(ARMA22_1000, method = 'CSS', order = c(0,0,4))
ARMA22_1000_ML <- arima(ARMA22_1000, method = 'ML', order = c(0,0,4))

# Insert into dataframe
# MK
data_MK <- data.frame(Method = rep('MK', 4),
                      theta_order = rep('Theta', 4),
                        theta_nr = as.factor(c(1, 2, 3, 4)), 
                        Estimate = c(ARMA22_1000_MK$coef[1],
                                 ARMA22_1000_MK$coef[2],
                                 ARMA22_1000_MK$coef[3],
                                 ARMA22_1000_MK$coef[4]),
                        se = c(sqrt(ARMA22_1000_MK$var.coef[1]), 
                               sqrt(ARMA22_1000_MK$var.coef[7]), 
                               sqrt(ARMA22_1000_MK$var.coef[13]),
                               sqrt(ARMA22_1000_MK$var.coef[19])))
                      
# MK
data_ML <- data.frame(Method = rep('ML', 4),
                      theta_order = rep('Theta', 4),
                      theta_nr = as.factor(c(1, 2, 3, 4)), 
                      Estimate = c(ARMA22_1000_ML$coef[1],
                               ARMA22_1000_ML$coef[2],
                               ARMA22_1000_ML$coef[3],
                               ARMA22_1000_ML$coef[4]),
                      se = c(sqrt(ARMA22_1000_ML$var.coef[1]), 
                             sqrt(ARMA22_1000_ML$var.coef[7]), 
                             sqrt(ARMA22_1000_ML$var.coef[13]),
                             sqrt(ARMA22_1000_ML$var.coef[19])))

# Plot
data_both <- rbind(data_MK, data_ML)

ggplot(data_both, aes(x = theta_order, y = Estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - se * 1.96, ymax = Estimate + se * 1.96), width = 0.05) +
  geom_text(data = data_both, aes(label = round(Estimate, 3)), nudge_x = 0.3) +
  scale_y_continuous(limits = c(-1, 1.5)) +
  labs(title = 'MK- och ML-skattningar för ARMA(2,2)') +
  facet_grid(Method~theta_nr)

