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


###--------------
# Mix modell
###--------------

# Beersales med cosinus och linjär trend multipel


model5 <- lm(beersales~ har. + time(beersales) + I(time(beersales)^2))
summary(model5)
acf(residuals(model5))


plot(ts(fitted(model5), freq = 12, start=c(1975, 1)),
     ylab = "Beersales", type = "l",
     ylim = range(c(fitted(model5), beersales))); points(beersales)
# Modellen mkt bättre


###--------------
# 3 Sumulering av ARMA(p,q)-processer
###--------------



### 3.2 AR(1) och MA(1)
# Koefficient val: 0.9

# AR(1)
AR1_phi0.9 <- arima.sim(model = list(ar=c(0.9)), n= 400, sd = 1)
AR1_phi_0.9 <- arima.sim(model = list(ar=c(-0.9)), n= 400, sd = 1)

plot(AR1_phi0.9)
plot(AR1_phi_0.9)
# Då koefficienten är positiv så korsar processen sällan det teoretiska väntevärdet 0
# och tenderar att stanna en längre tid över eller under 0 innan den korsar igen.
# Då koefficienten är negativ tenderar processen att centrera runt det teoretiska väntevärdet 0 och gör
# ibland större (men kortvariga) hopp i positiv eller negativ riktning


# Vad gäller autokorrelation ser vi att, med den positiva koefficienten, så är korrelationen högt positiv vid få lag och 
# minskar stadigt med antalet lag.
# Vid 15 lag så blir den inte möjlig att skilja från noll vid sig. 5%.
acf(AR1_phi0.9)
ARMAacf(ar=c(0.9), lag.max = 50)
# Detta stämmer överrens med den teoretiska autokorrelationen. Den går mot 0 med antal lag och blir inte negativ. 

# Vad gäller autokorrelation med den negativa koefficienten så minskar korrelationen med antalet lag och vid ca 20 lag så
# blir en inte möjllig att skilja från 0 vid sig. 5%
# Korrelationen vid lag 1 är starkt negativ och sedan minskar korrelationen stadigt men byter tecken vid varje lag.
acf(AR1_phi_0.9)
ARMAacf(ar=c(-0.9), lag.max = 50)
# Stämmer överrens med den teoretiska autokorrelationen.

## MA(1)
MA1_pos <- arima.sim(model = list(ma=c(0.9)), n= 400, sd = 1)
MA1_neg <- arima.sim(model = list(ma=c(-0.9)), n= 400, sd = 1)

# Då koefficienten är positiv är har första laggen relativt stor negativ korrelation (runt 0.5) men sedan går inte
# korrelationen att skilja från 0 vid sig. 5%. Teoretiskt ser vi att den första laggen ska vara ca 0.5 positiv korrelation 
# (VARFÖR HAR VI FÅTT DEN NEG??) och att korrelationen sedan blir 0 vid vidare lag. Vidare finns tendensen att nästa observation byter tecken
# så som vid AR(-0.9)
plot(MA1_pos)
acf(MA1_pos)
ARMAacf(ma=c(0.9), lag.max = 10)

# Då koefficienten är negativ är har första laggen relativt stor positiv korrelation (runt 0.5) men sedan går inte
# korrelationen att skilja från 0 vid sig. 5%. Teoretiskt ser vi att den första laggen ska vara ca 0.5 negativ korrelation 
# (VARFÖR HAR VI FÅTT DEN NEG??) och att korrelationen sedan blir 0 vid vidare lag.
plot(MA1_neg)
acf(MA1_neg)
ARMAacf(ma=c(-0.9), lag.max = 10)

### 3.3 AR(2)
# Reella rötter pos pos
AR2_pos_pos <- arima.sim(model = list(ar=c(0.45, 0.5)), n= 400, sd = 1)
plot(AR2_pos_pos)
acf(AR2_pos_pos, lag.max = 50)
ARMAacf(ar=c(0.45, 0.5), lag.max = 500)
# Då modellen har reella rötter som båda är positiva ser vi att modellen tenderar att 
# sällan korsa det teoretiska väntevärdet 0. Om en observation är positiv tenderar nästa 
# observation att också vara positiv.
# Det tar förhållandevis många lag för att korrelationen ska försvinna.

AR2_neg_pos <- arima.sim(model = list(ar=c(-0.45, 0.5)), n= 400, sd = 1)
plot(AR2_neg_pos)
acf(AR2_neg_pos, lag.max = 50)
ARMAacf(ar=c(-0.45, 0.5), lag.max = 10)
# Då modellen har reella rötter där ena är positiv och den andra är negativ ser vi att modellen tenderar att 
#  korsa det teoretiska väntevärdet 0 vid varje lag. Den är centrerad kring 0. Om en observation är positiv tenderar nästa 
# observation att istället vara negativ
# Det tar förhållandevis många lag för att korrelationen ska försvinna.

AR2_neg_neg <- arima.sim(model = list(ar=c(-0.45, -0.5)), n= 400, sd = 1)
plot(AR2_neg_neg)
acf(AR2_neg_neg, lag.max = 50)
ARMAacf(ar=c(-0.45, -0.5), lag.max = 10)
# Vid båda koeffienterna negativ är autokorrelationen mycket låg. Teoretiskt byter den initiellt tecken efter två tidslag men 
# går sedan över till att byta tecken vid varje lag efter 1-2 lag.

# Vi finner bara stationära processer när den andra koefficienten är negativ.
AR2_C_pos_neg <- arima.sim(model = list(ar=c(1, -0.5)), n= 400, sd = 1)
plot(AR2_C_pos_neg)
acf(AR2_C_pos_neg, lag.max = 50)
ARMAacf(ar=c(1, -0.5), lag.max = 10)
# Vid komplexa rötter då första koefficienten är positiv  har vi initiellt hög positiv korrelation 
# som snabbt avtar men går mot 0 när lag -> /inf. Processen korsar relativt ofta det teoretiska väntevärdet.

AR2_C_neg_neg <- arima.sim(model = list(ar=c(-1, -0.5)), n= 400, sd = 1)
plot(AR2_C_neg_neg)
acf(AR2_C_neg_neg, lag.max = 50)
ARMAacf(ar=c(-1, -0.5), lag.max = 10)
# Vid komplexa rötter då båda koefficienterna är negativa  har vi initiellt hög negativ korrelation
# som snabbt avtar men går mot 0 när lag -> /inf. Processen är centrerad kring det teoretiska väntevärdet


### 3.4 ARMA(p,q)
# AR positivt, MA positiv
ARMA_pos_pos <- arima.sim(model = list(ar=c(0.9), ma=c(0.9)), n= 400, sd = 1)
ARMAacf(ar=c(0.9), ma=c(0.9), lag.max = 25)

plot(ARMA_pos_pos)
acf(ARMA_pos_pos, lag.max = 25)

# AR positiv, MA negativ
ARMA_pos_neg <- arima.sim(model = list(ar=c(0.9), ma=c(-0.9)), n= 400, sd = 1)
ARMAacf(ar=c(0.9), ma=c(-0.9), lag.max = 25)

plot(ARMA_pos_neg)
acf(ARMA_pos_neg, lag.max = 25)

# AR positiv, MA negativ
ARMA_neg_pos <- arima.sim(model = list(ar=c(-0.9), ma=c(0.9)), n= 400, sd = 1)
ARMAacf(ar=c(-0.9), ma=c(0.9), lag.max = 25)

plot(ARMA_neg_pos)
acf(ARMA_neg_pos, lag.max = 25)

# AR neg, MA neg
ARMA_neg_neg <- arima.sim(model = list(ar=c(-0.9), ma=c(-0.9)), n= 400, sd = 1)
ARMAacf(ar=c(-0.9), ma=c(-0.9), lag.max = 25)

plot(ARMA_neg_neg)
acf(ARMA_neg_neg, lag.max = 25)

#- Vi hade eventuellt kunnat testa multipla typ ar=c(0.45, -0.5), ma=c(0.45,-0.5) eller liknande


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
### COSINE
har.W <- harmonic(delta, 1)
model7 <- lm(delta ~ har.W)
summary(model7)



