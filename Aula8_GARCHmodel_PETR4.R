
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br

## GARCH model - PETR4 stock


install.packages("quantmod")
require(quantmod)

### Petrobrás

getSymbols('PETR4.SA',src='yahoo')
head(PETR4.SA)
tail(PETR4.SA)


PETR4.SA.Close <- PETR4.SA$PETR4.SA.Close
ts.plot(PETR4.SA.Close)

# Calculando o retorno

r_PETR4<- log(PETR4.SA.Close/lag(PETR4.SA.Close,k=1))
str(r_PETR4)
length(r_PETR4)

ts.plot(r_PETR4)
hist(r_PETR4,100)

## vamos encuntar um pouco a ST para termos uma informação mais relevante

#ts.plot(window(r_PETR4,start = 2010-01-01,frequency = 365))
r_PETR4_pos2010<-r_PETR4[739:2011]
hist(r_PETR4_pos2010,100)

max(r_PETR4_pos2010)
min(r_PETR4_pos2010)

## ------------------Stilized facts -------------------------


## 1. Random walk (martingal) property of prices

              # absence of autocorrelations in returns (relative price changes); 
              # no predictability of future price developments;
              # efficient price formation.


# no predictability of future price developments;
chartSeries(PETR4.SA)

# absence of autocorrelations in returns (relative price changes); 
acf(r_PETR4_pos2010)


## 2. Non normality

install.packages("normtest")
require(normtest)

# Fat tails of returns
## Curtose
install.packages("moments")
require(moments)

kurtosis(r_PETR4_pos2010)
kurtosis.norm.test(r_PETR4_pos2010)

skewness(r_PETR4_pos2010)
skewness.norm.test(r_PETR4_pos2010)


jb.norm.test(r_PETR4_pos2010)


## 3. Stationarity

require(urca)

# Level
adf.none <- ur.df(y = r_PETR4_pos2010, type = c("none"), lags = 24, selectlags = "AIC")
summary(adf.none)
acf(adf.none@res, lag.max = 36, drop.lag.0 = T)


## 4 . Clusters of volatility Clusters of volatility

ts.plot(r_PETR4_pos2010)
abline(h = mean(r_PETR4_pos2010),col=2, lwd=2)

## 5. Non linear dependence

acf(r_PETR4_pos2010^2)


## --------------------- Modeling PETR4 return ----------------------------

install.packages("fGarch")
require(fGarch)

fit<-garchFit(formula= r_PETR4_pos2010 ~ arma(1,0) + aparch(1,1), data=100*r_PETR4_pos2010, cond.dist = "norm",
              include.mean = TRUE, trace=FALSE)
summary(fit)


# plot in-sample volatility estimates
hist <- ts(r_PETR4_pos2010,start=c(2010,1,1),frequency = 252)
resid <- ts(fit@fitted,start=c(2010,1,1),frequency = 252)
ts.plot(hist,resid,col=c(2,1))


# forecast 12-steps-ahead
require(stats)
forecast <- predict(object = fit, h=12)
forecast



