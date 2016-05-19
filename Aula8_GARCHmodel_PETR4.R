
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


## CRAN Task View: Empirical Finance
## https://cran.r-project.org/web/views/Finance.html

## Vignettes: rugarch package
## https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf


## GARCH and TARCH model - PETR4 stock


install.packages("quantmod")
require(quantmod)

### Petrobrás

getSymbols('PETR4.SA',src='yahoo')
head(PETR4.SA)
tail(PETR4.SA)

PETR4.SA.Close <- PETR4.SA$PETR4.SA.Close
ts.plot(PETR4.SA.Close)

# Calculando o retorno

r_PETR4 <- dailyReturn(PETR4.SA.Close,type = "log")
ts.plot(r_PETR4)
hist(r_PETR4,100)


## vamos encuntar um pouco a ST para termos uma informação mais relevante

r_PETR4_pos2010 <- window(r_PETR4, start = "2010-01-01")
ts.plot(r_PETR4_pos2010)
        
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

#############################################################################################
## --------------------- Modeling PETR4 return GARCH (1,1) ----------------------------
#############################################################################################

install.packages("rugarch")
require(rugarch)

############################################
#######     Estimação      ###############
############################################


garch11.spec = ugarchspec(mean.model = list(armaOrder = c(1,0),include.mean=TRUE), 
                          variance.model = list(garchOrder = c(1,1), 
                          model = "sGARCH"))

garch.fit = ugarchfit(garch11.spec, data = r_PETR4_pos2010*100,fit.control=list(scale=TRUE), 
                      distribution.model = "norm")
print(garch.fit)

coef(garch.fit)

############################################
####### Diagnóstico #######################
############################################

plot(garch.fit, which = "all")

##############################################################
####### plot in-sample and forecast 12-steps-ahead ############
###############################################################

# plot in-sample volatility estimates
hist <- ts(r_PETR4_pos2010,start=c(2010,1,1),frequency = 252)
fitted <- ts(garch.fit@fit$fitted.values,start=c(2010,1,1),frequency = 252)
ts.plot(r_PETR4_pos2010,fitted,col=c("red","blue"))


# Time Series Prediction (unconditional)
previsao <- ugarchforecast(garch.fit,n.ahead = 12)
plot(previsao,which = 1)

# Sigma Prediction (unconditional)
previsao <- ugarchforecast(garch.fit,n.ahead = 12)
plot(previsao,which = 3)


#############################################################################################
## --------------------- Modeling PETR4 return TARCH (1,1) ----------------------------
#############################################################################################

install.packages("rugarch")
require(rugarch)

############################################
#######     Estimação      ###############
############################################


tarch11.spec = ugarchspec(mean.model = list(armaOrder = c(1,0),include.mean=TRUE), 
                          variance.model = list(garchOrder = c(1,1), model = "fGARCH",
                                                submodel = "TGARCH"))

tarch.fit = ugarchfit(tarch11.spec, data = r_PETR4_pos2010*100,fit.control=list(scale=TRUE), 
                      distribution.model = "norm")
print(tarch.fit)

coef(tarch.fit)

############################################
####### Diagnóstico #######################
############################################

plot(tarch.fit, which = "all")


##############################################################
####### plot in-sample and forecast 12-steps-ahead ############
###############################################################

# plot in-sample volatility estimates
hist <- ts(r_PETR4_pos2010,start=c(2010,1,1),frequency = 252)
fitted <- ts(tarch.fit@fit$fitted.values,start=c(2010,1,1),frequency = 252)
ts.plot(r_PETR4_pos2010,fitted,col=c("red","blue"))


# Time Series Prediction (unconditional)
previsao <- ugarchforecast(tarch.fit,n.ahead = 12)
plot(previsao,which = 1)

# Sigma Prediction (unconditional)
previsao <- ugarchforecast(tarch.fit,n.ahead = 12)
plot(previsao,which = 3)



## ----------------------- Value at Risk ---------------

VaR=quantile(tarch.fit,0.01)
plot(VaR)

carteira <-(VaR/100)*1000000
plot(carteira)


