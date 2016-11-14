
setwd("C:\\Users\\pedro.guilherme\\Dropbox\\08 Cursos Pedro Costa Ferreira\\04 Macroeconometria usando R\\03 Estudos de caso\\Existe relação entre inflação e crescimento")

## O que queremos testar: inflação causa crescimento?!
## É possível gerar crescimento econômico gerando inflação?! Ou seja, inflação hoje causa crescimento no longo prazo?!



require(xlsx)

ipca<-read.xlsx(file = "ipca.xls",sheetName = "ipca")
ipca<-ipca$IPCA
ipca<-ts(data = ipca, start = c(1993,12),end=c(2015,8),frequency = 12)
ipca<-window(x = ipca, start=c(2000,1))
ts.plot(ipca)

pib<-read.xlsx(file = "pib.xls",sheetName = "pib")
pib<-pib$PIB.
pib<-ts(data = pib, start = c(1993,12),end=c(2015,8),frequency = 12)
pib<-window(x = pib, start=c(2000,01))
ts.plot(pib)

layout(1:2)
ts.plot(pib)
ts.plot(ipca)

## two y-axes plot
plot(ipca,type="l",col="red")
par(new=TRUE)
plot(pib,type="l")

## ------------------------------------- Verifying Unit root ------------------------ 

## dado que o foco é estimar uma relação de longo prazo, estamos na verdade interessados em estimar um modelo de correção de erros,
## Porém, para isso é preciso primeiro determinar se as séries em questão são integradas da mesma ordem (e numa ordem maior que 0). 


## Before ADF test, let's see the ACF

acf(ipca)
acf(pib)

## Unit root test - IPCA

require(urca)

# Level
adf.drift <- ur.df(y = ipca, type = c("drift"), lags = 24, selectlags = "AIC")
summary(adf.drift)
acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)

#1 difference
y = diff(log(ipca),lag = 1,differences = 1)
yy <- diff(y,lag = 12,differences = 1)
adf.drift <- ur.df(yy, type = c("drift"), lags = 24, selectlags = "AIC")
summary(adf.drift)
acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)


## Unit root test - PIB
# Level
adf.drift <- ur.df(y = pib, type = c("drift"), lags = 24, selectlags = "AIC")
summary(adf.drift)
acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)

#1 difference
y = diff(log(pib),lag = 1,differences = 1)
yy <- diff(y,lag = 12,differences = 1)
adf.drift <- ur.df(yy, type = c("drift"), lags = 24, selectlags = "AIC")
summary(adf.drift)
acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)


## So, both TS are I(1)


reg <- lm(log(pib) ~ log(ipca))
summary(reg)

## Durbin Watson test
install.packages("car")
require(car)
durbinWatsonTest(reg, max.lag=1, simulate=TRUE, reps=1000,method=c("resample"), alternative=c("two.sided"))

## residual test
plot(reg$residuals,type="l")
abline(h=mean(reg$residuals), col="red",lwd=1)

## Residuals are stationary?! No

## Unit rot test
# Level
adf.drift <- ur.df(y = reg$residuals, type = c("drift"), lags = 24, selectlags = "AIC")
summary(adf.drift)
acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)


## Engle Granger Test

install.packages("egcm")
require(egcm)

EG_test <- egcm(pib,ipca)
summary(EG_test)

## --------------------- Short term relationship --------------------

reg <- lm(diff(log(pib)) ~ diff(log(ipca)))
summary(reg)

## There is no short term relationship

