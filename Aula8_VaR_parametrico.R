
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


## Value at Risk paramétrico feito a mão ##


install.packages("quantmod")
require(quantmod)
getSymbols('PETR4.SA',src='yahoo')
PETR4.SA.Close <- PETR4.SA$PETR4.SA.Close
r_PETR4 <- dailyReturn(PETR4.SA.Close,type = "log")
r_PETR4_pos2010 <- window(r_PETR4, start = "2010-01-01")

garch11.spec = ugarchspec(mean.model = list(armaOrder = c(1,0),include.mean=TRUE), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"))

garch.fit = ugarchfit(garch11.spec, data = r_PETR4_pos2010*100,fit.control=list(scale=TRUE), 
                      distribution.model = "norm")


m_t <- garch.fit@fit$coef[[1]] + garch.fit@fit$coef[[2]]*garch.fit@model$modeldata$data[length(garch.fit@model$modeldata$data)]

h_t <- garch.fit@fit$coef[[3]] + garch.fit@fit$coef[[4]]*garch.fit@fit$residuals[length(garch.fit@fit$residuals)]+
  garch.fit@fit$coef[[5]]*garch.fit@fit$var[length(garch.fit@fit$var)]


##########################################
## Como sabemos para calcular o VaR precisamos do nível de confiança e do valor investido 
confidence <- 1.96
volume<- 30000

r_asterisco <- m_t - confidence*sqrt(h_t)


(VaR <- (r_asterisco/100)*volume)


####################################################################################################
###################################################################################################
############### VaR paramétrico usando o BETS #####################################################

require(devtools)
install_github("pedrocostaferreira/BETS",force = TRUE)
require(BETS)

BETS.param_var(garch.fit,30000,conf.level=0.95)


