

## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


## Value at Risk (VaR) não paramétrico

install.packages("quantmod")
require(quantmod)

### Petrobras

getSymbols('PETR4.SA',src='yahoo')
head(PETR4.SA)
tail(PETR4.SA)

chartSeries(PETR4.SA)


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

# TEste de Normalidade
install.packages("normtest")
require(normtest)

jb.norm.test(r_PETR4_pos2010)

# VaR n?o param?trico - Simula??o Hist?rica

# Propensão ao risco: 5%


(VaR_SH<- quantile(x =r_PETR4_pos2010,probs = 0.05))

## Isto é, 95% do tempo n?o h? perda maior que 4%

## Imagine que você tenha R$ 100.000,00 investidos na PETR4, então

# VaR=V0*VaR_SH

(Var=100000*VaR_SH)

# Então sua perda máxima esperada em um dia é aproximadamente R$ 4.000,00 

