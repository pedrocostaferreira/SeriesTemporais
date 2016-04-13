
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


## MODELS WITH TREND


## -------------------- Random Walk process -------------------------------
set.seed(10)
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t-1] + w[t]
plot(x, type="l")

# Autocorrelation function
install.packages("TSA")
require(TSA)
acf(x, lag.max = 36, drop.lag.0 = T)

acf(diff(x), lag.max = 36, drop.lag.0 = T, na.action = na.omit)


## Random Walk process with drift
c <- 0.5
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- c + x[t-1] + w[t]
plot(x, type="l")



## ---------------- Fitting a Random Walk process to Financial Data ----------------

install.packages("quantmod")
require(quantmod)

### Petrobrás

getSymbols('PETR4.SA',src='yahoo')
head(PETR4.SA)
tail(PETR4.SA)

chartSeries(PETR4.SA)


PETR4.SA.Close <- PETR4.SA$PETR4.SA.Close
ts.plot(PETR4.SA.Close)

acf(PETR4.SA.Close, lag.max = 36, drop.lag.0 = T)

acf(diff(PETR4.SA.Close), lag.max = 36, drop.lag.0 = T, na.action = na.omit)


### Apple

getSymbols('AAPL',src='yahoo')
chartSeries(AAPL)
# We can use the following commands to (respectively) obtain the Open, High, Low, Close, Volume
# and Adjusted Close prices for the Microsoft stock: Op(MSFT), Hi(MSFT), Lo(MSFT), Cl(MSFT), Vo(MSFT), Ad(MSFT).

plot(Ad(AAPL))
acf(Ad(AAPL), lag.max = 36, drop.lag.0 = T)
acf(diff(Ad(AAPL)), lag.max = 36, drop.lag.0 = T, na.action = na.omit)


