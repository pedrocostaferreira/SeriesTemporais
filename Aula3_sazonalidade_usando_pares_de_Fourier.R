## Modelando a sazonalidade usando pares de termos de Fourier

## Forecasting: principles and practice
## https://www.otexts.org/fpp

## ---- Accidental Deaths in the US 1973–1978 ----
# A time series giving the monthly totals of accidental deaths in the USA.
# The values for the first six months of 1979 are 7798 7406 8363 8460 9217 9316.


library(fpp)

plot(USAccDeaths)
fit <- auto.arima(USAccDeaths,
                  xreg=fourier(USAccDeaths,5),
                  seasonal=FALSE)

fc <- forecast(fit,xreg=fourierf(USAccDeaths,5, 24))
plot(fc)

## Observe o exemplo abaixo onde não modelamos a sazonalidade

fit2 <- auto.arima(USAccDeaths,seasonal=FALSE)
fc2 <- forecast(fit2)
plot(fc2)
