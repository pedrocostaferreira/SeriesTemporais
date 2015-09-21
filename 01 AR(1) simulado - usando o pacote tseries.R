#### Simulando com o pacote tseries

# Vamos instalar o pacote e habilitá-lo.
install.packages("tseries")
require(tseries)

# Simulando um processo AR(1) com parâmetro 0.5
sim.AR<-arima.sim(model=list(ar=0.5),n=100)
plot.ts(sim.AR)
acf(sim.AR)

## Observe que a FAC apresenta decrescimento lento