#Simulando um Processo MA usando o pacote tseries

# Vamos instalar o pacote e habilitá-lo.
install.packages("tseries")
require(tseries)

# Simulando um processo MA(1) com parâmetro 0.8
sim.ma<-arima.sim(model=list(ma=0.8),n=100)
plot.ts(sim.ma)

acf(sim.ma)

## observe que a FAC apresenta corte brusco no lag 1