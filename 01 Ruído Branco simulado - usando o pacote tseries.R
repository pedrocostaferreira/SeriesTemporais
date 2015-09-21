#### Simulando um processo Ruído Branco com o pacote tseries

# Vamos instalar o pacote e habilitá-lo.
install.packages("tseries")
require(tseries)


sim.wn<-arima.sim(model=list(),n=100)
plot.ts(sim.wn)
acf(sim.wn)

## observe que a FAC não apresenta lag significante.