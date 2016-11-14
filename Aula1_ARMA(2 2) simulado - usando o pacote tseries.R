#Simulando um Processo ARMA(2,2) usando o pacote tseries

# Vamos instalar o pacote e habilitá-lo.
install.packages("tseries")
require(tseries)

# Simulando um processo ARMA(2,2) com parâmetros 0.8, -0.2, -0.7 e 0.5
sim.arma<-arima.sim(model=list(ar=c(0.8,-0.2),ma=c(-0.7,0.5)),n=100)
plot.ts(sim.arma)

acf(sim.arma)
pacf(sim.arma)

## observe que apenas observando a FAC e a FACP não dá para saber qual é a estrutura do modelo ARMA (p,q)