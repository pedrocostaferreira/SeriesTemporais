# Vamos instalar o pacote e habilitá-lo.

install.packages("tseries")
require(tseries)


#####################################################################
#####################################################################
###################### Exemplo 1 ####################################

# Processos MA(1) com theta positivo e negativo

# theta +
sim.MA<-arima.sim(model=list(ma=0.88),n=100)
plot.ts(sim.MA)
acf(sim.MA)

# theta -
sim.MA<-arima.sim(model=list(ma=-0.88),n=100)
plot.ts(sim.MA)
acf(sim.MA)

#####################################################################
#####################################################################
###################### Exemplo 2 ####################################

# Processos MA(2) com theta positivo e negativo

# theta +
sim.MA<-arima.sim(model=list(ma=c(0.4,0.44)),n=100)
plot.ts(sim.MA)
acf(sim.MA)

# theta -
sim.MA<-arima.sim(model=list(ma=c(-0.4,-0.44)),n=100)
plot.ts(sim.MA)
acf(sim.MA)


#####################################################################
#####################################################################
###################### Exemplo 3 ####################################

# Processos AR(1) com phi positivo e negativo

#phi +
sim.AR<-arima.sim(model=list(ar=0.84),n=100)
plot.ts(sim.AR)
acf(sim.AR)

#phi -
sim.AR<-arima.sim(model=list(ar=-0.84),n=100)
plot.ts(sim.AR)
acf(sim.AR)


#####################################################################
#####################################################################
###################### Exemplo 4 ####################################

# Processos AR(2) com phi positivo e negativo

#phi +
sim.AR<-arima.sim(model=list(ar=c(0.64,0.2)),n=100)
plot.ts(sim.AR)
acf(sim.AR)

#phi -
sim.AR<-arima.sim(model=list(ar=c(-0.64,-0.2)),n=100)
plot.ts(sim.AR)
acf(sim.AR)
