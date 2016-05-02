
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


## Analisando alguns fatos estilizados


setwd("C:\\Users\\pedro.guilherme\\Dropbox\\12 Cursos Graduação\\02 Econometria II - Séries Temporais\\08 Github R code")

rm(list=ls())

## ===================== IBOVESPA ================================

IBOV <- read.csv(file = "IBOV.csv",header = F,sep = ";",dec = ",")
IBOV <- IBOV$V2
IBOV <- ts(IBOV, start = c(2002,1),frequency = 12)
ts.plot(IBOV)

require(dygraphs)
dygraph(IBOV)

ret_IBOV <- NULL
for (i in 1:length(IBOV)) {
  ret_IBOV[i] <- log(IBOV[i+1]/IBOV[i])
}
ret_IBOV <- ret_IBOV[1:(length(IBOV)-1)]
ret_IBOV <- ts(ret_IBOV, start = c(2002,1),frequency = 12)

dygraph(ret_IBOV, "Retorno IBOVESPA")


## ===================== CDI ================================


CDI <- read.csv(file = "CDI.csv",header = T,sep = ";",dec = ",")
CDI <- ts(CDI, start = c(2002,1),frequency = 12)
ts.plot(CDI)

require(dygraphs)
dygraph(CDI)

ret_CDI <-NULL
for (i in 1:length(CDI)) {
ret_CDI[i] <- log(CDI[i+1]/CDI[i])
}
ret_CDI <- ret_CDI[1:(length(CDI)-1)]
ret_CDI <- ts(ret_CDI, start = c(2002,1),frequency = 12)

dygraph(ret_CDI, "Retorno CDI")


## -------- Laverage Efect ----------- 

layout(1:2)
ts.plot(CDI)
ts.plot(ret_CDI)

layout(1:2)
ts.plot(IBOV)
ts.plot(ret_IBOV)




