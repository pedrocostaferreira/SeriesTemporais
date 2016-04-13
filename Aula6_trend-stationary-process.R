
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br

## MODELS WITH TREND

## ------- simulando um processo com tendência determinística -------------


td <- function(beta0,beta1,sigma,N){
  R<- NULL
  y<- 0
  for(i in 1:N){
    y<- beta0 + beta1 * i + rnorm(1,0,sigma)
    R[i]<-y
  }
  R<-R[20:N]
  return(R)  
}

# Vamos testar nossa função e ver como ficou nosso gráfico
tend_det<- td(10,0.2,0.55,100)
plot.ts(tend_det)
require(TSA)
acf(tend_det, drop.lag.0 = T)

## Corrigindo a Tendência Determinística (lembrando que no R já podemos fazer esse correção 
## diretamente na função de estimação do nosso modelo SARIMA)

t<-20:100
reg<-lm(tend_det~ t)
res<-reg$residuals
plot.ts(res)
