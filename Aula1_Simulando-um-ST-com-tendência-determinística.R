# simulando um processo com tendência determinística 


td <- function(beta0,beta1,phi,sigma,N){
  R<- NULL
  y<- 0
  for(i in 1:N){
    y<- beta0 + beta1 * i + phi * y + rnorm(1,0,sigma)
    R[i]<-y
  }
  R<-R[20:N]
  return(R)  
}

# Vamos testar nossa função e ver como ficou nosso gráfico
tend_det<- td(10,0.2,0.55,1,100)
plot.ts(tend_det)

## Corrigindo a Tendência Determinística (lembrando que no R já podemos fazer esse correção 
## diretamente na função de estimação do nosso modelo SARIMA)

t<-20:100
reg<-lm(tend_det~ t)
res<-reg$residuals
plot.ts(res)