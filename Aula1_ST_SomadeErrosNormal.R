## Nós mostramos matematicamente em sala de aula que a equação Yt= erro_t + erro_t-1, onde
## erro ~ N(0,sigma) é estacionária de segunda ordem. Nesse código vamos provar que isso é
## verdade.

## O primeiro passo foi criar uma função que simule uma ST gerada a partir da equação proposta acima.

soma_erros<-function(N,sigma){
  R<-NULL
  erro<-rnorm(n = N,mean = 0,sd = sigma)
  for(i in 1:(N+1)){
    y<-erro[i]+erro[i-1]
    R[i]<-y
  }
  R<-R[2:N]
   return(R)
}

## teste teste teste

## Em seguida, vamos gerar 1000 informações com variância 1 e, em seguida, plotar a ST gerada a partir
## da equação de soma dos erros.

ST<-soma_erros(1000,1)
plot(ST,type = "l")

## Conforme esperado, a ST gerada é estacionária tanto na média quanto na variância.

## Quanto a autocorelação, observe que a FAC apresenta corte brusco no lag 1, comportamento típico do
## processo gerado acima (um processo MA(1)) e de acordo com os resultados que encontramos matematicamente.

acf(ST)


