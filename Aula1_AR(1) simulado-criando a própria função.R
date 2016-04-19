## Simulando um Processo AR(1) no R

## Criando sua própria função
# phi1 -> parâmetro do modelo AR(1)
# N -> número de valores que deseja gerar
# sigma -> variância do erro (erro ~ N(0,sigma))

ar1<-function(phi1,N,sigma)
{
  R<-NULL
  y<-0
  for (i in 1:N)
  {
    y<-y*phi1+rnorm(1,0,sigma)
    R[i]<-y
  }
  return(R)
}

## Após criar a função, vamos testá-la.
Serie_Sim_AR1<-ar1(0.72,100,1)
ts.plot(Serie_Sim_AR1)
acf(Serie_Sim_AR1)
