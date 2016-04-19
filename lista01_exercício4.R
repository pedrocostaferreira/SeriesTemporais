
## Considere o processo estocástico Z_t=a_t, onde a_t é um processo ruído branco com t=0,±1,±2,…, e:
  
  
##        |  +1, com probabilidade 1⁄2
## a_t =  | 
##        |  -1, com probabilidade 1⁄2

## (a) Obtenha a média do processo Z_t;
## (b) Calcule a autocovariância γ(τ), τ=0,±1,±2,…
## (c) Calcule a autocorrelação ρ(τ), τ=0,±1,±2,…


at<-NULL
prob<- runif(200,1,100)
for (i in 1:200){
ifelse(prob[i]<51,at[i]<- 1,at[i]<- -1)
}
plot(at)

# (a)
(media<-mean(at))

# (b)
(var<-var(at))

# (c)
(acf(at))


## Os resultados são os mesmos que encontramos matematicamente em sala de aula.
