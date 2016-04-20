
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br

## Capítulo 5 - livro: Análise de Séries Temporais em R: um curso introdutório



# Instalar o pacote BETS

require(devtools)
install_github("GreedBlink/BETS-beta")
require(BETS)


install.packages("devtools")
require(devtools)
install_github("pedrocostaferreira/BETS-package")
require(BETS)

# Definir o diretório de trabalho

setwd("C:\\Users\\pedro.guilherme\\Dropbox\\12 Cursos Graduação\\02 Econometria II - Séries Temporais\\00 Aulas 2\\Aula 7 - Modelagem AirPassengers")


# Comentário 1: há diversos pacotes que são necessários serem instalados para uma boa AST, contudo quando vc
# instala o pacote BETS todos esses pacotes auxiliares já são instalados, o que facilita nosso trabalho. 

# carregando a ST
data("AirPassengers")

# Análise Exploratória da ST

ts.plot(AirPassengers)
monthplot(AirPassengers)

plot(decompose(AirPassengers))


# Conhecendo a ST antes de iniciar a modelagem
require(TSA) # temporário!!
acf(AirPassengers, lag.max = 36, drop.lag.0 = T)

## Teste de Raíz Unitária
require(urca) # temporário

adf.drift <- ur.df(AirPassengers, type = c("drift"), lags = 24, selectlags = "AIC")

summary(adf.drift)

# para ver se o teste está correto e preciso verificar a autocorrelação do resíduo

acf(adf.drift@res, lag.max = 36, drop.lag.0 = T)

# Ao concluir que a ST tem raiz unitária, precisamos descobrir o número de diferenciações necessárias 
# para torná-la estacionária

ts.plot(diff(AirPassengers, lag = 1, differences = 1))

acf(diff(AirPassengers, lag = 1, differences = 1),lag.max = 36, drop.lag.0 = T)


# Observe que ao aplicar a diferenciação, a ST aparenta estar estacionária na média, mas a variância é 
# crescente ao longo do tempo. Como sabemos, um dos pressupostos da teoria Box \& Jenkins é que a ST seja
# também estacionária na variância, para tal, iremos passar o log 

ts.plot(diff(log(AirPassengers),lag = 1,differences = 1))
acf(diff(log(AirPassengers), lag = 1, differences = 1),lag.max=48, drop.lag.0=T)

## Avaliando a estacionariedade da parte sazonal


# o leitor atento já observou que nos lags sazonais lags sazonais são 1(=12), 2(=24), 3 (=36), etc.} 
# a função de autocorrelação também apresenta um decrescimento lento, indicando que a ST é não estacionária
# na parte sazonal

acf(diff(diff(log(AirPassengers), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 48, drop.lag.0 = T)

# Observe que agora a FAC apresenta cortes bruscos nos lags 1 e 12. E não apresenta mais decrescimento lento 
# tanto na parte sazonal quanto na não sazonal. 

# Vamos refazer o teste de RU para confirmar a estacionariedade da ST após aplicar as transformações anteriores.
# O valor da estatística de teste (-4,0398) é inferior ao valor crítico (-2,88). 
# Assim, podemos concluir que a série é estacionária.

adf.drift2 <- ur.df(y = diff(diff(log(AirPassengers), lag = 1), lag = 12), type = "drift", lags = 24, selectlags = "AIC")
acf(adf.drift2@res, lag.max = 36, drop.lag.0 = T)

summary(adf.drift2)


### ----------- Modelando a ST ----------


# Identificação

layout(1:2)
acf(diff(diff(log(AirPassengers), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 48, drop.lag.0 = T)
pacf(diff(diff(log(AirPassengers), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 48)

# Observando os gráficos e com um pouco de boa vontade podemos pensar nos seguintes modelos:
# SARIMA(1,1,1)(1,1,1)} - corte brusco na FAC e na FACP nas partes sazonais e não sazonais;
# SARIMA(0,1,1)(0,1,1)} - corte brusco na FAC e decrescimento das partes sazonais e não sazonais.


# Estimação

library("forecast")
fit.air <- Arima(AirPassengers, order = c(1,1,1), seasonal = c(1,1,1),method = "ML", lambda = 0)
summary(fit.air)

# Para verificar, de forma rápida, se os parâmetros do modelo são significativos, desenvolvemos uma função no R

t.test <- function(modelo_arima){
  # estatística t
coef <- modelo_arima$coef
se <- sqrt(diag(modelo_arima$var.coef))
t <- abs(coef/se)
  # Teste t
ok <- t > qt(0.975, length(modelo_arima$x) - sum(modelo_arima$arma[c(1,2,3,4,6,7)]))
resul <- data.frame(Coef = coef, sd = se, t = t, rej_H0 = ok)
return(resul)
}

# teste de significância para o modelo SARIMA(1,1,1)(1,1,1)12
t.test(fit.air)

# Conforme pode ser observado, temos um modelo SARIMA(0,1,1)(0,1,1)$_{12}$ onde todos os parâmetros são 
# significativos e que minimiza os critérios de informação

fit.air <- Arima(AirPassengers, order = c(0,1,1), seasonal = c(0,1,1), method = "ML", lambda = 0)
summary(fit.air)

t.test(fit.air)


# Diagnóstico

diag <- tsdiag(fit.air, gof.lag = 20)

Box.test(x = fit.air$residuals, lag = 24, type = "Ljung-Box", fitdf = 2)


require(FinTS)
ArchTest(fit.air$residuals,lags = 12)

require(normtest)
jb.norm.test(fit.air$residuals, nrepl=2000)


# Previsão

require(forecast)
prev <- forecast(object = fit.air, h=12, level = 0.95)
plot(prev)

accuracy(fit.air)


# Extraindo as Previsões
write.csv2(data.frame(prev),"previsao.csv")



