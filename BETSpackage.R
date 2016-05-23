
## Econometria de Séries Temporais
## Prof. Dr. Pedro Costa Ferreira
## pedro.guilherme@fgv.br


###### ----  Utilizando o pacote BETS ------------


## Se você nunca utilizou os softwares R E RStudio assita o vídeo abaixo:
## Video-aula instalação: https://www.youtube.com/watch?v=8HQHf5XCS7g

install.packages("devtools")
require(devtools)
install_github("pedrocostaferreira/BETS")
require(BETS)

## Antes de começar a usar o pacote vamos conhece-lo:
?BETS

## Para conhecer ainda mais o pacote vá até a conta "https://github.com/pedrocostaferreira/BETS" e
## leia o arquivo ..pdf


## Vamos falar um pouco sobre o indicador de incerteza


BETSsearch(name = "IIE-Br")

IIE_Br <- BETSget("100.0")
decompose_IIE <- decompose(IIE_Br)
plot(decompose_IIE)

## Vamos ver a tendência do indicador

plot(IIE_Br)
lines(decompose_IIE$trend, col = "red")