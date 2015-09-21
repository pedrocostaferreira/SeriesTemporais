## Vamos mostrar como ler a ST diária do valor das ações da Petrobrás em uma planilha excel.
## No curso aprendemos que podemos fazer isso de uma maneira muito mais eficiente usando o pacote "QuantMod",
## mas nossa ideia aqui é focar na leitura da planilha excel e mostrar uma maneira de calcular o retorno da ação.
## Além disso, mostramos como calcular a curtose e a assimetria, dois fatos estilizados das ST de retorono
## financeiro


## Antes precisamos instalar e carregar o programa .XLSX
install.packages("xlsx")
require(xlsx)

dados.petr <- read.xlsx("petr_dia.xlsx", sheetName="plan1",as.data.frame = TRUE, header = TRUE)

petr.dia <- ts(dados.petr[,1],start=as.Date('2000-3-1'), frequency=365)

ts.plot(petr.dia, col="black",lty = 1,lwd=2)

## Retorno
ret_petr.dia <- matrix(,length(petr.dia),)

for (i in 2:length(petr.dia))
{
  ret_petr.dia[i] <- (petr.dia[i]-petr.dia[i-1])/petr.dia[i-1]
}

ret_petr.dia[124] <- 0
ts.plot(ret_petr.dia, col="black",lty = 1,lwd=2)

hist(ret_petr.dia,100)

## Para calcula a Assimetria e a Curtose precisamos instalar e carregar o pacote "moments"
install.packages("moments")
require(moments)

##

skewness(ret_petr.dia, na.rm = TRUE)
kurtosis(ret_petr.dia, na.rm = TRUE)

## Comparando com a Normal - observe que a Curtose é aproximadamente 3 e a assimetria é aproximadamente 0;
normal<-rnorm(10000,0,1)
hist(normal,100)
kurtosis(normal)
skewness(normal)

