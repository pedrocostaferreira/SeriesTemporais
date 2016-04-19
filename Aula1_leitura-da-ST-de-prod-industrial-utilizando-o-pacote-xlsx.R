## Vamos ler a ST de produção industrial (disponível no site do IBGE) utilizando o pacote xlsx.
## Após a leitura, iremos plotar a ST utilizando o gráfico ts.plot.

# Antes vamos instalar o pacote
install.packages("xlsx")
require(xlsx)

## pim_pf_sas <- ST sem ajuste sazonal
## pim_pf_cas <- ST com ajuste sazonal

dados.pim<-read.xlsx("pim-pf.xlsx",sheetName = "Plan1", as.data.frame = TRUE, header = TRUE)

pim_pf_sas <- ts(dados.pim[,1],start=c(2002,1), freq=12)
pim_pf_cas <- ts(dados.pim[,2],start=c(2002,1), freq=12)

ts.plot(pim_pf_sas, col= "blue",lty = 2,lwd=2)
legend("bottomright","pim_pf_sas",lty = 2,col= "blue")

ts.plot(pim_pf_cas, col="red",lty = 2,lwd=2)
legend("bottomright","pim_pf_cas",lty = 2,col= "blue")

ts.plot(pim_pf_sas, pim_pf_cas, col= c("blue","red"),lty = c(2, 1),lwd=c(2,1))
legend("bottomright",lty = c(2, 1),lwd=c(2,1), c("pim_pf_sas", "pim_pf_cas"), col= c("blue", "red"))
