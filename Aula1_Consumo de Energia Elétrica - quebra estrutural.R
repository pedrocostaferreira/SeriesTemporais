# Consumo de Energia Elétrica (quebra estrutural)

# Antes vamos instalar o pacote
install.packages("xlsx")
require(xlsx)


dados.cons_ee <- read.xlsx("consumo_ee.xlsx",sheetName="serie_R",as.data.frame=TRUE,header=T)
consumo_ee<-ts(dados.cons_ee,start=1994, freq=1)
ts.plot(consumo_ee)

# Reduzindo o gráfico para deixar mais claro a "quebra estrutural"

consumo_ee_1996<-ts(consumo_ee[45:length(consumo_ee)],start=1996, freq=1)
plot.ts(consumo_ee_1996)

## Essa quebra ocorreu devido ao racionamento de energia elétrica em 2001
