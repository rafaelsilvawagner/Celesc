require(openxlsx)
require(xts)
require(forecast)
require(ggplot2)

#Carrega os dados
destino <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\energia_injetada.xlsx'
series <- openxlsx::read.xlsx(destino,
                              detectDates = TRUE)

series <- xts (series[,-1],
               order.by = series[,1])
series <- diff(series,1)
series <- diff(series,12)


#carrega serie de temperatura
destino_temp <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\temperatura.xlsx'
temp <- openxlsx::read.xlsx(destino_temp,
                            detectDates = TRUE)

temp <- xts (temp[,-1],
             order.by = temp[,1])

temp_estac <- diff(temp,12)[,'media']

#carrega serie de precipitacao
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\precipitacao.xlsx'
prec <- openxlsx::read.xlsx(destino_prec,
                            detectDates = TRUE)

prec <- xts (prec[,-1],
             order.by = prec[,1])

prec_estac <- diff(prec,12)
colnames(prec_estac) <- 'precipitacao'

#carrega serie ibc-br
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\pim.xlsx'
pim <- openxlsx::read.xlsx(destino_prec,
                           detectDates = TRUE)

pim <- xts (pim[,-1],
            order.by = pim[,1])

pim <- diff(pim)

#junta as duas series
serie <- merge.xts(series,temp_estac,
                   join = 'inner')
serie <- merge.xts(serie,prec_estac,
                   join = 'inner')
serie <- merge.xts(serie,pim,
                   join = 'inner')
serie <- na.omit(serie)

colnames(serie) <- c('demanda','temp',
            'precipitacao','pim')

#Comecemos conhecendo as series individuais
autoplot(serie[,1])
autoplot(serie[,2])
autoplot(serie[,3])
autoplot(serie[,4])

#Verifica o histograma dos dados
qplot(serie[,1])
qplot(serie[,2])
qplot(serie[,3])
qplot(serie[,4])



#Verifica as estatísticas descritivas dos dados
require(fBasics)
fBasics::basicStats(serie)



#
require(psych)
psych::pairs.panels(as.matrix(serie),
                    lm = TRUE, ci = TRUE)

summary(lm(demanda~1+temp+pim,serie))
