require(openxlsx)
require(xts)
require(forecast)
require(sarima)
require(astsa)


#Carrega os dados
destino <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\energia_injetada.xlsx'
series <- openxlsx::read.xlsx(destino,
                              detectDates = TRUE)

series <- xts (series[,-1],
               order.by = series[,1])

#carrega serie de temperatura
destino_temp <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\temperatura.xlsx'
temp <- openxlsx::read.xlsx(destino_temp,
                            detectDates = TRUE)

temp <- xts (temp[,-1],
             order.by = temp[,1])

temp_estac <- temp

#carrega serie de precipitacao
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\precipitacao.xlsx'
prec <- openxlsx::read.xlsx(destino_prec,
                            detectDates = TRUE)

prec <- xts (prec[,-1],
             order.by = prec[,1])

prec_estac <- prec
colnames(prec_estac) <- 'precipitacao'

#carrega serie ibc-br
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\pim.xlsx'
pim <- openxlsx::read.xlsx(destino_prec,
                           detectDates = TRUE)

pim <- xts (pim[,-1],
            order.by = pim[,1])



#junta as duas series
serie <- merge.xts(series,temp_estac,
                   join = 'inner')
serie <- merge.xts(serie,prec_estac,
                   join = 'inner')
serie <- merge.xts(serie,pim,
                   join = 'inner')
serie <- na.omit(serie)

#Define a simulação
share_train <- 0.70 #Usaremos 69% da amostra pois isso gera 3 anos de previsões

n.obs <- nrow(serie)
n.train <- round(share_train * n.obs)
n.teste <- n.obs - n.train

index.obs <- 1:n.obs
index.train <- 1:n.train
index.teste <- (n.train + 1):n.obs

dates.obs <- index(serie)
dates.train <- dates.obs[index.train]
dates.teste <- dates.obs[index.teste]

#Monta o local aonde guardar as previsões
forecasts_injetada <- matrix(0,
                             nrow = n.teste,
                             ncol = n.teste)

forecasts_injetada <- xts(forecasts_injetada,
                          order.by = index(serie)[index.teste-1])

colnames(forecasts_injetada) <- paste("Passo",1:n.teste, sep = "_")


#Realiza as previsões
for(loop in 1:n.teste){
  #Seleciona a amostra
  last_train <- (n.train-1+loop)
  n.forecasts <- (n.teste-loop+1)
  passado <- serie[1:last_train,]
  
  #Monta um arma pra prever a media de temperatura
  modelo_temp <- Arima(y = passado[,'media'],
                       order = c(1,0,0), 
                       include.mean = FALSE,
                       seasonal = list(order = c(1,1,0), period = 12),
                       method = c("CSS-ML"))
  
  #Monta um arma pra prever a media de temperatura
  modelo_pib <- Arima(y = passado[,'pim'],
                      order = c(1,1,0), 
                      include.mean = FALSE,
                      seasonal = list(order = c(0,1,0), period = 12),
                      method = c("CSS-ML"))
  
  
  pred_temp <- predict(modelo_temp, 
                       n.forecasts)$pred
  
  pred_pib <- predict(modelo_pib, 
                      n.forecasts)$pred
  
  
  
  pred_xreg <- cbind(pred_temp,pred_pib)
  
  
  
  modelo_injetada <- astsa::sarima.for(xdata = (passado[,'series']) ,
                                       newxreg = pred_xreg,
                                       p = 1, d = 1, q = 0 ,
                                       P = 1, D = 1, Q = 1, S = 12,
                                       n.ahead = n.forecasts)
  
  
  #modelo_injetada <- sarima(xdata = passado[,'series'],
  #                          xreg = passado[,c('media','pim')],
  #                          p = 1, d = 1, q = 1, 
  #                          P = 0, D = 1, Q = 0, S = 12)
  
  
  #Realiza as previsões 
  pred<- (modelo_injetada$pred)
  
  index <- dates.teste[(loop:n.teste)]
  
  predss <- xts(pred,
                order.by = index)
  
  forecasts_injetada[loop,] <- c(as.matrix(predss), rep (NA, loop-1))
}

modelo_injetada


#Salva as previsões
save_path_injetada <-'K://DPCL//_Comum//Projeção de Energia Injetada//previsoes_injetada//sarma12_injetada'


write.zoo((forecasts_injetada),save_path_injetada)














