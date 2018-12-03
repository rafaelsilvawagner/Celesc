require(openxlsx)
require(xts)
require(forecast)
require(sarima)
require(tsDyn)


#Carrega os dados
destino <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\energia_injetada.xlsx'
series <- openxlsx::read.xlsx(destino,
                              detectDates = TRUE)

series <- xts (series[,-1],
               order.by = series[,1])
series <- log(series)

series_estac_1 <- diff(series)
series_estac_12_1 <- (diff(series_estac_1,12))
colnames(series_estac_12_1) <- 'energia_injetada'
#series_estac_12_1 + lag(series_estac_1,12) == series_estac_1

recuperar_1 <- lag(series,1)
recuperar_12 <- lag(series_estac_1,12)
#series_estac_12_1 + recuperar_12 + recuperar_1 == series


#carrega serie de temperatura
destino_temp <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\temperatura.xlsx'
temp <- openxlsx::read.xlsx(destino_temp,
                            detectDates = TRUE)

temp <- xts (temp[,-1],
             order.by = temp[,1])

temp_estac <- diff(temp,12)

#carrega serie de precipitacao
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\precipitacao.xlsx'
prec <- openxlsx::read.xlsx(destino_prec,
                            detectDates = TRUE)

prec <- xts (prec[,-1],
             order.by = prec[,1])

prec_estac <- diff(prec,12)
colnames(prec_estac) <- 'precipitacao'
#apesar de nenhum teste apontar integração de décima segunda ordem eu tomei ela pois meu coração mandou
#nsdiffs(ts(prec, c(2001,10),freq = 12),test = 'ch')

#junta as duas series
serie <- merge.xts(series_estac_12_1,temp_estac,
                   join = 'inner')
serie <- merge.xts(serie,prec_estac,
                   join = 'inner')
serie <- na.omit(serie)

#Define a simulação
share_train <- 0.69 #Usaremos 69% da amostra pois isso gera 3 anos de previsões

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
  
  #Estima o modelo
  modelo_injetada <- Arima(y = passado [,'energia_injetada'],
                           order = c(1,0,1), 
                           include.mean = FALSE,
                           seasonal = list(order = c(0,0,1), period = 12),
                           xreg = passado[,'precipitacao'],
                           method = c("CSS-ML")
  )
  
  
  
  #Monta um arma pra prever a queda de chuva
  modelo_prec <- Arima(y = passado[,'precipitacao'],
                       order = c(1,0,1), 
                       include.mean = FALSE,
                       seasonal = list(order = c(1,0,1), period = 12),
                       method = c("CSS-ML"))
  
  pred_prec <- predict(modelo_prec, 
                       n.forecasts)$pred
  
  #Realiza as previsões 
  pred<- predict(modelo_injetada, 
                 n.forecasts,
                 newxreg = pred_prec)
  
  index <- dates.teste[(loop:n.teste)]
  
  predss <- xts(pred$pred,
                order.by = index)
  
  
  
  pred_nivel <- predss + recuperar_12[index] + recuperar_1[index]
  
  
  forecasts_injetada[loop,] <- c(as.matrix(pred_nivel), rep (NA, loop-1))
}

modelo_injetada


#Salva as previsões
save_path_injetada <-'K://DPCL//_Comum//Projeção de Energia Injetada//previsoes_injetada//nnet_injetada'


write.zoo(exp(forecasts_injetada),save_path_injetada)














