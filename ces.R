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
serie <- na.omit(series)

#Define a simulação
share_train <- 0.80 #Usaremos 69% da amostra pois isso gera 3 anos de previsões

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
  passado <- serie[loop:last_train,]
 
  passado <- ts((passado), frequency = 12)
  passado_reg <- passado[,-1]
  
  
  
  #Realiza as previsões 
  model <- smooth::auto.ces(passado,
                            h = n.forecasts)

  pred <- model$forecast
  
  index <- dates.teste[(loop:n.teste)]
  
  predss <- xts(pred,
                order.by = index)
  
  forecasts_injetada[loop,] <- c(as.matrix(predss), rep (NA, loop-1))
}




#Salva as previsões
save_path_injetada <-'K://DPCL//_Comum//Projeção de Energia Injetada//previsoes_injetada_univariados//ces_injetada'


write.zoo((forecasts_injetada),save_path_injetada)
