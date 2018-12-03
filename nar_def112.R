require(openxlsx)
require(xts)
require(forecast)
library(neuralnet)

#Carrega os dados
destino <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\energia_injetada.xlsx'
series <- openxlsx::read.xlsx(destino,
                              detectDates = TRUE)

series <- xts (series[,-1],
               order.by = series[,1])
series <- (series)

demanda  <- series
demanda.1 <- lag(series)
demanda.12 <- lag(series,12)
demanda.13 <- lag(series,13)

dif_12_13 <- demanda.12 - demanda.13


series_estac <- (demanda-demanda.1)-(demanda.12-demanda.13)

#carrega serie de temperatura
destino_temp <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\temperatura.xlsx'
temp <- openxlsx::read.xlsx(destino_temp,
                            detectDates = TRUE)

temp <- xts (temp[,-1],
             order.by = temp[,1])

temp_estac <- temp [,'media']
temp_estac <- diff(temp_estac,12)

#carrega serie de precipitacao
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\precipitacao.xlsx'
prec <- openxlsx::read.xlsx(destino_prec,
                            detectDates = TRUE)

prec <- xts (prec[,-1],
             order.by = prec[,1])

prec_estac <- prec
colnames(prec_estac) <- 'chuva'
prec_estac <- diff(prec_estac,12)

#carrega serie pim
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\pim.xlsx'
pim <- openxlsx::read.xlsx(destino_prec,
                              detectDates = TRUE)

pim <- xts (pim[,-1],
             order.by = pim[,1])
pim <- diff(pim)

#carrega serie pcm
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\pcm.xlsx'
pcm <- openxlsx::read.xlsx(destino_prec,
                           detectDates = TRUE)

pcm <- xts (pcm[,-1],
            order.by = pcm[,1])

pcm <- diff(pcm)
pcm <- diff(pcm,12)


#carrega serie pcm
destino_prec <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\INF_IND.xlsx'
inf_ind <- openxlsx::read.xlsx(destino_prec,
                           detectDates = TRUE)

inf_ind <- xts (inf_ind[,-1],
            order.by = inf_ind[,1])
inf_ind <- diff(inf_ind)


#junta as duas series
serie <- merge.xts(series_estac,lag(series_estac,1),
                   join = 'inner')
serie <- merge.xts(serie,lag(series_estac,12),
                   join = 'inner')
serie <- merge.xts(serie,temp_estac,
                   join = 'inner')
serie <- merge.xts(serie,prec_estac,
                   join = 'inner')
serie <- merge.xts(serie,pim,
                   join = 'inner')
serie <- merge.xts(serie,inf_ind,
                   join = 'inner')
serie <- merge.xts(serie,pcm,
                   join = 'inner')

serie <- na.omit(serie)

colnames(serie) <- c('demanda','demanda.1','demanda.12',
                     'temperatura','chuva','pim','inf_ind','pcm')

#Define a simulação
share_train <- 0.595 #Usaremos 69% da amostra pois isso gera 3 anos de previsões

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
  
  ###############################
  ######Seleciona a amostra######
  ###############################
  last_train <- (n.train-1+loop)
  n.forecasts <- (n.teste-loop+1)
  
  passado <- serie[loop:last_train,]
  futuro <- serie[(last_train+1):(last_train+n.forecasts),]
  
  dates_train <- index(passado)
  dates_test <- index(futuro)
  
  
  ###############################
  ########Escala a amostra#######
  ###############################
  
  maxs <- apply(passado, 2, max) 
  mins <- apply(passado, 2, min)
  
  maxs_matrix <- t(matrix(as.vector(maxs), ncol = nrow(passado), nrow = length(maxs)))
  mins_matrix <- t(matrix(as.vector(mins), ncol = nrow(passado), nrow = length(mins)))
  
  scaled <- (passado - mins_matrix) / (maxs_matrix - mins_matrix)
  scaled <- as.data.frame(scaled)
  scaled_futuro <- as.data.frame(scale(futuro, center = mins, scale = maxs - mins))
  
  ###############################
  ########Cria a formula#########
  ###############################
  n <- names(scaled)
  target <- n[1]
  regressors <- n[!n %in% target]
  n.regressors <- length(regressors)
  f <- as.formula(paste(target, " ~ ", paste(regressors, collapse = " + ")))
  
  
  ###############################
  #######Estrutura da NN#########
  ###############################
  hiddens <- 5:1
  
  
  ###############################
  #########Estima a NN###########
  ###############################
  nn <- neuralnet(f,data=scaled,
                  hidden=hiddens,
                  linear.output=T,
                  rep = 5e+02)
  
    #############################################################
  #########Cria a matriz onde ficarão os regressores###########
  #############################################################
  regressores <- matrix(NA, 
                        nrow = n.forecasts,
                        ncol = n.regressors)
  regressores <- xts(regressores,
                     order.by = dates.teste)
  predictions <- regressores[,1]
  colnames(regressores) <- regressors
  
  ext_regressors <- regressors[!grepl('demanda',regressors)]
  regressores[,ext_regressors] <- as.matrix(scaled_futuro[,ext_regressors])
  regressores[1,'demanda.1'] <- last(passado$demanda)
  regressores[1:12,'demanda.12'] <- tail(passado$demanda,12)
  
  
  ###################################
  ##########Prevê com a NN###########
  ###################################
  for(i in 1:n.forecasts){
    forecasts <- compute(nn,na.omit(regressores))$net.result
    ult_period <- last(forecasts)
    predictions[i,] <- ult_period
    if(i < (n.forecasts-11)){
      regressores[i+1,1] <- ult_period
      regressores[i+12,2] <- ult_period
    } else if(i < (n.forecasts)) {
      regressores[i+1,1] <- ult_period
    }
  }
  
    #plot(as.ts(cbind(forecasts,scaled_futuro$demanda)))
  
  ##################################################
  #########RETIRA AS ESCALA DAS VARIÁVEIS###########
  ##################################################
  predictions_rescaled <- predictions * (maxs_matrix[1] - mins_matrix[1]) + mins_matrix[1]
  
  
  #####################################################################
  #########Cria a Matriz para recuperar as previsões em nível###########
  #####################################################################
  recuperar_nível <-cbind(predictions,
                          predictions*0,
                          predictions*0
                          )
  colnames(recuperar_nível) <- c('nn_forecast','diff_12_13','lag_1')
  recuperar_nível[1,'lag_1'] <- demanda.1[last(dates_train),]
  recuperar_nível[1:12,'diff_12_13'] <- dif_12_13[tail(dates_train,12),]
  
  recovered <- predictions*0
  
  #####################################################################
  #####################Recupera as previsões em nível#################
  #####################################################################
  for(i in 1:n.forecasts){
    recov <- sum(recuperar_nível[i,])
    
    if(i < 67){
      recovered[i] <- recov
      recuperar_nível[i+1,'lag_1'] <- recov
      recuperar_nível[i+12,'diff_12_13'] <-  recuperar_nível[i,'lag_1'] - recov 
    }else if(i < 78){
      recovered[i] <- recov
      recuperar_nível[i+1,'lag_1'] <- recov
    } else {
      recovered[i] <- recov
    }
  }
  
  
  
  forecasts_injetada[loop,] <- c(as.matrix(predictions), rep (NA, loop-1))
}


plot(rbind(series[dates_train],recovered))
#Recupera as previsões em nível




predicted_recovered <- pr.nn_ + (demanda.12 - demanda.13) - demanda.1


demanda  <- series
demanda.1 <- lag(series)
demanda.12 <- lag(series,12)
demanda.13 <- lag(series,13)