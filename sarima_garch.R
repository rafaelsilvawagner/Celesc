require(openxlsx)
require(xts)
require(forecast)
require(sarima)
require(rugarch)
require(lubridate)

################################################
##########CARREGA OS DADOS E ESTACIONA##########
################################################

destino <- 'K:\\DPCL\\_Comum\\Projeção de Energia Injetada\\energia_injetada.xlsx'
series <- openxlsx::read.xlsx(destino,
                              detectDates = TRUE)
series <- xts (series[,-1],
               order.by = series[,1])

#Corta a amostra pela que está disponível para todos os outros modelos
#initial_all <- as.Date('2001/09/01')
#common_series <- (index(series)>=initial_all)
#series <- series[common_series]


serie_diff1 <- diff (series)
serie_diff12 <- diff (serie_diff1, 12 )
serie <- na.omit(serie_diff12)



################################################
###############DEFINE A SIMULAÇÃO###############
################################################

share_train <- 0.805 #Usaremos 0.805% da amostra pois isso gera 3 anos de previsões

n.obs <- nrow(serie)
n.train <- round(share_train * n.obs)
n.teste <- n.obs - n.train

index.obs <- 1:n.obs
index.train <- 1:n.train
index.teste <- (n.train + 1):n.obs

dates.obs <- index(serie)
dates.train <- dates.obs[index.train]
dates.teste <- dates.obs[index.teste]

################################################
###############DEFINE A SIMULAÇÃO###############
################################################

#Monta o local aonde guardar as previsões
forecasts_injetada <- matrix(0,
                             nrow = n.teste,
                             ncol = n.teste)
forecasts_injetada <- xts(forecasts_injetada,
                          order.by = index(serie)[index.teste-1])
colnames(forecasts_injetada) <- paste("Passo",1:n.teste, sep = "_")


#Pega a serie recuperada
series_recover_diff1 <- lag(serie_diff1, 12)

#Realiza as previsões
for(loop in 1:n.teste){
  
  ################################################
  ##########DEFINE AMOSTRA E PREVISÕES############
  ################################################
  
  #Define o que já é passado
  last_train <- (n.train-1+loop)
  passado <- serie[loop:last_train,]
  
  #Deixa a série menor
  scaless = 1e-03
  passados <- passado * scaless
  
  #Defasa o passado em 12 períodos
  passado_def12 <- lag(passados,12)
  
  #Verifica quais dias não tem lag correspondente
  exc <- is.na(passado_def12)
  
  #exclui dos frames as observações sem o correspondente passado
  passados <- passados[!exc]
  passado_def12 <- passado_def12[!exc]
  
  #Verifica o tamanho final da amostra
  sample_size <- sum(!exc)
  first_train <- first(index(passado_def12))
  last_train <- last(index(passado_def12))
 
  #Verifica quais os meses a serem previstos  
  n.forecasts <- (n.teste-loop+1)
  fut_dates <-   seq(last_train,
                    length=n.forecasts + 1,
                    by="1 month")
  fut_dates <- fut_dates[-1]
  
  ################################################
  ################ESTIMA O MODELO#################
  ################################################
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                           garchOrder = c(1, 1),
                                           variance.targeting = FALSE), 
                     
                     mean.model     = list(armaOrder = c(1, 1), 
                                           archm = FALSE,
                                           external.regressors = as.matrix(cbind(passado_def12)),
                                           include.mean = FALSE))
  
  
  garch_estimation <- ugarchfit(spec = spec, data = passados, 
                     solver.control = list(trace=0))
  

  
  
  #############################################################
  ################REALIZA AS PROJEÇÕES FUTURAS#################
  #############################################################
  forecasts <- rep(NA,
                   n.forecasts)
  
  for(i in 1:n.forecasts){
    if( i <= 12){
      ext_index <- (sample_size-11):(sample_size-12+i)
      external_reggg <- as.matrix(passado_def12[ext_index])
      
      pred  <- ugarchforecast(garch_estimation,
                                  n.ahead = i,
                                  external.forecasts = list(mregfor = external_reggg))
      forecasts[1:i] <- pred@forecast$seriesFor
    } else {
      external_reggg <- as.vector(tail(passado_def12,12))
      external_reggg <- c(external_reggg, head(forecasts,i-12))
      external_reggg <- as.matrix(external_reggg)
      
      pred  <- ugarchforecast(garch_estimation,
                              n.ahead = i,
                              external.forecasts = list(mregfor = external_reggg))
      forecasts[1:i] <- pred@forecast$seriesFor
    }
  }
  
  index <- dates.teste[(loop:n.teste)]
  predss <- xts(forecasts,
                order.by = index)
  pred_nivel_diff1_diff_12 <- predss/scaless
  
  

  
  ############################################################################
  ################RECUPERA AS PROJEÇÕES NA PRIMEIRA DIFERENÇA#################
  ############################################################################
  
  pred_nivel_diff <- pred_nivel_diff1_diff_12 * 0
  
  
  for(i in 1:n.forecasts){
    if( i <= 12){
      done_forecast <- pred_nivel_diff1_diff_12[i]
      
      date_forecast <- index(done_forecast)
      date_forecast_12before <- date_forecast %m-% months(12)
      
      last_12_months <- series_recover_diff1[date_forecast_12before]
      last_12_months <- as.numeric(last_12_months)
      
      forecast_rebuild <- last_12_months + done_forecast
      
      pred_nivel_diff[i] <- forecast_rebuild
      
    } else {
      done_forecast <- pred_nivel_diff1_diff_12[i]
      
      date_forecast <- index(done_forecast)
      date_forecast_12before <- date_forecast %m-% months(12)
      
      last_12_months <- pred_nivel_diff[date_forecast_12before]
      last_12_months <- as.numeric(last_12_months)
      
      forecast_rebuild <- last_12_months + done_forecast
      
      pred_nivel_diff[i] <- forecast_rebuild
      
    }
  }
  
  #plot(cbind((series_recover_diff1[fut_dates]),pred_nivel_diff),legend.loc = TRUE)
  
  
  ############################################################################
  ################RECUPERA AS PROJEÇÕES NA ESTRUTURA ORIGINAL#################
  ############################################################################
  
  pred_nivel <- pred_nivel_diff * 0
  
  
  for(i in 1:n.forecasts){
    if( i <= 1){
      done_forecast <- pred_nivel_diff[i]
      
      date_forecast <- index(done_forecast)
      date_forecast_1before <- date_forecast %m-% months(1)
      
      last_1_months <- series[date_forecast_1before]
      last_1_months <- as.numeric(last_1_months)
      
      forecast_rebuild <- last_1_months + done_forecast
      
      pred_nivel[i] <- forecast_rebuild
      
    } else {
      done_forecast <- pred_nivel_diff[i]
      
      date_forecast <- index(done_forecast)
      date_forecast_1before <- date_forecast %m-% months(1)
      
      last_1_months <- pred_nivel[date_forecast_1before]
      last_1_months <- as.numeric(last_1_months)
      
      forecast_rebuild <- last_1_months + done_forecast
      
      pred_nivel[i] <- forecast_rebuild
      
    }
  }
 
  
  #plot(cbind((series[fut_dates]),pred_nivel),legend.loc = TRUE)
  
  
  forecasts_injetada[loop,] <- c(as.matrix(pred_nivel), rep (NA, loop-1))
}


#Salva as previsões
save_path_injetada <-'K://DPCL//_Comum//Projeção de Energia Injetada//previsoes_injetada//sarima_garch_injetada'


write.zoo((forecasts_injetada),save_path_injetada)














