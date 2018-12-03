require(openxlsx)
require(xts)
require(forecast)
require(sarima)
require(astsa)
require(fBasics)
require(ggplot2)


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

##############################################################################
#####################DEFINE OS PERÍODOS DA SIMULAÇÃO##########################
##############################################################################

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


##############################################################################
###########MONTA AS MATRIZES QUE SERÃO UTILIZADAS NA AVALIAÇÃO################
##############################################################################

#Verifica a verdadeira realização
feedback_injetada <- serie[dates.teste,'series']
modelos <- c(#'integrado',
             #'ar',
             #''sarma12',
             #sarima',
             'sarima_temp',
             #'ssarima',
             #'ssarima_temp',
             #'temp_econo2',
             #'sarima_precipit',
             #''sarima_temp_precipit',
             #'sarima_econo',
             #''sarima_temp_econo',
             #'holt_winters',
             #'ces',
             'gum',
             'gum_reg'
             #'ets',
             #'sarima_temp_precipit_econo',
             #'sarima_garch'
             )


mercados <- 'injetada'

#Cria a matriz resposta
feedback_full_injetada <- matrix(0, 
                                 nrow = n.teste, 
                                 ncol = n.teste)

for(feed in 1:n.teste){
  #Elimina o passado
  retirada <- lag(feedback_injetada,-feed+1)
  retirada <- na.omit(retirada)
  
  #monta o vetor complementar
  nas <- rep(NA,feed-1)
  
  feedback_full_injetada[feed,] <- c(as.vector(retirada),nas)
}

feedback_full_cumsum_injetada <- feedback_full_injetada * 0

for(feed in 1:n.teste){
  feedback_full_cumsum_injetada[feed,] <- cumsum(feedback_full_injetada[feed,])
}



erro_medio_injetada <- matrix(ncol = n.modelos,
                              nrow = n.teste)

colnames(erro_medio_injetada) <- modelos
row.names(erro_medio_injetada) <- paste("horizonte",1:n.teste,sep = "_")

erro_medio_acum_injetada <- erro_medio_injetada
erro_mediana_acum_injetada <- erro_medio_injetada
erro_var_acum_injetada <- erro_medio_injetada
erro_skw_acum_injetada <- erro_medio_injetada
erro_kurt_acum_injetada <- erro_medio_injetada
erro_25_acum_injetada <- erro_medio_injetada
erro_75_acum_injetada <- erro_medio_injetada
erro_5_acum_injetada <- erro_medio_injetada
erro_95_acum_injetada <- erro_medio_injetada
erro_inter90_acum_injetada <- erro_medio_injetada
erro_inter50_acum_injetada <- erro_medio_injetada
erro_min_acum_injetada <- erro_medio_injetada
erro_max_acum_injetada <- erro_medio_injetada



#Cria o objeto que guardará as previsões dos modelos
previsoes <- array(NA, dim = c(n.teste,n.teste,n.modelos))
previsoes_cumsum <- array(NA, dim = c(n.teste,n.teste,n.modelos))

#############################################################
##############RECUPERA AS PREVISÕES DOS MODELOS##############
#############################################################


for(teste_models in 1:n.modelos){
  modelo <- modelos[teste_models]
  
  paths <- paste('K://DPCL//_Comum//Projeção de Energia Injetada//previsoes_injetada//',
                 paste(modelo,mercados,sep = "_"),sep="")
  
  previsoes_injetada <- read.zoo(paths[1],
                                 format = "%Y-%m-%d",
                                 header = TRUE)
  
  previsoes[,,teste_models] <- previsoes_injetada
  
  
  previsoes_cumsum_injetada <- previsoes_injetada * 0 
  
  for(feed in 1:nrow(previsoes_cumsum_injetada)){
    previsoes_cumsum_injetada[feed,] <- cumsum(as.matrix(previsoes_injetada[feed,]))
  }
  
  
  previsoes_cumsum[,,teste_models] <- previsoes_cumsum_injetada
}

###########################################################
##############AVALIA AS PREVISÕES DOS MODELOS##############
###########################################################

erro_injetada <- array(NA, dim = c(n.teste,n.teste,n.modelos),
                       dimnames = list(dates.teste,paste('Passo',1:n.teste),modelos))
erro_acum_injetada <- erro_injetada



for(teste_models in 1:n.modelos){
  
  previsoes_injetada <- previsoes[,,teste_models]
  previsoes_cumsum_injetada <- previsoes_cumsum[,,teste_models]
  
  erro_injetada[,,teste_models] <- (previsoes_injetada - feedback_full_injetada)/feedback_full_injetada
  
  erro_acum_injetada[,,teste_models] <- (previsoes_cumsum_injetada - feedback_full_cumsum_injetada)/feedback_full_cumsum_injetada
  
  tabela <- basicStats(erro_acum_injetada[,,teste_models])
  quants <- c(0.05,0.95)
  perc_9 <- apply( erro_acum_injetada , 2 , quantile , probs = quants , na.rm = TRUE )
  
  erro_medio_acum_injetada[,teste_models] <- unlist(tabela[7,])
  erro_mediana_acum_injetada[,teste_models] <- unlist(tabela[8,])
  erro_var_acum_injetada[,teste_models] <- unlist(tabela[13,])
  erro_skw_acum_injetada[,teste_models] <-  unlist(tabela[14,])
  erro_kurt_acum_injetada[,teste_models] <-  unlist(tabela[15,])
  erro_25_acum_injetada[,teste_models] <- unlist(tabela[5,])
  erro_75_acum_injetada[,teste_models] <- unlist(tabela[6,])
  erro_inter50_acum_injetada[,teste_models] <- unlist(tabela[6,])-unlist(tabela[5,])
  erro_5_acum_injetada[,teste_models] <- perc_9[1,]
  erro_95_acum_injetada[,teste_models] <- perc_9[2,]
  erro_inter90_acum_injetada[,teste_models] <- perc_9[2,]-perc_9[1,]
  
  erro_min_acum_injetada[,teste_models] <- unlist(tabela[3,])
  erro_max_acum_injetada[,teste_models] <- unlist(tabela[4,])
  
  
}


#autoplot(as.ts(erro_5_acum_injetada))

################################################
######ANÁLISE ERRO 12 PERÍODOS A FRENTE#########
################################################

#Cria as matrizes a serem utilizadas na análise
horizonte_previsao <- 18
plot_matrix <- na.omit(erro_acum_injetada[,horizonte_previsao,])
colnames(plot_matrix) <- modelos
n.forecasts <- nrow(plot_matrix)
kill <- n.teste - n.forecasts

df.m <- reshape2::melt(plot_matrix, id.vars = NULL)
df.m.reshaped <- df.m[,-1]

plot_matrix <- xts(plot_matrix,
                   order.by = dates.teste[(1):n.forecasts])


#Plota como os modelos erraram ao longo do tempo
plot(plot_matrix,
     legend.loc = TRUE)

#Plota como os modelos erraram ao longo do tempo
ggplot(df.m.reshaped, aes(x = Var2, y = value)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(title="Errors Distribution",x="Models", y = "Errors")+
  geom_boxplot(width=0.1)+
  theme_classic()

#Faz um histograma da distribuição dos erros
ggplot(df.m, aes(x=value))+
  geom_density(alpha=.2, 
               fill="#FF6666")+
  geom_histogram(color="black", 
                 fill="white",
                 bins = 20)+
  facet_grid(Var2 ~ .)

