##ImportaÃ§Ã£o####
dados <- read.csv("Serie Historica 2014 - Evasao SESI.csv",header=TRUE, sep=";", dec ="," )
str(dados)
head(dados,5)

#***********************************************************************************************
#***********************************************************************************************
#********************************MODELAGEM PARA EDUCAÃÃO INFANTIL*******************************
#***********************************************************************************************
#***********************************************************************************************

##TransformaÃ§Ã£o das Regionais em Serie Temporal####
ts_rs_inf<- ts(dados$rs_inf, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr1_inf<- ts(dados$rr1_inf, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr2_inf<- ts(dados$rr2_inf, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rnn_inf<- ts(dados$rnn_inf, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rgr_inf<- ts(dados$rgr_inf, start=c(2014,1), end=c(2018,6), frequency=12)

plot(ts_rs_inf)
plot(ts_rr1_inf)
plot(ts_rr2_inf)
plot(ts_rnn_inf)
plot(ts_rgr_inf)


fit_rs_inf <- HoltWinters(ts_rs_inf, seasonal="add")
plot(fit_rs_inf)

fit_rr1_inf <- HoltWinters(ts_rr1_inf, seasonal="add")
plot(fit_rr1_inf)

fit_rr2_inf <- HoltWinters(ts_rr2_inf, seasonal="add")
plot(fit_rr2_inf)

fit_rnn_inf <- HoltWinters(ts_rnn_inf, seasonal="add")
plot(fit_rnn_inf)

fit_rgr_inf <- HoltWinters(ts_rgr_inf, seasonal="add")
plot(fit_rgr_inf)


library(forecast)


#########Infantil Regional Sul
fore= forecast(fit_rs_inf,18)
fore_rs_inf<- ts(fore$upper[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_inf)

for (i in 1:18){
  if (fore_rs_inf[i]<0.006) (fore_rs_inf[i]=0)
}

autoplot(ts(cbind(ts_rs_inf,fore_rs_inf), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########Infantil Regional Rio 1
fore= forecast(fit_rr1_inf,18)
fore_rr1_inf<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_inf)

for (i in 1:18){
if (fore_rr1_inf[i]<0) (fore_rr1_inf[i]=0)
}
fore_rr1_inf[9]=0

autoplot(ts(cbind(ts_rr1_inf,fore_rr1_inf), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

######################Infantil Regional Rio 2 NÃ£o tem dados##########################

#########Infantil Regional Norte-Noroeste
fore= forecast(fit_rnn_inf,18)
fore_rnn_inf<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rnn_inf)

for (i in 1:18){
  if (fore_rnn_inf[i]<0.003) (fore_rnn_inf[i]=0)
}

autoplot(ts(cbind(ts_rnn_inf,fore_rnn_inf), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########Infantil Regional Grande Rio
fore= forecast(fit_rgr_inf,18)
fore_rgr_inf<- ts(fore$upper[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rgr_inf)

for (i in 1:18){
  if (fore_rgr_inf[i]<0.007) (fore_rgr_inf[i]=0)
}

autoplot(ts(cbind(ts_rgr_inf,fore_rgr_inf), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#***********************************************************************************************
#***********************************************************************************************


#***********************************************************************************************
#***********************************************************************************************
#************************MODELAGEM PARA EDUCAÃÃO FUNDAMENTAL 1S A 4S****************************
#***********************************************************************************************
#***********************************************************************************************


##TransformaÃ§Ã£o das Regionais em Serie Temporal####
ts_rs_fun1<- ts(dados$rs_fun1, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr1_fun1<- ts(dados$rr1_fun1, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr2_fun1<- ts(dados$rr2_fun1, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rnn_fun1<- ts(dados$rnn_fun1, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rgr_fun1<- ts(dados$rgr_fun1, start=c(2014,1), end=c(2018,6), frequency=12)

plot(ts_rs_fun1)
plot(ts_rr1_fun1)
plot(ts_rr2_fun1)
plot(ts_rnn_fun1)
plot(ts_rgr_fun1)


fit_rs_fun1 <- HoltWinters(ts_rs_fun1, seasonal="add")
plot(fit_rs_fun1)

fit_rr1_fun1 <- HoltWinters(ts_rr1_fun1, seasonal="add")
plot(fit_rr1_fun1)

fit_rr2_fun1 <- HoltWinters(ts_rr2_fun1, seasonal="add")
plot(fit_rr2_fun1)

fit_rnn_fun1 <- HoltWinters(ts_rnn_fun1, seasonal="add")
plot(fit_rnn_fun1)

fit_rgr_fun1 <- HoltWinters(ts_rgr_fun1, seasonal="add")
plot(fit_rgr_fun1)


library(forecast)


#########FUNDAMENTAL 1S A 4S Regional Sul
fore= forecast(fit_rs_fun1,18)
fore_rs_fun1<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_fun1)

for (i in 1:18){
  if (fore_rs_fun1[i]<0.005) (fore_rs_fun1[i]=0)
}



autoplot(ts(cbind(ts_rs_fun1,fore_rs_fun1), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########FUNDAMENTAL 1S A 4S Regional Rio 1
fit_rr1_fun1 <- ets(ts_rr1_fun1,model="ZAA")

fore= forecast(fit_rr1_fun1,18)
fore_rr1_fun1<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_fun1)

for (i in 1:18){
  if (fore_rr1_fun1[i]<0.002) (fore_rr1_fun1[i]=0)
}

autoplot(ts(cbind(ts_rr1_fun1,fore_rr1_fun1), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

######################FUNDAMENTAL 1S A 4S Regional Rio 2 NÃ£o tem historico##########################


#########FUNDAMENTAL 1S A 4S Regional Norte-Noroeste
fore= forecast(fit_rnn_fun1,18)
fore_rnn_fun1<- ts(fore$upper[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rnn_fun1)

for (i in 1:18){
  if (fore_rnn_fun1[i]<0.0035) (fore_rnn_fun1[i]=0)
}

fore_rnn_fun1[17]=0
fore_rnn_fun1[16]=0
fore_rnn_fun1[14]=0
fore_rnn_fun1[13]=0

autoplot(ts(cbind(ts_rnn_fun1,fore_rnn_fun1), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########FUNDAMENTAL 1S A 4S Regional Grande Rio
fore= forecast(fit_rgr_fun1,18)
fore_rgr_fun1<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)

for (i in 1:18){
  if (fore_rgr_fun1[i]<0.002) (fore_rgr_fun1[i]=0)
}

plot(fore_rgr_fun1)
autoplot(ts(cbind(ts_rgr_fun1,fore_rgr_fun1), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#***********************************************************************************************
#***********************************************************************************************

#***********************************************************************************************
#***********************************************************************************************
#************************MODELAGEM PARA EDUCAÃÃO FUNDAMENTAL 5S A 8S****************************
#***********************************************************************************************
#***********************************************************************************************


##TransformaÃ§Ã£o das Regionais em Serie Temporal####
ts_rs_fun2<- ts(dados$rs_fun2, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr1_fun2<- ts(dados$rr1_fun2, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr2_fun2<- ts(dados$rr2_fun2, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rnn_fun2<- ts(dados$rnn_fun2, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rgr_fun2<- ts(dados$rgr_fun2, start=c(2014,1), end=c(2018,6), frequency=12)

plot(ts_rs_fun2)
plot(ts_rr1_fun2)
plot(ts_rr2_fun2)
plot(ts_rnn_fun2)
plot(ts_rgr_fun2)


fit_rs_fun2 <- HoltWinters(ts_rs_fun2, seasonal="add")
plot(fit_rs_fun2)

fit_rr1_fun2 <- HoltWinters(ts_rr1_fun2, seasonal="add")
plot(fit_rr1_fun2)

fit_rr2_fun2 <- HoltWinters(ts_rr2_fun2, seasonal="add")
plot(fit_rr2_fun2)

fit_rnn_fun2 <- HoltWinters(ts_rnn_fun2, seasonal="add")
plot(fit_rnn_fun2)

fit_rgr_fun2 <- HoltWinters(ts_rgr_fun2, seasonal="add")
plot(fit_rgr_fun2)


library(forecast)

#########FUNDAMENTAL 5S A 8S Regional Sul Re modelagem para Exponential Smoothing State Space Model
#########Considerando sazonalidade aditiva

fit_rs_fun2 <- ets(ts_rs_fun2,model="ZZA")
plot(fit_rs_fun2)

#########FUNDAMENTAL 5S A 8S Regional Sul
fore= forecast.ets(fit_rs_fun2,18)
fore_rs_fun2<- ts(fore$upper[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_fun2)

for (i in 1:18){
  if (fore_rs_fun2[i]<0.0038) (fore_rs_fun2[i]=0)
}

autoplot(ts(cbind(ts_rs_fun2,fore_rs_fun2), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########FUNDAMENTAL 5S A 8S Regional Rio 1
fore= forecast(fit_rr1_fun2,18)
fore_rr1_fun2<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_fun2)

for (i in 1:18){
  if (fore_rr1_fun2[i]<0.0025) (fore_rr1_fun2[i]=0)
}
fore_rr1_fun2[8]=0
fore_rr1_fun2[14]=0

autoplot(ts(cbind(ts_rr1_fun2,fore_rr1_fun2), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

######################FUNDAMENTAL 5S A 8S Regional Rio 2 NÃ£o tem dados consistente##########################


#########FUNDAMENTAL 5S A 8S Regional Norte-Noroeste

#########FUNDAMENTAL 5S A 8S Regional Norte-NoroesteRe modelagem para Exponential Smoothing State Space Model
#########Considerando sazonalidade aditiva

fit_rnn_fun2 <- ets(ts_rnn_fun2,model="ZZA")
plot(fit_rnn_fun2)

fore= forecast(fit_rnn_fun2,18)
fore_rnn_fun2<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rnn_fun2)

for (i in 1:18){
  if (fore_rnn_fun2[i]<0.0025) (fore_rnn_fun2[i]=0)
}

autoplot(ts(cbind(ts_rnn_fun2,fore_rnn_fun2), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########FUNDAMENTAL 5S A 8S Regional Grande Rio
fit_rgr_fun2 <- ets(ts_rgr_fun2,model="ZZA")
plot(fit_rgr_fun2)


fore= forecast(fit_rgr_fun2,18)
fore_rgr_fun2<- ts(fore$upper[1:18,1],start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rgr_fun2)

for (i in 1:18){
  if (fore_rgr_fun2[i]<0.0045) (fore_rgr_fun2[i]=0)
}
autoplot(ts(cbind(ts_rgr_fun2,fore_rgr_fun2), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#***********************************************************************************************
#***********************************************************************************************

#***********************************************************************************************
#***********************************************************************************************
#************************MODELAGEM PARA EDUCAÃÃO ENSINO MÃDIO REGULAR***************************
#***********************************************************************************************
#***********************************************************************************************


##TransformaÃ§Ã£o das Regionais em Serie Temporal####
ts_rs_mr<- ts(dados$rs_mr, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr1_mr<- ts(dados$rr1_mr, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr2_mr<- ts(dados$rr2_mr, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rnn_mr<- ts(dados$rnn_mr, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rgr_mr<- ts(dados$rgr_mr, start=c(2014,1), end=c(2018,6), frequency=12)

plot(ts_rs_mr)
plot(ts_rr1_mr)
plot(ts_rr2_mr)
plot(ts_rnn_mr)
plot(ts_rgr_mr)



library(forecast)

#########ENSINO MÃDIO REGULAR Regional Sul Re modelagem para Exponential Smoothing State Space Model
#########Considerando sazonalidade aditiva

fit_rs_mr <- ets(ts_rs_mr,model="AAA")
plot(fit_rs_mr)


#########ENSINO MÃDIO REGULAR Regional Sul
fore= forecast(fit_rs_mr,18,level = c(50,65,90,95),bootstrap = TRUE, allow.multiplicative.trend=TRUE)
fore_rs_mr<- ts(fore$upper[1:18,2], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_mr)

for (i in 1:18){
  if (fore_rs_mr[i]<0.008) (fore_rs_mr[i]=0)
}

fore_rs_mr[13]=0
fore_rs_mr[14]=0
fore_rs_mr[8]=0.00638
autoplot(ts(cbind(ts_rs_mr,fore_rs_mr), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#########ENSINO MÃDIO REGULAR Regional Grande Rio
fit_rgr_mr <- ets(ts_rgr_mr,model="AAA")
plot(fit_rgr_mr)

fore= forecast(fit_rgr_mr,18,level = c(50,65,90,95),bootstrap = TRUE, allow.multiplicative.trend=TRUE)
fore_rgr_mr<- ts(fore$upper[1:18,2], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rgr_mr)

for (i in 1:18){
  if (fore_rgr_mr[i]<0.004) (fore_rgr_mr[i]=0)
}

fore_rgr_mr[8]=0
fore_rgr_mr[10]=0
autoplot(ts(cbind(ts_rgr_mr,fore_rgr_mr), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#***********************************************************************************************
#***********************************************************************************************

#***********************************************************************************************
#***********************************************************************************************
#**********************MODELAGEM PARA EDUCAÃÃO ENSINO MÃDIO ARTICULADO**************************
#***********************************************************************************************
#***********************************************************************************************

##TransformaÃ§Ã£o das Regionais em Serie Temporal####
ts_rs_ma<- ts(dados$rs_ma, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr1_ma<- ts(dados$rr1_ma, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rr2_ma<- ts(dados$rr2_ma, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rnn_ma<- ts(dados$rnn_ma, start=c(2014,1), end=c(2018,6), frequency=12)
ts_rgr_ma<- ts(dados$rgr_ma, start=c(2014,1), end=c(2018,6), frequency=12)

plot(ts_rs_ma)
plot(ts_rr1_ma)
plot(ts_rr2_ma)
plot(ts_rnn_ma)
plot(ts_rgr_ma)


fit_rs_ma <- HoltWinters(ts_rs_ma, seasonal="add")
plot(fit_rs_ma)

fit_rr1_ma <- HoltWinters(ts_rr1_ma, seasonal="add")
plot(fit_rr1_ma)

fit_rr2_ma <- HoltWinters(ts_rr2_ma, seasonal="add")
plot(fit_rr2_ma)

fit_rnn_ma <- HoltWinters(ts_rnn_ma, seasonal="add")
plot(fit_rnn_ma)

fit_rgr_ma <- HoltWinters(ts_rgr_ma, seasonal="add")
plot(fit_rgr_ma)


library(forecast)

#########FUNDAMENTAL 5S A 8S Regional Sul Re modelagem para Exponential Smoothing State Space Model
#########Considerando sazonalidade aditiva

fit_rs_ma <- ets(ts_rs_ma,model="ZZA")
plot(fit_rs_ma)

#########ENSINO MÃDIO ARTICULADO Regional Sul
fore= forecast.ets(fit_rs_ma,18)
fore_rs_ma<- ts(fore$upper[1:18,2], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rs_ma)

for (i in 1:18){
  if (fore_rs_ma[i]<0.024) (fore_rs_ma[i]=0)
}

fore_rs_ma[8]=0
fore_rs_ma[10]=0
autoplot(ts(cbind(ts_rs_ma,fore_rs_ma), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########ENSINO MÃDIO ARTICULADO Regional Rio 1
fit_rr1_ma <- ets(ts_rr1_ma,model="ZAA")
plot(fit_rr1_ma)

fore= forecast(fit_rr1_ma,18)
fore_rr1_ma<- ts(fore$mean, start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rr1_ma)

for (i in 1:18){
  if (fore_rr1_ma[i]<0.002) (fore_rr1_ma[i]=0)
}

autoplot(ts(cbind(ts_rr1_ma,fore_rr1_ma), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

######################ENSINO MÃDIO ARTICULADO Regional Rio 2##########################
fit_rr2_ma <- HoltWinters(ts_rr2_ma,seasonal="add")
plot(fit_rr2_ma)

fore= forecast(fit_rr2_ma,18,level = c(50,65,90,95),bootstrap = TRUE, allow.multiplicative.trend=TRUE)
fore_rr2_ma<- ts(fore$lower[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rr2_ma)

for (i in 1:18){
  if (fore_rr2_ma[i]<0.005) (fore_rr2_ma[i]=0)
}

fore_rr2_ma[14]=0

autoplot(ts(cbind(ts_rr2_ma,fore_rr2_ma), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#########ENSINO MÃDIO ARTICULADO  Regional Norte-Noroeste

fit_rnn_ma <- ets(ts_rnn_ma,model="ZZA")
plot(fit_rnn_ma)

fore= forecast(fit_rnn_ma,18)
fore_rnn_ma<- ts(fore$upper[1:18,1], start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rnn_ma)
for (i in 1:18){
  if (fore_rnn_ma[i]<0.015) (fore_rnn_ma[i]=0)
}

fore_rnn_ma[12]=0
fore_rnn_ma[9]=0

autoplot(ts(cbind(ts_rnn_ma,fore_rnn_ma), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)


#########ENSINO MÃDIO ARTICULADO  Regional Grande Rio
fit_rgr_ma <- ets(ts_rgr_ma,model="ZZA")
plot(fit_rgr_ma)


fore= forecast(fit_rgr_ma,18)
fore_rgr_ma<- ts(fore$mean,start=c(2018,7), end=c(2019,12), frequency=12)
plot(fore_rgr_ma)

for (i in 1:18){
  if (fore_rgr_ma[i]<0.0057) (fore_rgr_ma[i]=0)
}
autoplot(ts(cbind(ts_rgr_ma,fore_rgr_ma), start=c(2014,1), end=c(2019,12), frequency=12), facets=FALSE)

#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************
#******************************                          ***************************************
#******************************ESTRUTURAÃÃO PARA EXPORTAR***************************************
#******************************                          ***************************************
#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************

bd_fore_inf=ts(cbind(fore_rs_inf,fore_rr1_inf,fore_rnn_inf,fore_rgr_inf),start=c(2018,7),
               end=c(2019,12), frequency=12)

bd_fore_fun1=ts(cbind(fore_rs_fun1,fore_rr1_fun1,fore_rnn_fun1,fore_rgr_fun1),start=c(2018,7),
                end=c(2019,12), frequency=12)

bd_fore_fun2=ts(cbind(fore_rs_fun2,fore_rr1_fun2,fore_rnn_fun2,fore_rgr_fun2),start=c(2018,7),
                end=c(2019,12), frequency=12)

bd_fore_mr=ts(cbind(fore_rs_mr, fore_rgr_mr),start=c(2018,7),
              end=c(2019,12), frequency=12)

bd_fore_ma=ts(cbind(fore_rs_ma,fore_rr1_ma,fore_rr2_ma,fore_rnn_ma,fore_rgr_ma),start=c(2018,7),
              end=c(2019,12), frequency=12)



write.table(bd_fore_inf, "Z:/NÃºcleo de InformaÃ§Ãµes/BI/FIRJAN/Rafael Saraiva/Forecasting/dados modelados/bd_fore_inf.csv",sep=" ",dec=",",row.names =TRUE, col.names=TRUE )
write.table(bd_fore_fun1, "Z:/NÃºcleo de InformaÃ§Ãµes/BI/FIRJAN/Rafael Saraiva/Forecasting/dados modelados/bd_fore_fun1.csv",sep=" ",dec=",",row.names =TRUE, col.names=TRUE )
write.table(bd_fore_fun2, "Z:/NÃºcleo de InformaÃ§Ãµes/BI/FIRJAN/Rafael Saraiva/Forecasting/dados modelados/bd_fore_fun2.csv",sep=" ",dec=",",row.names =TRUE, col.names=TRUE )
write.table(bd_fore_mr, "Z:/NÃºcleo de InformaÃ§Ãµes/BI/FIRJAN/Rafael Saraiva/Forecasting/dados modelados/bd_fore_mr.csv",sep=" ",dec=",",row.names =TRUE, col.names=TRUE )
write.table(bd_fore_ma, "Z:/NÃºcleo de InformaÃ§Ãµes/BI/FIRJAN/Rafael Saraiva/Forecasting/dados modelados/bd_fore_ma.csv",sep=" ",dec=",",row.names =TRUE, col.names=TRUE )
