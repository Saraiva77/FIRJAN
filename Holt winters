## Elaboração do modelo de Holt Winters
##A série possui sazonalidade.
## A série não é estacionário em dif=0 e é estacionária em dif=1

#Ajustando modelo de HoltWinters multiplicativo e sazional
fit <- HoltWinters(tx_ts, seasonal="mult")
plot(fit)

library(forecast)

fore_fit <-forecast(fit,h=11,level = c(90, 95))
fore_fit
plot(fit,fore_fit)

#Forecasting Holt Winters
library(forecast)

fore_fit <-forecast(fit,h=12,level =90,allow.multiplicative.trend=TRUE )

#Quando faz o grafico ele fica distorcido devido as valores do lower e upper
plot(fore_fit)

#armezana os valores em um novo objeto time series
fore_ts=fore_fit$mean

#Colocar o objeto time series no modelo holt winters e plotar junto com serie historica
