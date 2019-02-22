library(ggplot2)
library(gridExtra)
library(e1071)
library(rpart)
library(caret)
library(tidyr)
library(randomForest)

setwd("Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/score")
dadost= as.data.frame(read.csv("totaldata.csv", sep=";",dec=",", header =  TRUE, stringsAsFactors = FALSE))

write.table(dadost,"total.csv", row.names = FALSE,col.names = TRUE, sep = ",")
dados= as.data.frame(read.csv("total.csv", sep=",",dec=".", header =  TRUE, stringsAsFactors = FALSE))

str(dados)

dados$dias30 <- factor(dados$dias30 )
dados$rendaNUM <- factor(dados$rendaNUM )
dados$faixa_idade <- factor(dados$faixa_idade)
dados$mensal <- factor(dados$mensal )
dados$pessoasNUM <- factor(dados$pessoasNUM )
dados$setorNUM <- factor(dados$setorNUM )
dados$mensalNUM <- factor(dados$mensalNUM )


dados.filtro = dados[,c("dias30","rendaNUM","faixa_idade","mensal","pessoasNUM","setorNUM","mensalNUM")]

roww <- nrow(dados.filtro)
coll <- ncol(dados.filtro)
numTrain <- floor((9/10) * roww)
numTest <- roww - numTrain
training <- dados.filtro[sample(roww, numTrain), ]
test <- dados.filtro[sample(roww, numTest), ]





train.rf = randomForest(as.factor(dias30) ~ rendaNUM + mensalNUM+ pessoasNUM + setorNUM
                        + faixa_idade, data=training,ntree=500,maxnodes=650,nPerm =10,do.trace=TRUE , importance=FALSE,keep.inbag=TRUE)

train.rf