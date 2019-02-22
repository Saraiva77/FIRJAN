setwd("Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva")
library(caret)
library(scatterplot3d)
attach(mtcars)
library(gclus)

dados= as.data.frame(read.csv("DadosModeloRENDA.csv", sep=";",dec=",", header =  TRUE, stringsAsFactors = FALSE))

dados$Qtd.Dependentes <- factor(dados$Qtd.Dependentes )
dados$dias30 <- factor(dados$dias30 )
dados$dias60 <- factor(dados$dias60 )
dados$dias90 <- factor(dados$dias90 )
dados$SerNUM <- factor(dados$SerNUM)
dados$fax_idade <- factor(dados$fax_idade)
dados$fax_renda <- factor(dados$fax_renda)
dados$sexo <- factor(dados$sexo)
dados$estado <- factor(dados$estado)
dados$escol <- factor(dados$escol)
str(dados)

validation_index <- createDataPartition(dados$dias30, p=0.80, list=FALSE)

validation <- dados[-validation_index,]

dataset <- dados[validation_index,]

str(dataset)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#linear algorithms
set.seed(7)
fit.lda <- train(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, method="lda", metric=metric, trControl=control)

# CART
set.seed(7)
fit.cart <- train(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, method='cforest', metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, method="knn", metric=metric, trControl=control)

# SVM
set.seed(7)
fit.svm <- train(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, method="rf", metric=metric, trControl=control)



#predicted <- as.data.frame(predict(logitMod, validation, type="response"))

summary(logitMod)



library(InformationValue)
logitMod <- glm(dias30~Qtd.Dependentes+estado+fax_renda, data=dataset, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, validation))
optCutOff <- optimalCutoff(dataset$dias30, predicted)[1] 
optCutOff 
confusionMatrix(validation$dias30,predicted, threshold = 0.2)

library(ROCR)
#score test data set
test$score<-predict(m,type='response',test)
pred<-prediction(test$score,test$good_bad)
perf <- performance(pred,"tpr","fpr")
plot(perf)


