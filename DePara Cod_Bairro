library(foreign)
library(stringr)
library(dplyr)

miss_bairro <- read.spss("MISSING_COD_BAIRRO_ALUNOS_EB.sav", to.data.frame =TRUE )
miss_bairro_2 <-read.csv("Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/Cod_Bairro/Matriculas_EB_missing_2.csv",header=TRUE,sep=";")
  
  
tijolo <- read.spss("Z:/Núcleo de Informações/BI/FIRJAN/Universo Censo/TIJOLAO_Universo.sav", to.data.frame =TRUE )

tijolo_rj<- subset(tijolo, Cod_UF == 33)

parte_tijolo <- tijolo_rj[,c("Cod_municipio","Nome_do_municipio","Cod_bairro","Nome_do_bairro")]

miss_bairro_conc <- cbind(paste(str_trim(miss_bairro$Municipio),str_trim(miss_bairro$Bairro), sep="_"),miss_bairro$Alun_ID)
parte_tijolo_conc <- cbind(paste(str_trim(parte_tijolo$Nome_do_municipio),str_trim(parte_tijolo$Nome_do_bairro),sep="_"),parte_tijolo$Cod_bairro)

colnames(miss_bairro_conc) <- c("Chave","Alun_ID")
colnames(parte_tijolo_conc) <- c("Chave","Cod_bairro")

parte_tijolo_conc <- as.data.frame(parte_tijolo_conc )
miss_bairro_conc <- as.data.frame(miss_bairro_conc )


parte_tijolo_conc$Chave <- gsub("\\(todos os setores)","",parte_tijolo_conc$Chave)
parte_tijolo_conc$Chave <- gsub("\\(demais setores)","",parte_tijolo_conc$Chave)
parte_tijolo_conc$Chave <- str_trim(parte_tijolo_conc$Chave)


parte_tijolo_conc$Chave <- toupper(parte_tijolo_conc$Chave)
miss_bairro_conc$Chave <-toupper(miss_bairro_conc$Chave )

novo <- merge(miss_bairro_conc,parte_tijolo_conc)

write.table(novo, "Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/depara_Cod_Bairro_1.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )

####################################################################Segunda parte para os 'todos setores' e 'demais setores'##########################

tijolo_rj_T<- tijolo_rj[grep("todos os setores", tijolo_rj$Nome_do_bairro),]
tijolo_rj_D<- tijolo_rj[grep("demais setores", tijolo_rj$Nome_do_bairro),]

parte_tijolo_T <- tijolo_rj_T[,c("Cod_municipio","Nome_do_municipio","Cod_bairro","Nome_do_bairro")]
parte_tijolo_D <- tijolo_rj_D[,c("Cod_municipio","Nome_do_municipio","Cod_bairro","Nome_do_bairro")]

write.table(parte_tijolo_T, "Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/tijolo_T.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )
write.table(parte_tijolo_D, "Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/tijolo_D.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )

################################################################### Se precisar exportar 1 municipio especifico########################################################
tijolo_fim<- subset(tijolo_rj, Cod_municipio == "INSIRA O CODIGO DO MUNICIPIO")
barrap=tijolo_fim[,c("Nome_do_municipio","Cod_bairro","Nome_do_bairro")]
write.table(barrap, "Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/Cod_bairro/barrap.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )


