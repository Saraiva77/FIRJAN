library(googleway)

dados <- read.csv("basecarrosroubados.csv", header=TRUE, sep=";", dec=",")
geocoding <- paste(dados$ENDERECO, dados$UF, sep=",")
str(dados)

key <- "AIzaSyC8ptTT76kpVE24vZUPUng1PDUbqHMYW6I"
final= data.frame(matrix(1,nrow=596,ncol=3))


for (i in 1:596){
  teste=google_geocode(address = geocoding[i] , key = key)
  if (teste$status[1] != "ZERO_RESULTS") {
    final[i,3]=teste$results$formatted_address[1]
    final[i,1]=teste$results$geometry$location$lat[1]
    final[i,2]=teste$results$geometry$location$lng[1]
  }
}

salvar <- cbind(dados,final)
write.table(salvar, "geo_BASE.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )

dados <- read.csv("geo_BASE.csv", header=TRUE, sep=";", dec=",")
##########################################################FIM####################################################
##########################################################FIM####################################################
##########################################################FIM####################################################
##########################################################FIM####################################################
