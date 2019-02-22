library(ggmap)

dados <- read.table("MatriculasEB2018_CODSETOR_NA_2.csv", header=TRUE, sep=";")
geocoding <- paste(dados$CEPEstab,dados$Municipio,sep=",")

###############Verificar quantidade de buscas que pode ser feita
geocodeQueryCheck(userType = "free")

###############Para usar essa versão é necessário instalar a ultima versão de GGMAP direto do Github
##############Para Criar uma chave vá no seguinte LINK: https://developers.google.com/maps/documentation/geocoding/get-api-key

register_google(key = "SUA API KEY AQUI")
latlongEDU_m <-geocode(geocoding)

###############Juntando na base original
baseedu <- cbind(dados,latlongEDU_m)

write.table(baseedu, "Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/MatriculasEB2018_latlong_na_2.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )


###############Outro método
###############Outro método
###############Outro método

library(googleway)

key <- "SUA API KEY AQUI"
qtd_row = "quantidade de linhas do arquivo"
final= data.frame(matrix(1,nrow=qtd_row,ncol=2))

for (i in 1:qtd_row){
teste=google_geocode(address = geocoding[i] , key = key)
for (j in 1:2)  {
final[i,j]=teste$results$geometry$location[i,j]
}
}
