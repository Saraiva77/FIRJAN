library(magrittr)
library(dplyr)
library(leaflet)
library(stringr)
library(RColorBrewer)
library(raster)
library(rgdal)
library(sp)
library(sf)
library(rgr)
library(leaflet.extras)

setwd("Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/BI ESCOLAS")

dataset= as.data.frame(read.csv("Bd Clientes EB Geo Final.csv", sep=";",dec=",", header =  TRUE, stringsAsFactors = FALSE))
escolas= as.data.frame(read.csv("Resumo EB concorrentes.csv", sep=";",dec=",", header =  TRUE, stringsAsFactors = FALSE))

dataset$Status.Pagamento =as.factor(dataset$Status.Pagamento)
dataset$Divida.Total=as.numeric(gsub(",",".",dataset$Divida.Total))

dataset_Adim=subset(dataset,Status.Pagamento != "Inadimplente" )
dataset_Inad=subset(dataset,Status.Pagamento == "Inadimplente" )

textoPopup <- function(data,tipo) {
  if (!tipo %in% c("Adimplente", "Últimos Matriculados","Inadimplente")) stop("Input errado.")
  if (tipo=="Adimplente" | tipo=="Últimos Matriculados" ){
  x = paste0(
    "Unidade de Negócio: ", dataset_Adim$UnidadeNegocio, "<br>",
    "Valor Contrato ", dataset_Adim$ValorContrato, "<br>"
  )
  }  
    if (tipo =="Inadimplente") {
  x = paste0(
    "Unidade de Negócio: ", dataset_Inad$UnidadeNegocio, "<br>",
    "Responsável: ", dataset_Inad$NomeResponsavel, "<br>",
    "Telefone: ", dataset_Inad$UltimoCelular, "<br>",
    "E-mai: ", dataset_Inad$UltimoEmail, "<br>",
    "Dívida Total R$: ", dataset_Inad$Divida.Total, "<br>",
    "Qtd. de Boletos: ", dataset_Inad$Qtd.Boletos, "<br>",
    "Status :", dataset_Inad$Status.Pagamento, "<br>",
    "Venc. Boleto mais antigo:", dataset_Inad$Boleto.Mais.Antigo, "<br>"
  )
  }
  return(x)
}


intervalo <- seq(0,10000,2000)
palette_rev <- brewer.pal(length(intervalo), "YlOrRd")
vetorDivida <- colorBin(palette_rev, domain = dataset_Inad$Divida.Total, bins = intervalo, na.color = "Purple")


map <- leaflet() %>%
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addMarkers(lng = -43.183447, lat = -22.913912) %>% 
  addCircleMarkers(data=dataset_Inad,
    lng=~Long, lat=~Lat,
      color = ~vetorDivida(Divida.Total),
      opacity = 1.5,
      popup = textoPopup(dataset_Inad$Status.Pagamento,"Inadimplente"),
      group = "Inadimplente")%>% 
  addCircleMarkers(data=dataset_Adim,
                 lng=~Long, lat=~Lat,
                 color = "Blue",
                 opacity = 1.5,
                 popup = textoPopup(dataset_Adim$Status.Pagamento,"Adimplente"),
                 group = "Adimplente")%>%
  addLegend(pal = vetorDivida, values = dataset_Inad$Divida.Total)%>%
addLayersControl( overlayGroups = c("Adimplente", "Inadimplente"),
  options = layersControlOptions(collapsed = FALSE))
map  

dataset_jac=subset(dataset,UnidadeNegocio == "Centro de Atividade Volta Redonda" )

pal <- colorFactor(c("red", "navy"), domain = c("SESI", "Monitorado"))

radio=escolas$Preco_Medio/80
label= paste(escolas$No_Entidade, ", R$",escolas$Preco_Medio,"")


map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addMarkers(lng = -43.183447, lat = -22.913912) %>%
addHeatmap(data=dataset_jac ,lng = ~Long, lat = ~Lat, intensity = ~contador,
           blur = 20, max = 0.05, radius = 10)%>% 
addCircleMarkers(data=escolas,
                 lng=~Long, lat=~Lat,
                 popup=label,
  radius =radio ,
  color = ~pal(índice.de.competitividade.SESI ),
  stroke = FALSE, fillOpacity = 1
)
map 
