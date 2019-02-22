####Necessário criar um arquivo ODBC no Windows no configurador de odbc
library(DBI)
ddd <- dbConnect(odbc::odbc(),
                 timezone = "UTC",
                 encoding = "UTF-8",
                 Driver = "SQLServer",
                 Server = "SRVSEDE0119",
                 Database = "ProdInfoODS",
                 dsn="teste r",
                 Port = 1433)

dbListFields(ddd,"vwSRC_PF_Fiel")
data <- dbReadTable(ddd, "vwSRC_PF_Fiel")
dataN <- dbReadTable(ddd, "vwSRC_PF_Novos")
library(sqldf)
teste=sqldf("SELECT CPF,DDD_Celular,Numero_Celular,Email FROM data GROUP BY CPF")
testeN=sqldf("SELECT CPF,DDD_Celular,Numero_Celular,Email FROM dataN GROUP BY CPF")

write.table(teste, "telefone_fiel.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )
write.table(testeN, "telefone_novos.csv",sep=";",dec=",",row.names =FALSE, col.names=TRUE )
