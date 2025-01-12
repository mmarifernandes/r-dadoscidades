dadoschapeco <- read.csv("dados-chapeco.csv",header = TRUE, skip = 9, sep = ";",dec = ".", check.names = FALSE )
dados <- read.csv("dados-xanxere.csv",header = TRUE, skip = 9, sep = ";",dec = ".", check.names = FALSE )
str(dados)
View(dados)
str(dadoschapeco)
View(dadoschapeco)
dados[9] <- NULL
names(dados) <- c("Data", "Prec", "TempMax", "TempMedia", "TempMin", "UmidadeMedia", "VentoRajMax", "VentoVelMedia")
dadoschapeco[9] <- NULL
names(dadoschapeco) <- c("Data", "Prec", "TempMax", "TempMedia", "TempMin", "UmidadeMedia", "VentoRajMax", "VentoVelMedia")

dados$Data<-as.Date(dados$Data,"%Y-%m-%d")
dados[,2] <- as.numeric(dados[,2]) 
dados[,3] <- as.numeric(dados[,3]) 
dados[,4] <- as.numeric(dados[,4]) 
dados[,5] <- as.numeric(dados[,5]) 
dados[,6] <- as.numeric(dados[,6]) 
dados[,7] <- as.numeric(dados[,7])
dados[,8] <- as.numeric(dados[,8]) 

dadoschapeco$Data<-as.Date(dadoschapeco$Data,"%Y-%m-%d")
dadoschapeco[,2] <- as.numeric(dadoschapeco[,2]) 
dadoschapeco[,3] <- as.numeric(dadoschapeco[,3]) 
dadoschapeco[,4] <- as.numeric(dadoschapeco[,4]) 
dadoschapeco[,5] <- as.numeric(dadoschapeco[,5]) 
dadoschapeco[,6] <- as.numeric(dadoschapeco[,6]) 
dadoschapeco[,7] <- as.numeric(dadoschapeco[,7])
dadoschapeco[,8] <- as.numeric(dadoschapeco[,8])

str(dados)
View(dados)
str(dadoschapeco)
View(dadoschapeco)

dados$Dia <- as.numeric(format(dados$Data, format = "%d"))
dados$Mes <- as.numeric(format(dados$Data, format = "%m"))
dados$Ano <- as.numeric(format(dados$Data, format = "%Y"))

dadoschapeco$Dia <- as.numeric(format(dadoschapeco$Data, format = "%d"))
dadoschapeco$Mes <- as.numeric(format(dadoschapeco$Data, format = "%m"))
dadoschapeco$Ano <- as.numeric(format(dadoschapeco$Data, format = "%Y"))

install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

temperatura <- select(dados, TempMedia)
temperaturachap <- select(dadoschapeco, TempMedia)

mean(temperatura$TempMedia, na.rm=TRUE)
min(temperatura$TempMedia, na.rm=TRUE)
max(temperatura$TempMedia, na.rm=TRUE)

resumo<-summary(temperatura$TempMedia)
resumo
range(temperatura$Temp, na.rm = TRUE)

View(dados$Ano)
anos <- dados%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
anosc <- dadoschapeco%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
View(anosc)


grafico <- plot_ly() %>%
  add_trace( x = anos$Ano, y = anos$TempMedia, type = 'bar', name = 'Xanxerê', marker = list(color = 'pink')) %>%
  add_trace(x = anosc$Ano, y = anosc$TempMedia, type = 'bar', name = 'Chapecó', marker = list(color = 'red')) %>%
  layout(title = "Temperatura Média Anual - Xanxerê e Chapecó",xaxis = list(title = "Ano"),yaxis = list(title = "Temperatura Média (°C)"), barmode = 'group'
  )

grafico

grafico2 <- plot_ly() %>%
  add_trace( x = max(temperatura$TempMedia, na.rm=TRUE), y='Xanxerê', type = 'bar', name = 'Temperatura Máxima Xanxerê', marker = list(color = 'orange')) %>%
  add_trace(x = max(temperaturachap$TempMedia, na.rm=TRUE), y='Chapecó',  type = 'bar', name = 'Temperatura Máxima Chapecó', marker = list(color = 'red')) %>%
  add_trace( x = min(temperatura$TempMedia, na.rm=TRUE), y='Xanxerê', type = 'bar', name = 'Temperatura Mínima Xanxerê', marker = list(color = 'blue')) %>%
  add_trace(x = min(temperaturachap$TempMedia, na.rm=TRUE), y='Chapecó', type = 'bar', name = 'Temperatura Mínima Chapecó', marker = list(color = 'gray')) %>%
  layout(title = "Temperaturas Mínimas e Máximas - Xanxerê e Chapecó",xaxis = list(title = "Temperatura (°C)"), yaxis = list(title='Cidades'),barmode = 'group'
          )
grafico2
#save.image("Aula01R.RData")
