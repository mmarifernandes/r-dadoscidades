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

#install.packages("tidyverse")
#install.packages("plotly")

library(tidyverse)
library(plotly)

temperatura <- select(dados, TempMedia)
temperaturachap <- select(dadoschapeco, TempMedia)

mean(temperatura$TempMedia, na.rm=TRUE)
mean(temperaturachap$TempMedia, na.rm=TRUE)

mean(dados$TempMax, na.rm=TRUE)
mean(dadoschapeco$TempMax, na.rm=TRUE)

mean(dados$TempMin, na.rm=TRUE)
mean(dadoschapeco$TempMin, na.rm=TRUE)

mean(dados$Prec, na.rm=TRUE)
mean(dadoschapeco$Prec, na.rm=TRUE)

mean(dados$VentoVelMedia, na.rm=TRUE)
mean(dadoschapeco$VentoVelMedia, na.rm=TRUE)

mean(dados$UmidadeMedia, na.rm=TRUE)
mean(dadoschapeco$UmidadeMedia, na.rm=TRUE)

mean(dados$VentoRajMax, na.rm=TRUE)
mean(dadoschapeco$VentoRajMax, na.rm=TRUE)

resumo<-summary(temperatura$TempMedia)
resumo
range(temperatura$Temp, na.rm = TRUE)

View(dados$Ano)
anos <- dados%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
anosc <- dadoschapeco%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
View(anosc)


graficorajada <- plot_ly() %>%
  add_trace( x = dados$Data[1858:4018], y = dados$VentoRajMax[1858:4018], type = 'scatter', name = 'Xanxerê', marker = list(color = 'red')) %>%
  add_trace( x = dadoschapeco$Data, y = dadoschapeco$VentoRajMax, type = 'scatter', name = 'Chapecó', marker = list(color = 'blue')) %>%
  layout(title = "Velocidade Máxima Rajada de Vento - Xanxerê",xaxis = list(title = "Ano"),yaxis = list(title = "Velocidade Máxima Rajada de Vento (mm)"), barmode = 'group'
  )

graficorajada


graficorajadazoom <- plot_ly() %>%
  add_trace( x = dados$Data[2404:3565], y = dados$VentoRajMax[2404:3565], type = 'scatter', name = 'Xanxerê', marker = list(color = 'red')) %>%
  add_trace( x = dadoschapeco$Data[499:1660], y = dadoschapeco$VentoRajMax[499:1660], type = 'scatter', name = 'Chapecó', marker = list(color = 'blue')) %>%
  layout(title = "Velocidade Máxima Rajada de Vento - Xanxerê",xaxis = list(title = "Ano"),yaxis = list(title = "Velocidade Máxima Rajada de Vento (mm)"), barmode = 'group'
  )

graficorajadazoom

plot(dadoschapeco$Data,dadoschapeco$TempMedia,ylim =c(-5,32),type="l" )
points(dadoschapeco$Data,dadoschapeco$TempMax,type="l",col="red" )
points(dadoschapeco$Data,dadoschapeco$TempMin,type="l",col="blue" )

grafico <- plot_ly() %>%
  add_trace( x = anos$Ano[7:12], y = anos$TempMedia[7:12], type = 'bar', name = 'Xanxerê', marker = list(color = 'red')) %>%
  add_trace(x = anosc$Ano, y = anosc$TempMedia, type = 'bar', name = 'Chapecó', marker = list(color = 'blue')) %>%
  layout(title = "Temperatura Média Anual - Xanxerê e Chapecó",xaxis = list(title = "Ano"),yaxis = list(title = "Temperatura Média (°C)"), barmode = 'group'
  )

grafico



#save.image("Aula01R.RData")
