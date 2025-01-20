dadosc <- read.csv("dados-chapeco.csv",header = TRUE, skip = 9, sep = ";",dec = ".", check.names = FALSE )
dadosx <- read.csv("dados-xanxere.csv",header = TRUE, skip = 9, sep = ";",dec = ".", check.names = FALSE )
str(dadosx)
View(dadosx)
str(dadosc)
View(dadosc)
dadosx[9] <- NULL
names(dadosx) <- c("Data", "Prec", "TempMax", "TempMedia", "TempMin", "UmidadeMedia", "VentoRajMax", "VentoVelMedia")
dadosc[9] <- NULL
names(dadosc) <- c("Data", "Prec", "TempMax", "TempMedia", "TempMin", "UmidadeMedia", "VentoRajMax", "VentoVelMedia")

dadosx$Data<-as.Date(dadosx$Data,"%Y-%m-%d")
dadosx[,2] <- as.numeric(dadosx[,2]) 
dadosx[,3] <- as.numeric(dadosx[,3]) 
dadosx[,4] <- as.numeric(dadosx[,4]) 
dadosx[,5] <- as.numeric(dadosx[,5]) 
dadosx[,6] <- as.numeric(dadosx[,6]) 
dadosx[,7] <- as.numeric(dadosx[,7])
dadosx[,8] <- as.numeric(dadosx[,8]) 

dadosc$Data<-as.Date(dadosc$Data,"%Y-%m-%d")
dadosc[,2] <- as.numeric(dadosc[,2]) 
dadosc[,3] <- as.numeric(dadosc[,3]) 
dadosc[,4] <- as.numeric(dadosc[,4]) 
dadosc[,5] <- as.numeric(dadosc[,5]) 
dadosc[,6] <- as.numeric(dadosc[,6]) 
dadosc[,7] <- as.numeric(dadosc[,7])
dadosc[,8] <- as.numeric(dadosc[,8])

str(dadosx)
View(dadosx)
str(dadosc)
View(dadosc)

dadosx$Dia <- as.numeric(format(dadosx$Data, format = "%d"))
dadosx$Mes <- as.numeric(format(dadosx$Data, format = "%m"))
dadosx$Ano <- as.numeric(format(dadosx$Data, format = "%Y"))

dadosc$Dia <- as.numeric(format(dadosc$Data, format = "%d"))
dadosc$Mes <- as.numeric(format(dadosc$Data, format = "%m"))
dadosc$Ano <- as.numeric(format(dadosc$Data, format = "%Y"))

#install.packages("tidyverse")
#install.packages("plotly")

library(tidyverse)
library(plotly)

temperatura <- select(dadosx, TempMedia)
temperaturachap <- select(dadosc, TempMedia)

mean(temperatura$TempMedia, na.rm=TRUE)
mean(temperaturachap$TempMedia, na.rm=TRUE)

mean(dadosx$TempMax, na.rm=TRUE)
mean(dadosc$TempMax, na.rm=TRUE)

mean(dadosx$TempMin, na.rm=TRUE)
mean(dadosc$TempMin, na.rm=TRUE)

mean(dadosx$Prec, na.rm=TRUE)
mean(dadosc$Prec, na.rm=TRUE)

mean(dadosx$VentoVelMedia, na.rm=TRUE)
mean(dadosc$VentoVelMedia, na.rm=TRUE)

mean(dadosx$UmidadeMedia, na.rm=TRUE)
mean(dadosc$UmidadeMedia, na.rm=TRUE)

mean(dadosx$VentoRajMax, na.rm=TRUE)
mean(dadosc$VentoRajMax, na.rm=TRUE)

resumo<-summary(temperatura$TempMedia)
resumo
range(temperatura$Temp, na.rm = TRUE)

View(dadosx$Ano)
anos <- dadosx%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
anosc <- dadosc%>% group_by(Ano)%>%
  summarise(TempMedia = mean(TempMedia, na.rm=TRUE))
View(anosc)


graficorajada <- plot_ly() %>%
  add_trace( x = dadosx$Data[1858:4018], y = dadosx$VentoRajMax[1858:4018], type = 'scatter', name = 'Xanxerê', marker = list(color = 'red')) %>%
  add_trace( x = dadosc$Data, y = dadosc$VentoRajMax, type = 'scatter', name = 'Chapecó', marker = list(color = 'blue')) %>%
  layout(title = "Velocidade Máxima Rajada de Vento",xaxis = list(title = "Ano"),yaxis = list(title = "Velocidade Máxima Rajada de Vento (m/s)"), barmode = 'group'
  )

graficorajada


graficorajadazoom <- plot_ly() %>%
  add_trace( x = dadosx$Data[2404:3565], y = dadosx$VentoRajMax[2404:3565], type = 'scatter', name = 'Xanxerê', marker = list(color = 'red')) %>%
  add_trace( x = dadosc$Data[499:1660], y = dadosc$VentoRajMax[499:1660], type = 'scatter', name = 'Chapecó', marker = list(color = 'blue')) %>%
  layout(title = "2020 à 2023 Velocidade Máxima Rajada de Vento",xaxis = list(title = "Ano"),yaxis = list(title = "Velocidade Máxima Rajada de Vento (m/s)"), barmode = 'group'
  )

graficorajadazoom

plot(dadosc$Data,dadosc$TempMedia,ylim =c(-5,32),type="l" )
points(dadosc$Data,dadosc$TempMax,type="l",col="red" )
points(dadosc$Data,dadosc$TempMin,type="l",col="blue" )


dados_agrupados <- dadosx %>%
  group_by(Ano) %>%
  summarise(
    TempMax = mean(TempMax, na.rm = TRUE),
    TempMin = mean(TempMin, na.rm = TRUE),
    TempMedia = mean(TempMedia, na.rm = TRUE)
  )

fig <- plot_ly(dados_agrupados[7:12, ], x = ~Ano, y = ~TempMax, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = 'TempMax') 
fig <- fig %>% add_trace(y = ~TempMin, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor = 'rgba(184,90,90,0.2)', line = list(color = 'transparent'),
                         showlegend = FALSE, name = 'TempMin') 
fig <- fig %>% add_trace(y = ~TempMedia, type = 'scatter', mode = 'lines',
                         line = list(color = 'rgb(184,90,90)'),
                         name = 'TempMedia') 
fig <- fig %>% layout(
  title = "Temperatura Média, Máxima e Mínima Anual de Xanxerê",
  paper_bgcolor = 'rgb(255,255,255)', 
  plot_bgcolor = 'rgb(229,229,229)',
  xaxis = list(
    title = "Ano",
    gridcolor = 'rgb(255,255,255)',
    showgrid = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    tickcolor = 'rgb(127,127,127)',
    ticks = 'outside',
    zeroline = FALSE
  ),
  yaxis = list(
    title = "Temperatura (°C)",
    gridcolor = 'rgb(255,255,255)',
    showgrid = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    tickcolor = 'rgb(127,127,127)',
    ticks = 'outside',
    zeroline = FALSE
  )
)

fig

dados_agrupadosC <- dadosc %>%
  group_by(Ano) %>%
  summarise(
    TempMax = mean(TempMax, na.rm = TRUE),
    TempMin = mean(TempMin, na.rm = TRUE),
    TempMedia = mean(TempMedia, na.rm = TRUE)
  )

figC <- plot_ly(dados_agrupadosC, x = ~Ano, y = ~TempMax, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = 'TempMax') 
figC <- figC %>% add_trace(y = ~TempMin, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor = 'rgba(100,109,201,0.2)', line = list(color = 'transparent'),
                         showlegend = FALSE, name = 'TempMin') 
figC <- figC %>% add_trace(y = ~TempMedia, type = 'scatter', mode = 'lines',
                         line = list(color = 'rgb(100,109,201)'),
                         name = 'TempMedia') 
figC <- figC %>% layout(
  title = "Temperatura Média, Máxima e Mínima Anual de Chapecó",
  paper_bgcolor = 'rgb(255,255,255)', 
  plot_bgcolor = 'rgb(229,229,229)',
  xaxis = list(
    title = "Ano",
    gridcolor = 'rgb(255,255,255)',
    showgrid = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    tickcolor = 'rgb(127,127,127)',
    ticks = 'outside',
    zeroline = FALSE
  ),
  yaxis = list(
    title = "Temperatura (°C)",
    gridcolor = 'rgb(255,255,255)',
    showgrid = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    tickcolor = 'rgb(127,127,127)',
    ticks = 'outside',
    zeroline = FALSE
  )
)

figC

save.image("TrabalhoMarinaSabrina.RData")
