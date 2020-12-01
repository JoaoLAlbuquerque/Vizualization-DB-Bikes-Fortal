# imagens 

rm(list = ls());gc()

library(shiny)
# require(shiny)
library(leaflet)
library(sf)
library(dplyr)
# require(dplyr)
library(data.table)
library(ggplot2)
# library(shinyWidgets)
# library(tableHTML)
library(extrafont)
library(shinydashboard)
# library(shinyjs)
library(viridis)
library(mapview)
library(gridExtra)
loadfonts()
loadfonts(device = "win")

spatial.coleta <- st_read('BASE_GEO_DB_VOLUME_BIKES.gpkg')
mapview(spatial.coleta)

df.info.sp <- cbind(spatial.coleta, st_coordinates(spatial.coleta)) %>% select(1,2,8,9) %>%
  mutate(type = factor('bike.icon'))

colnames(df.info.sp) <- c("ID","END","lng","lat",'type.icon')

data.original <- fread('DB_BIKES_1_V2.csv')

ranking <- fread(input = 'ranking_2018.csv')

infra <- st_read(dsn = 'infra_v2.shp')

mypal <- colorFactor(palette = "viridis", domain = infra$CONDITION)

# geom_sf 

ggplot() + geom_sf(data = spatial.coleta)



  loc.geral <-   leaflet(df.info.sp) %>%
      # addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      # addMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END) %>%
      addCircleMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END, stroke = FALSE,fillOpacity = 0.5, color = 'black') %>%
      addPolylines(data = infra,color = ~mypal(infra$CONDITION)) %>%
      addLegend(position = "bottomright", pal = mypal, values = infra$CONDITION,
                title = "Condition") %>%
      addCircleMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END, stroke = FALSE,fillOpacity = 0.5, color = 'black')
    
    
    teste <- data.original %>% filter(ID_SITIO == 5) %>% mutate(ANO = stringr::str_sub(DATA,-4,-1),
                               MINUTOS = 15) %>%
      group_by(ANO,DATA,PESQUISA,SENTIDO) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = sum(MINUTOS)/60) %>%
      arrange(SENTIDO) %>% ungroup() %>% group_by(SENTIDO) %>%
      mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>% arrange(SENTIDO,DATA) %>%
      mutate(DIF = VOLUME - lag(VOLUME), CORD.Y = lag(VOLUME) + (VOLUME-lag(VOLUME))/2,
             CORD.X = lag(as.numeric(ANO)) +(as.numeric(ANO)-lag(as.numeric(ANO)))/2, ANO = as.numeric(ANO)) %>%
      ungroup()

    # ggplot(teste) + geom_point(aes(y = VOLUME, x = ANO, color = as.factor(PESQUISA)), size = 3) +
    #     geom_text(data = teste,aes(x = CORD.X, y = CORD.Y, label = DIF),nudge_y = 100) +
    #     geom_line(aes(y = VOLUME, x = ANO, group = 1))  + ylab('Volume/15 min') + xlab('Ano') +
    #     facet_wrap(~SENTIDO) + theme_minimal() + labs(color = 'Tipo de pesquisa') +
    #   scale_x_continuous(breaks = seq(2013,2020,1))+ theme_classic()
    
    indiv <- ggplot(teste) + geom_line(aes(y = VOLUME, x = DATA, group = 1), size = 1) +
      geom_point(aes(y = VOLUME, x = DATA, color = as.factor(PESQUISA)), size = 4,alpha = 0.8) +
      geom_text(data = teste,aes(x = DATA, y = VOLUME, label = VOLUME),nudge_y = 0.05*max(teste$VOLUME), fontface = "bold",size = 4) +
      ylab('Volume de bicicletas ') + xlab('Mes/Ano') +
      facet_wrap(~SENTIDO)  + labs(color = 'Tipo de pesquisa', size = 'Horas Coletadas') + theme_classic()+
      theme(plot.title = element_text(face = "bold",size = 25),
            plot.subtitle = element_text(face = "bold", colour = "#27746E",size = 30),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size = 14, colour = '#27746E', face = "bold"),
            axis.text.x = element_text(size = 10,colour = '#27746E', face = "bold"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            strip.text =  element_text(size = 12, face = "bold", color = 'white'),
            strip.background = element_rect(fill="#27746E"),
            axis.line = element_line(size = 1, colour = "#27746E")) +
      scale_x_datetime(date_labels = "%m-%y",date_breaks = '6 month')+
      ggtitle('Por sentido',subtitle = unique(data()$END))


    
    evol.geral <- data.original %>% filter(ID_SITIO == 5) %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>%
      mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>%
      group_by(DATA,PESQUISA) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = (n()*15)/60/4) %>%
      arrange(DATA) %>% ungroup() %>% mutate(DIF = round(100*(VOLUME - lag(VOLUME))/lag(VOLUME),digits = 2),
                                             CORD.Y = lag(VOLUME) + (VOLUME - lag(VOLUME))/2,
                                             CORD.X = lag(as.numeric(DATA)) + 0.5)
    
    
   geral <-  ggplot(evol.geral) +
      geom_line(aes(y = VOLUME, x = DATA),size = 1)  + ylab('Volume de bicicletas ') + xlab('Mes/Ano') +
      geom_point(aes(y = VOLUME, x = DATA,color = as.factor(PESQUISA)),alpha = 0.8, size = 5) +
      geom_text(data = evol.geral, aes(x = DATA, y = VOLUME, label = VOLUME),nudge_y = 0.05*max(evol.geral$VOLUME), fontface = "bold", size = 5) +
      # scale_color_manual(values = c("red", "black")) +
      # geom_text(data = evol.geral %>% filter(DIF > 0),aes(x = CORD.X, y = CORD.Y, label = paste(DIF,"%")),nudge_y = 200,nudge_x = -.3, color = 'blue') +
      # geom_text(data = evol.geral %>% filter(DIF < 0),aes(x = CORD.X, y = CORD.Y, label = paste(DIF,"%")),nudge_y = 200,nudge_x = -.3, color = 'red')  +
      theme_classic() + labs(size = 'Horas coletadas') +
      theme(plot.title = element_text(face = "bold",size = 25),
            plot.subtitle = element_text(face = "bold", colour = "#27746E",size = 30),
            axis.title = element_text(size = 14),
            # axis.text.y = element_text(size = 14),
            # axis.text.x = element_text(size = 13),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            strip.text = element_text(size = 12, face = "bold", color = 'white'),
            strip.background = element_rect(fill="#27746E"),
            axis.line = element_line(size = 1, colour = "#27746E"),
            axis.text.y = element_text(size = 14, colour = '#27746E', face = "bold"),
            axis.text.x = element_text(size = 12,colour = '#27746E', face = "bold")) +
      scale_x_datetime(date_labels = "%m-%y",date_breaks = '6 month')+
      ggtitle('Total do ponto',subtitle = unique(data()$END))+
      labs(color = 'Tipo de pesquisa')
   
   
   teste.graf <- grid.arrange(geral,indiv)

    ## save html to png
    saveWidget(m, "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = "Rplot.png",
            cliprect = "viewport")    
    
    

