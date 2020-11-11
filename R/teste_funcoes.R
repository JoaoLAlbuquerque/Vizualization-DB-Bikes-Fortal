rm(list = ls());gc()

library(sf); library(dplyr);library(data.table); library(ggplot2); library(lubridate)
library(extrafont); library(leaflet); library(viridis); library(leaflet.opacity)

loadfonts(device = "win")


spatial.coleta <- st_read('BASE_GEO_DB_VOLUME_BIKES.gpkg')
df.info.sp <- cbind(spatial.coleta, st_coordinates(spatial.coleta)) %>% select(1,2,8,9)
colnames(df.info.sp) <- c("ID","END","lng","lat")


icon_list <- iconList(
  bike.icon = makeIcon(iconUrl = "icons/bicicleta.png",iconWidth = 38, iconHeight = 38))

bike.icon <- makeIcon(
  iconUrl = "icons/bicicleta.png",
  iconWidth = 38, iconHeight = 38
)

leaflet(df.info.sp) %>%
  addTiles() %>%  
  addMarkers( lng= ~lng, lat=~ lat,layerId = ~ID, icon = icon_list$bike.icon)
  
  
data <- fread('DB_BIKES_1_V2.csv')

ggplot(data, aes(y = TOTAL, x = HORA_I)) + geom_point()
rnorm(n = 100,mean = 1,sd = 1)

x <- data %>% na.omit()

plot(y = x$TOTAL, x = x$HORA_I)

teste <- data %>% filter(ID_SITIO == 80) %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>% 
  group_by(ANO,PESQUISA,SENTIDO) %>% summarise( VOLUME = sum(TOTAL)) %>% 
  arrange(SENTIDO) %>% ungroup() %>% group_by(SENTIDO) %>% 
  mutate(DIF = VOLUME - lag(VOLUME), CORD.Y = VOLUME - lag(VOLUME)/2,
         CORD.X = lag(as.numeric(ANO)) + 0.5, ANO = as.numeric(ANO)) 

teste <- data %>% filter(ID_SITIO == 80) %>% mutate(ANO = stringr::str_sub(DATA,-4,-1),
                                                    MINUTOS = 15) %>% 
  group_by(ANO,DATA,PESQUISA,SENTIDO) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = sum(MINUTOS)/60) %>% 
  arrange(SENTIDO) %>% ungroup() %>% group_by(SENTIDO) %>% 
  mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>% arrange(SENTIDO,DATA) %>% 
  mutate(DIF = VOLUME - lag(VOLUME), CORD.Y = lag(VOLUME) + (VOLUME-lag(VOLUME))/2,
         CORD.X = lag(as.numeric(ANO)) +(as.numeric(ANO)-lag(as.numeric(ANO)))/2, ANO = as.numeric(ANO)) %>% 
  ungroup()
  

# grafico individual 

ggplot(teste) + geom_line(aes(y = VOLUME, x = DATA, group = 1), size = 1.5) +
  geom_point(aes(y = VOLUME, x = DATA, color = as.factor(PESQUISA), size = 2*HORAS.COLETADAS),alpha = 0.5) +
  geom_text(data = teste,aes(x = DATA, y = VOLUME, label = VOLUME),nudge_y = 100,family = 'Bahnschrift', fontface = "bold") + 
  ylab('Volume ') + xlab('Ano') + 
  facet_wrap(~SENTIDO)  + labs(color = 'Tipo de pesquisa', size = 'Horas Coletadas') + theme_classic()+
  theme(plot.title = element_text(face = "bold",size = 14,family = 'Bahnschrift'),
        plot.subtitle = element_text(face = "bold", colour = "#C23517",size = 12,family = 'Bahnschrift'),
        axis.title = element_text(size = 14,family = 'Bahnschrift'),
        axis.text.y = element_text(size = 14,family = 'Bahnschrift'),
        axis.text.x = element_text(size = 13,family = 'Bahnschrift'),
        legend.text = element_text(size = 14,family = 'Bahnschrift'),
        legend.title = element_text(size = 14,family = 'Bahnschrift'),
        strip.text = element_text(size = 12,family = 'Bahnschrift')) + 
  scale_x_datetime(date_labels = "%m-%y",date_breaks = '6 month')



# evolução geral 

evol.geral <- data %>% filter(ID_SITIO == 80) %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>% 
  mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>%  
  group_by(DATA,PESQUISA) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = (n()*15)/60/4) %>% 
  arrange(DATA) %>% ungroup() %>% mutate(DIF = round(100*(VOLUME - lag(VOLUME))/lag(VOLUME),digits = 2),
                                        CORD.Y = lag(VOLUME) + (VOLUME - lag(VOLUME))/2,
                                        CORD.X = lag(as.numeric(DATA)) + 0.5)

ggplot(evol.geral) +  
  geom_line(aes(y = VOLUME, x = DATA),size = 1)  + ylab('Volume ') + xlab('Ano') + 
  geom_point(aes(y = VOLUME, x = DATA, color = as.factor(PESQUISA)),alpha = 0.5) +
  geom_text(data = evol.geral, aes(x = DATA, y = VOLUME, label = VOLUME),nudge_y = 0.05*max(evol.geral$VOLUME),family = 'Bahnschrift', fontface = "bold") +
  # scale_color_manual(values = c("red", "black")) +
  # geom_text(data = evol.geral %>% filter(DIF > 0),aes(x = CORD.X, y = CORD.Y, label = paste(DIF,"%")),nudge_y = 200,nudge_x = -.3, color = 'blue') +
  # geom_text(data = evol.geral %>% filter(DIF < 0),aes(x = CORD.X, y = CORD.Y, label = paste(DIF,"%")),nudge_y = 200,nudge_x = -.3, color = 'red')  +
  theme_classic() + labs(size = 'Horas coletadas') +
  theme(plot.title = element_text(face = "bold",size = 14,family = 'Bahnschrift'),
        plot.subtitle = element_text(face = "bold", colour = "#C23517",size = 12,family = 'Bahnschrift'),
        axis.title = element_text(size = 14,family = 'Bahnschrift'),
        axis.text.y = element_text(size = 14,family = 'Bahnschrift'),
        axis.text.x = element_text(size = 13,family = 'Bahnschrift'),
        legend.text = element_text(size = 14,family = 'Bahnschrift'),
        legend.title = element_text(size = 14,family = 'Bahnschrift'),
        strip.text = element_text(size = 12,family = 'Bahnschrift')) + 
  scale_x_datetime(date_labels = "%m-%y",date_breaks = '6 month') + 
  labs(color = 'Tipo de pesquisa')


# ranking -----------------------------------------------------------------

resumo <- data %>% dplyr::mutate(ano =  stringr::str_sub(DATA,-4,-1)) %>% filter(ano == '2018') %>% 
  group_by(ID_SITIO) %>% summarise(TOTAL = sum(TOTAL)) %>% arrange(desc(TOTAL)) %>% mutate(Ranking = 1:n())

write.csv2(x = resumo,file = 'ranking_2018.csv',row.names = F)


# leaflet infra -----------------------------------------------------------

infra <- st_read(dsn = 'total_infra_v1.shp') %>% 
  mutate(CONDITION = case_when(cndc_nf == "???tima"~ 5,
                               cndc_nf == "Bom"~ 4,
                               cndc_nf == "Regular"~ 3,
                               cndc_nf == "Ruim"~ 2,
                               cndc_nf == "P???ssimo"~ 1
                               ))

st_write(infra,dsn = 'infra_v2.shp')

unique(infra$cndc_nf)

infra <- st_read(dsn = 'infra_v2.shp')

library(dplyr)
library(leaflet)
library(sf)
library(viridis)

mypal <- colorFactor(palette = "viridis", domain = infra$CONDITION)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = infra,color = ~mypal(infra$CONDITION)) %>% 
  addLegend(position = "bottomright", pal = mypal, values = infra$CONDITION,
                                            title = "Condition",
                                            opacity = 1)

leaflet(df.info.sp) %>%
  # addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END) %>%
  addCircleMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END, stroke = FALSE,fillOpacity = 0.5, color = 'black') %>% 
  addPolylines(data = infra,color = ~mypal(infra$CONDITION)) %>% 
  addLegend(position = "bottomright", pal = mypal, values = infra$CONDITION,
            title = "Condition") 

  # addOpacitySlider()
                                        