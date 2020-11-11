
# library(shiny)
# require(shiny)
library(leaflet)
library(sf)
library(dplyr)
# require(dplyr)
library(data.table)
library(ggplot2)
# library(shinyWidgets)
# library(tableHTML)
# library(extrafont)
library(shinydashboard)
# library(shinyjs)
# library(viridis)





# loadfonts()
# loadfonts(device = "win")

spatial.coleta <- st_read('BASE_GEO_DB_VOLUME_BIKES.gpkg')

df.info.sp <- cbind(spatial.coleta, st_coordinates(spatial.coleta)) %>% select(1,2,8,9) %>%
    mutate(type = factor('bike.icon'))

colnames(df.info.sp) <- c("ID","END","lng","lat",'type.icon')

data.original <- fread('DB_BIKES_1_V2.csv')

ranking <- fread(input = 'ranking_2018.csv')

infra <- st_read(dsn = 'infra_v2.shp')

mypal <- colorFactor(palette = "viridis", domain = infra$CONDITION)

# icon_list <- iconList(
#     bike.icon = makeIcon(iconUrl = "icons/bicicleta.png",iconWidth = 38, iconHeight = 38))

# cor TRANSITAR #27746E
# Define UI for application that draws a histogram
ui <- dashboardPage(


  dashboardHeader(disable = TRUE),


  dashboardSidebar(

    sidebarMenu(

      menuItem("Mobilidade", tabName = "mobility", icon = icon("bike")),
      menuItem("Acessiblidade", tabName = "acc", icon = icon("accessiblity"))

    )),


  dashboardBody(

    tabItems(

      tabItem(tabName = 'mobility',


              fluidRow(

                # output com mapa dos pontos coletados
                column(width = 5,
                       h1(tags$b("Selecione um ponto de coleta:")),
                       leafletOutput('map',height=700),



                       fluidRow(


                         infoBoxOutput("Ranking",width = 600),
                         # infoBoxOutput("approvalBox")



                       )



                ),

                column(width = 7,
                       # offset = 1,
                       # h1(tags$b("Grafico por Sentido")),
                       plotOutput('graf',height = 500),
                       # h1(tags$b("Grafico geral")),
                       plotOutput('graf2',height = 500)
                )


              ))

              )


    )


)



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Ranking <- renderValueBox({

    id <- unique(data()$ID_SITIO)

    ranking <- ranking %>% filter(ID_SITIO == id) %>% pull(Ranking)

    valueBox(paste(ranking,'de 118'),
      "Ranking de volume (P/ 2018):",
      icon = icon("trophy"),
      color = "green"
    )

  })


  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

    output$map <- renderLeaflet({

      leaflet(df.info.sp) %>%
        # addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        # addMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END) %>%
        addCircleMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END, stroke = FALSE,fillOpacity = 0.5, color = 'black') %>%
        addPolylines(data = infra,color = ~mypal(infra$CONDITION)) %>%
        addLegend(position = "bottomright", pal = mypal, values = infra$CONDITION,
                  title = "Condition") %>%
        addCircleMarkers(lng= ~lng, lat=~ lat,layerId = ~ID, label = ~END, stroke = FALSE,fillOpacity = 0.5, color = 'black')


    })


    ponto <- reactiveValues()

    # observe({
    #     event <- input$map_shape_click
    #     if (is.null(event)){
    #         ponto$id = NA
    #     } else{
    #         ponto$id = event$id
    #     }
    #
    # })

    observe({
        event <- input$map_marker_click
        if(is.null(event))
            return()

    ponto$id = event$id
    })


   data <-   reactive({

           if (is.null(ponto$id)){

               return(data.original %>% na.omit() %>% filter(ID_SITIO == 5))
           }

           else{

               return(data.original %>% filter(ID_SITIO %in% ponto$id) %>%
                          na.omit())

           }

       })



 output$graf <- renderPlot({

   teste <- data() %>% mutate(ANO = stringr::str_sub(DATA,-4,-1),
                                                                   MINUTOS = 15) %>%
     group_by(ANO,DATA,PESQUISA,SENTIDO) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = sum(MINUTOS)/60) %>%
     arrange(SENTIDO) %>% ungroup() %>% group_by(SENTIDO) %>%
     mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>% arrange(SENTIDO,DATA) %>%
     mutate(DIF = VOLUME - lag(VOLUME), CORD.Y = lag(VOLUME) + (VOLUME-lag(VOLUME))/2,
            CORD.X = lag(as.numeric(ANO)) +(as.numeric(ANO)-lag(as.numeric(ANO)))/2, ANO = as.numeric(ANO)) %>%
     ungroup()
     #
     # ggplot(teste) + geom_point(aes(y = VOLUME, x = ANO, color = as.factor(PESQUISA)), size = 3) +
     #     geom_text(data = teste,aes(x = CORD.X, y = CORD.Y, label = DIF),nudge_y = 100) +
     #     geom_line(aes(y = VOLUME, x = ANO, group = 1))  + ylab('Volume/15 min') + xlab('Ano') +
     #     facet_wrap(~SENTIDO) + theme_minimal() + labs(color = 'Tipo de pesquisa') +
     #   scale_x_continuous(breaks = seq(2013,2020,1))+ theme_classic()

   ggplot(teste) + geom_line(aes(y = VOLUME, x = DATA, group = 1), size = 1) +
     geom_point(aes(y = VOLUME, x = DATA, color = as.factor(PESQUISA)), size = 4,alpha = 0.8) +
     geom_text(data = teste,aes(x = DATA, y = VOLUME, label = VOLUME),nudge_y = 0.05*max(teste$VOLUME), fontface = "bold",size = 5) +
     ylab('Volume de bicicletas ') + xlab('Mes/Ano') +
     facet_wrap(~SENTIDO)  + labs(color = 'Tipo de pesquisa', size = 'Horas Coletadas') + theme_classic()+
     theme(plot.title = element_text(face = "bold",size = 25),
           plot.subtitle = element_text(face = "bold", colour = "#27746E",size = 30),
           axis.title = element_text(size = 14),
           axis.text.y = element_text(size = 14, colour = '#27746E', face = "bold"),
           axis.text.x = element_text(size = 12,colour = '#27746E', face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 14),
           strip.text =  element_text(size = 12, face = "bold", color = 'white'),
           strip.background = element_rect(fill="#27746E"),
           axis.line = element_line(size = 1, colour = "#27746E")) +
     scale_x_datetime(date_labels = "%m-%y",date_breaks = '6 month')+
     ggtitle('Por sentido',subtitle = unique(data()$END))


     # x <- ggplot(data, aes(y = TOTAL, x = HORA_I)) + geom_point()
     # x
     #
     # ggplot(data = data(),
     #        aes(y = TOTAL, x = HORA_I, color = as.factor(DATA))) + geom_point() +
     #     # geom_line(data = data(),aes(y = TOTAL, x = HORA_I,group = as.factor(DATA))) %>%
     #     theme_bw() + facet_wrap(~DATA)

     # hist(rnorm(nrow(data()),1,0),breaks = 10, col = 'darkgray', border = 'white')

 })


 output$graf2 <- renderPlot({

   # evol.geral <- data() %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>%
   #   group_by(ANO,PESQUISA) %>% summarise( VOLUME = sum(TOTAL)) %>%
   #   arrange(ANO) %>% ungroup() %>% mutate(DIF = round(100*(VOLUME - lag(VOLUME))/lag(VOLUME),digits = 2),
   #                                         CORD.Y = lag(VOLUME) + (VOLUME - lag(VOLUME))/2,
   #                                         CORD.X = lag(as.numeric(ANO)) + 0.5, ANO = as.numeric(ANO))

   # evol.geral <- data() %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>%
   #   mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>%
   #   group_by(DATA,PESQUISA) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = (n()*15)/60/4) %>%
   #   arrange(DATA) %>% ungroup() %>% mutate(DIF = round(100*(VOLUME - lag(VOLUME))/lag(VOLUME),digits = 2),
   #                                          CORD.Y = lag(VOLUME) + (VOLUME - lag(VOLUME))/2,
   #                                          CORD.X = lag(as.numeric(DATA)) + 0.5)

   evol.geral <- data() %>% mutate(ANO = stringr::str_sub(DATA,-4,-1)) %>%
     mutate(DATA = as.POSIXct(DATA, format = c("%d/%m/%Y"))) %>%
     group_by(DATA,PESQUISA) %>% summarise( VOLUME = sum(TOTAL), HORAS.COLETADAS = (n()*15)/60/4) %>%
     arrange(DATA) %>% ungroup() %>% mutate(DIF = round(100*(VOLUME - lag(VOLUME))/lag(VOLUME),digits = 2),
                                            CORD.Y = lag(VOLUME) + (VOLUME - lag(VOLUME))/2,
                                            CORD.X = lag(as.numeric(DATA)) + 0.5)

   # ggplot(evol.geral) + geom_point(aes(y = VOLUME, x = ANO, color = as.factor(PESQUISA))) +
   #   geom_line(aes(y = VOLUME, x = ANO), size = 1)  + ylab('Volume ') + xlab('Ano') +
   #   geom_text(data = evol.geral, aes(x = CORD.X, y = CORD.Y, label = paste(DIF,"%")),nudge_y = 200,nudge_x = -.3, color = ifelse(evol.geral$DIF >0,'#138904' , '#F82E03'),
   #             family = 'Bahnschrift', fontface = "bold",size = 5)+
   #   theme_classic() + labs(color = 'Tipo de pesquisa') +
   #   theme(plot.title = element_text(face = "bold",size = 14,family = 'Bahnschrift'),
   #         plot.subtitle = element_text(face = "bold", colour = "#C23517",size = 12,family = 'Bahnschrift'),
   #         axis.title = element_text(size = 14,family = 'Bahnschrift'),
   #         axis.text.y = element_text(size = 14,family = 'Bahnschrift'),
   #         axis.text.x = element_text(size = 12,family = 'Bahnschrift'),
   #         legend.text = element_text(size = 14,family = 'Bahnschrift'),
   #         legend.title = element_text(size = 14,family = 'Bahnschrift'),
   #         strip.text = element_text(size = 12,family = 'Bahnschrift'))

   ggplot(evol.geral) +
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

 })


}

# Run the application
shinyApp(ui = ui, server = server)
