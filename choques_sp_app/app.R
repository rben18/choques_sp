#Autor: Rodrigo Benavides
#Fecha: Enero 17, 2019

#Objetivo del script es poder visualizar en un mapa donde es la ubicación con mayor cantidad de choques en San Pedro y visualizar incidencias de distintos tipos de accidentes
#Los datos son de Enero 2014 a Septiembre del 2016 para los choques y Enero 2016 a Septiembre 2018 para los resultados de accidentes
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(DT)
library(zoo)

#Importar datos y cambiar formato de nombres de las calles
accidentes_cruceros <- read_csv("data/ACCIDENTES_CRUCEROS_SAN_PEDRO.csv")%>%
    #rename_all(iconv)%>%
    rename(anio = "año") %>%
    mutate(calle1 = iconv(calle1, from = "UTF-8", to = "UTF-8"),
           calle2 = iconv(calle2, from = "UTF-8", to = "UTF-8"),
           fechaCorte = as.Date(fechaCorte, format = "%d/%m/%Y"))

#Quedarnos con los nombres de los cruces para despues pegar sus coordenadas
coordenadas_cruces <- accidentes_cruceros %>%
    distinct(calle1,calle2)%>%
    arrange(calle1, calle2)

#NO CAMBIAR EL ORDEN
coordenadas_cruces$latitud <- c(25.65716, 25.65381, 25.65945, 25.67473, 25.67048,
                                25.64012, 25.65275, 25.66262, 25.66788, 25.65179, 
                                25.65158, 25.65399, 25.65214, 25.65337, 25.65380, 
                                25.63820, 25.66150, 25.65828, 25.65032, 25.66906
)
coordenadas_cruces$longitud <-  c(-100.37038, -100.35807, -100.36950, -100.40281, -100.40226,
                                  -100.36191, -100.35833, -100.35458, -100.38026, -100.36599, 
                                  -100.37154, -100.33980, -100.37795, -100.35280, -100.38121, 
                                  -100.33793, -100.36877, -100.36533, -100.35865, -100.38486
)

accidentes_cruceros <- left_join(accidentes_cruceros, coordenadas_cruces, by = c("calle1", "calle2"))

info_accidentes <- read_csv("data/ACCIDENTES_VIALES_SP.csv")%>%
    rename_all(tolower)%>%
    rename(accidente_vial = "accidente vial")%>%
    mutate(
        mes = ifelse(str_count(mes) == 1, paste0("0", mes), mes),
        mes_fecha = as.Date(paste0(anio,"-",mes, "-01")),
        anio_mes = as.yearmon(paste0(anio,"-",mes))
        )

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Titulo de aplicacion
    titlePanel("Información sobre choques en SPGG en distintos años"),
    em(h3("Primer tab podemos ver la ubicación de los choques y en el segundo tab más información de otros accidentes")),

        tabsetPanel(
        tabPanel(title = "Accidentes en Cruceros",
                 h3("Cantidad de choques en 20 cruces de San Pedro Garza Garcia"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  checkboxGroupInput(inputId = "anio_checkbox_cruces",
                                                     label = "Indique los años que quiera ver",
                                                     selected = sort(unique(accidentes_cruceros$anio)),
                                                     choices = sort(unique(accidentes_cruceros$anio))
                                  ),
                                  actionLink("seleccionar_todo","Seleccionar Todos"),
                                  br(),
                                  br(),
                                  actionButton(inputId = "ejecutar",
                                               label = "Ejecutar"
                                  ),
                                  br(),
                                  br(),
                                  h5("Nota: El año 2016 tiene datos hasta el 30 de Septiembre")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         #h3(paste0("Los datos son entre Enero del ", fecha_inicio, " y ", fecha_final)),
                         tabsetPanel(
                             tabPanel(title = "Mapa",
                                      leafletOutput(outputId = "mymap")
                             ),
                             tabPanel(title = "Tabla Datos",
                                      DT::dataTableOutput(outputId = "tabla_reporte_cruces")
                                      )
                             )
                         )
                     )
                 ),
        tabPanel(title = "Resultados de Accidentes",
                 h3("Más Información acerca de distintos accidentes, por mes"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  sliderInput(inputId = "rango_fechas_info", 
                                              label = "Escoge el rango de fechas",
                                              min = min(info_accidentes$mes_fecha),
                                              max = max(info_accidentes$mes_fecha),
                                              value = c(min(info_accidentes$mes_fecha),
                                                        max(info_accidentes$mes_fecha)),
                                              timeFormat="%b %Y"
                                  ),
                                
                                  br(),
                                  br(),
                                  h5("Nota: El año 2018 tiene datos hasta el 30 de Septiembre")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         #h3(paste0("Los datos son entre Enero del ", fecha_inicio, " y ", fecha_final)),
                         tabsetPanel(
                             tabPanel(title = "Visualizaciones",
                                      fluidRow(
                                          column(6,
                                                 h4("Cantidad de accidentes con menores"),
                                                 h4("En el verano se ve que hay menos accidentes involucrando a menores.\nQue bien que haya una tendencia a la baja"),
                                                 plotOutput(outputId = "grafica_menores")
                                          ),
                                          column(6,
                                                 h4("Cantidad de accidentes con lesionados"),
                                                 h4("En todos los meses se lesiona gente en accidentes"),
                                                 plotOutput(outputId = "grafica_lesionados")
                                          )
                                      ),
                                      fluidRow(
                                          column(6,
                                                 h4("Cantidad de accidentes con alcohol"),
                                                 h4("Hace falta más conciencia en la gente de NO manejar Y tomar"),
                                                 plotOutput(outputId = "grafica_alcohol")
                                          ),
                                          column(6,
                                                 h4("Cantidad de accidentes con muerte"),
                                                 h4("Desafortunadamente no pasan más de tres meses sin alguna muerte"),
                                                 plotOutput(outputId = "grafica_muertes")
                                          )
                                      )
                             ),
                             tabPanel(title = "Tabla Datos",
                                      DT::dataTableOutput(outputId = "tabla_reporte_viales")
                             )
                         )
                     )
                 )
                 )
    ),
    h4("Datos de choques obtenidos de la página oficial del gobierno de San Pedro Garza García"),
    h4("Por favor leer documento de ReadMe para más información acerca de esta plataforma")
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        if(input$seleccionar_todo == 0) return(NULL) 
        else if (input$seleccionar_todo%%2 == 0)
        {
            updateCheckboxGroupInput(session,"anio_checkbox_cruces",
                                     "Indique los años que quiera ver",
                                     choices=sort(unique(accidentes_cruceros$anio))
                                     )
        }
        else
        {
            updateCheckboxGroupInput(session,"anio_checkbox_cruces",
                                     "Indique los años que quiera ver",
                                     selected = sort(unique(accidentes_cruceros$anio)),
                                     choices = sort(unique(accidentes_cruceros$anio))
                                     )
        }
    })
    
    reporte_cruces <- eventReactive(input$ejecutar, {
        #Obtener la cantidad total de choques por cruce.
        accidentes_cruceros %>%
            filter(anio %in% input$anio_checkbox_cruces)%>%
            group_by(calle1, calle2) %>%
            summarise(cantidad_de_choques = sum(Total)) %>%
            arrange(calle1)%>%
            ungroup()%>%
            #Unir reporte estadistico con las coordenadas
            left_join(coordenadas_cruces, by = c("calle1", "calle2"))%>%
            #Popup es para el despliegue en el mapa.
            mutate(popup = paste("<b>","Cantidad de Choques: ", cantidad_de_choques,"</b><br>",
                                 "Calle 1: ", calle1, "  <br>",
                                 "Calle 2: ", calle2, sep = '')
                   )
        })
    
    reporte_cruces_sf <- eventReactive(input$ejecutar,{
        reporte_cruces()%>%
        #Cambiar dataframe a objeto sf para desplegar en el mapa
        st_as_sf(coords = c("longitud", "latitud"),
                 crs = "+proj=longlat +datum=WGS84")
    })

    leaflet_map <- eventReactive(input$ejecutar,{
        #Desplegar mapa
        leaflet(data = reporte_cruces_sf())%>%
            addTiles() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(popup = ~popup,
                             radius = ~cantidad_de_choques**(1/2.5), 
                             color = "red",
                             stroke = 0, 
                             fillOpacity = 1)
    })
    
    output$mymap <- renderLeaflet({
        leaflet_map()
    })
    
    output$tabla_reporte_cruces <- DT::renderDataTable({
        DT::datatable(
            select(reporte_cruces(),-popup),
            colnames = c("Calle 1" = 'calle1', "Calle 2" = 'calle2',
                         "Choques Totales" = 'cantidad_de_choques' , "Latitud" = 'latitud',
                         "Longitud" = 'longitud'
                         )
            )
    })
    #######
    reporte_viales <- reactive({
        #Obtener la cantidad total de choques por cruce.
        info_accidentes %>%
            filter(mes_fecha >= input$rango_fechas_info[1],
                   mes_fecha <= input$rango_fechas_info[2])    
            })
    
    output$grafica_alcohol <- renderPlot({
        ggplot(data = filter(reporte_viales(), accidente_vial == "Alcohol"), aes(x = anio_mes, y = cantidad)) + 
            geom_bar(stat = "identity", fill = "gold2") +
            scale_x_yearmon(n = 5, format = "%B %Y") + 
            labs(x = "Tiempo", y = "Cantidad de Incidentes", 
                 title = "Línea negra es el promedio del periodo")+
            geom_hline(yintercept = mean(df$cantidad), color = "black", size = 1.25)+
            theme_bw()
        
    })
    
    output$grafica_menores <- renderPlot({
        df <- filter(reporte_viales(), accidente_vial == "Menores")
        
        ggplot(data = df, aes(x = anio_mes, y = cantidad)) + 
            geom_bar(stat = "identity", fill = "blue") +
            scale_x_yearmon(n = 5, format = "%B %Y") + 
            labs(x = "Tiempo", y = "Cantidad de Incidentes")+
            theme_bw()
        
    })
    
    output$grafica_lesionados <- renderPlot({
        df <- filter(reporte_viales(), accidente_vial == "Lesionado")
        
        ggplot(data = df, aes(x = anio_mes, y = cantidad)) + 
            geom_bar(stat = "identity", fill = "red") +
            scale_x_yearmon(n = 5, format = "%B %Y") + 
            labs(x = "Tiempo", y = "Cantidad de Incidentes", title = "Línea negro es el promedio del periodo") +
            geom_hline(yintercept = mean(df$cantidad), color = "black", size = 1.25)+
            theme_bw()
    })
    
    output$text0_alcohol <- renderText({
        
    })
    
    output$grafica_muertes <- renderPlot({
        df <- filter(reporte_viales(), accidente_vial == "Muertes")
        
        ggplot(data = df, aes(x = anio_mes, y = cantidad)) + 
            geom_bar(stat = "identity", fill = "darkred") +
            scale_x_yearmon(n = 5, format = "%B %Y") + 
            labs(x = "Tiempo", y = "Cantidad de Incidentes")+
            theme_bw()
    })
    
    output$tabla_reporte_viales <- DT::renderDataTable({
        DT::datatable(
            select(reporte_viales(), -anio_mes, -mes_fecha),
            colnames = c("Año" = 'anio', "Mes" = 'mes',
                         "Tipo de Accidente" = 'accidente_vial' , "Cantidad" = 'cantidad')
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

