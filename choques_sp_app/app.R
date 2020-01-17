#Autor: Rodrigo Benavides
#Fecha: Enero 17, 2019

#Objetivo del script es poder visualizar en un mapa donde es la ubicación con mayor cantidad de choques en San Pedro.
#Los datos son de Enero 2014 a Septiembre del 2016.
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(DT)

#Importar datos y cambiar formato de nombres de las calles
registro_accidentes <- read_csv("data/ACCIDENTES_CRUCEROS_SAN_PEDRO.csv")%>%
    rename_all(iconv)%>%
    rename(anio = "año") %>%
    mutate(calle1 = iconv(calle1),
           calle2 = iconv(calle2),
           fechaCorte = as.Date(fechaCorte, format = "%d/%m/%Y"))

#Quedarnos con los nombres de los cruces para despues pegar sus coordenadas
coordenadas_cruces <- registro_accidentes %>%
    distinct(calle1,calle2)%>%
    arrange(calle1, calle2)

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

registro_accidentes <- left_join(registro_accidentes, coordenadas_cruces, by = c("calle1", "calle2"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Choques en 20 cruces de San Pedro Garza Garcia"),
    h4("Datos de choques obtenidos de la página oficial del gobierno de San Pedro Garza García"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
                     checkboxGroupInput(inputId = "anio_checkbox",
                                        label = "Indique los años que quiera ver",
                                        selected = sort(unique(registro_accidentes$anio)),
                                        choices = sort(unique(registro_accidentes$anio))
                     ),
                     actionLink("seleccionar_todo","Seleccionar Todos"),
                     br(),
                     br(),
                     actionButton(inputId = "ejecutar",
                                  label = "Ejecutar"
                     ),
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
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        if(input$seleccionar_todo == 0) return(NULL) 
        else if (input$seleccionar_todo%%2 == 0)
        {
            updateCheckboxGroupInput(session,"anio_checkbox",
                                     "Indique los años que quiera ver",
                                     choices=sort(unique(registro_accidentes$anio))
                                     )
        }
        else
        {
            updateCheckboxGroupInput(session,"anio_checkbox",
                                     "Indique los años que quiera ver",
                                     selected = sort(unique(registro_accidentes$anio)),
                                     choices = sort(unique(registro_accidentes$anio))
                                     )
        }
    })
    
    reporte_cruces <- eventReactive(input$ejecutar, {
        #Obtener la cantidad total de choques por cruce.
        registro_accidentes %>%
            filter(anio %in% input$anio_checkbox)%>%
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
        DT::datatable(select(reporte_cruces(),-popup),
                       colnames = c("Calle 1" = 'calle1', "Calle 2" = 'calle2', 
                                    "Choques Totales" = 'cantidad_de_choques' , "Latitud" = 'latitud',
                                    "Longitud" = 'longitud'
                                    )
                      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
