#Autor: Rodrigo Benavides
#Fecha: Enero 16, 2020

#Objetivo del script es poder visualizar en un mapa donde es la ubicación con mayor cantidad de choques en San Pedro.
#Los datos son de Enero 2014 a Septiembre del 2016.
library(tidyverse)
library(leaflet)
library(sf)

#Importar datos y cambiar formato de nombres de las calles
registro_accidentes <- read_csv("data/ACCIDENTES_CRUCEROS_SAN_PEDRO.csv")%>%
  rename_all(iconv) %>%
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

#Obtener la cantidad total de choques por cruce.
reporte_cruces <- registro_accidentes %>%
  group_by(calle1, calle2) %>%
  summarise(cantidad_de_choques = sum(Total),
            por_mes = round(cantidad_de_choques/6, digits = 1)) %>%
  arrange(desc(cantidad_de_choques))%>%
  ungroup()%>%
  #Unir reporte estadistico con las coordenadas
  left_join(coordenadas_cruces, by = c("calle1", "calle2"))%>%
  #Popup es para el despliegue en el mapa.
  mutate(popup = paste("<b>","Cantidad de Choques: ", cantidad_de_choques,"</b><br>",
                       "Calle 1: ", calle1, "  <br>",
                       "Calle 2: ", calle2, sep = '')
  )%>%
  #Cambiar dataframe a objeto sf para desplegar en el mapa
  st_as_sf(coords = c("longitud", "latitud"),
           crs = "+proj=longlat +datum=WGS84")

#Desplegar mapa
leaflet(data = reporte_cruces)%>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(popup = ~popup,
                   radius = ~cantidad_de_choques**(1/2.5), 
                   color = "red",
                   stroke = 0, 
                   fillOpacity = 1)
