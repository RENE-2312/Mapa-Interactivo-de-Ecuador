#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% shp cantones parroquias %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, veremos como trabajar a niveles de provincia, cantón y parroquia

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--- Librerias ----

library(data.table)   # Librería para el manejo eficiente de grandes volúmenes de datos.
library(tidyverse)    # Meta librería para el análisis y manipulación de datos en R.
library(jsonlite)     # Librería para trabajar con formatos JSON, utilizada para la lectura del mapa.
library(highcharter)  # Librería para la creación de gráficos interactivos utilizando la librería Highcharts.
library(sf)           # Librería para el manejo de datos espaciales, utilizada para operaciones espaciales.


#--- Lectura shp ----

# En esta caso, leemos el archivo shp de las parroquias del Ecuador y lo convertimos en un archivo GeoJSON.

shp_parr<-st_read("BDD/SHP/nxparroquias.shp")

#~~~ Conversión de un shp a geojson
file.remove("BDD/nxparroquias.json") # Eliminamos el archivo existente, si no existe, omitimos este paso
st_write(shp_parr, "BDD/nxparroquias.json", driver = "GeoJSON") # Conversión

#---- Carga de mapas del Ecuador -----

#~~~ Lectura del mapa del Ecuador por cantones, en formato lista
mapa.cantones.ec<-jsonlite::fromJSON("BDD/cantones-ecuador.geojson",
                                     simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por cantones, en formato data.frame
mapa_cantones_ec <- jsonlite::fromJSON("BDD/cantones-ecuador.geojson") %>% 
  as.data.frame()

#~~~ Lectura del mapa del Ecuador por parroquias, en formato lista
mapa.parroquias.ec <- jsonlite::fromJSON("BDD\\nxparroquias.json",
                                      simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por parroquias, en formato data.frame
mapa_parroquias_ec <- jsonlite::fromJSON("BDD\\nxparroquias.json")|>
  as.data.frame()

# Vamos a generar datos aleatorios para visualizar en el mapa. Para ello, extraeremos el código provincial,
# nombre provincial, código cantonal, nombre cantonal, código parroquial y nombre parroquial
# de la base de datos mapa_parroquias_ec. Si cuentas con una base de datos real, puedes incluirla aquí.
# Al trabajar a nivel de parroquias, se recomienda utilizar los códigos de identificación
# geográfica para realizar el cruce de datos entre bases, en lugar de recodificar los nombres de cada parroquia, 
# lo cual puede resultar tedioso. Los códigos provinciales, cantonales y parroquiales utilizados en el mapa del
# Ecuador corresponden al año 2011 y pueden ser consultados en la siguiente base de datos:

set.seed(12345)

datos<-data.frame(id_provincia= mapa_parroquias_ec$features.properties$DPA_PROVIN,
                  nombre_provincia = mapa_parroquias_ec$features.properties$DPA_DESPRO,
                  id_canton=mapa_parroquias_ec$features.properties$DPA_CANTON,
                  nombre_canton=mapa_parroquias_ec$features.properties$DPA_DESCAN,
                  id_parroquia= mapa_parroquias_ec$features.properties$DPA_PARROQ,
                  nombre_parroquia=mapa_parroquias_ec$features.properties$DPA_DESPAR,
                  valor=rpois(1040,80))


#--- Mapa Provincial ----

# En este caso graficaremos la información unicamente de la provincia de Pichincha (17) por sus cantones

# Función para identificar los valores correspondientes a una provincia en la base mapa.cantones.ec
indice_provincial <- unlist(lapply(mapa.cantones.ec$features, function (x){
  x$properties$DPA_PROVIN
}))

#~~~ Selección de los datos geográficos provinciales
# (Se extrae de mapa.cantones.ec la información geográfica de la provincia de interés)
cod_provincia<-"17"
mapa.cantones.subset <- mapa.cantones.ec
mapa.cantones.subset$features <- mapa.cantones.ec$features[which(indice_provincial == cod_provincia)]

#~~~ Selección de los datos estadísticos provinciales
# Nombre de la provincia seleccionada
provincia_interes<-datos|>
  dplyr::filter(id_provincia==cod_provincia)|>
  distinct(nombre_provincia)|>
  as.character()

# (Se extrae la información estadística de la base datos para la provincia de interés)
ec.prov <- datos|>filter(id_provincia == cod_provincia) %>% 
  group_by(id_canton,nombre_canton)|>
  summarise(valor=sum(valor))|>
  ungroup()|>
  transmute( code = id_canton, value = valor, canton_label = nombre_canton, prov_label = provincia_interes) %>%
  list_parse()

#~~~ Implementación del gráfico
highchart(type = 'map') %>%
  hc_chart(backgroundColor = "#161C20") %>%
  hc_add_series(
    mapData = mapa.cantones.subset,
    data = ec.prov,
    joinBy = c('DPA_CANTON',"code"),
    borderWidth = 0.8,
    dataLabels = list(enabled = FALSE, format = '{point.canton_label}'),
    tooltip = list( 
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.prov_label}</b>, <i> {point.canton_label} </i> <br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "",
      borderColor = "black",
      nullColor = "#e8e8e8"
    )) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis( minColor = "#1e8bb0", maxColor = "#ff4f4f")%>% 
  hc_exporting(enabled = TRUE)%>%
  hc_mapNavigation(enabled = TRUE)

#--- Mapa Cantonal ----

# En este caso graficaremos la información unicamente del cantón Quito de Pichincha (1701) por sus parroquias

# Función para identificar los valores correspondientes a un cantón en la base mapa.parroquias.ec
indice_cantonal <- unlist(lapply(mapa.parroquias.ec$features, function (x){
  x$properties$DPA_CANTON
}))

#~~~ Selección de los datos geográficos cantonales
# (Se extrae de mapa.parroquias.ec la información geográfica del cantón de interés)
cod_canton<-"1701"
mapa.parroquias.subset <- mapa.parroquias.ec
mapa.parroquias.subset$features <- mapa.parroquias.ec$features[which(indice_cantonal == cod_canton)]

#~~~ Selección de los datos estadísticos cantonales
# (Se extrae la información estadística de la base datos para el cantón de interés)
ec.canton <- datos|>filter(id_canton == cod_canton) %>% 
  transmute( code = id_parroquia, value = valor, canton_label =nombre_canton, parroq_label = nombre_parroquia) %>%
  list_parse()

#~~~ Implementación del gráfico
highchart(type = 'map') %>%
  hc_chart(backgroundColor = "#161C20") %>% 
  hc_add_series(
    mapData = mapa.parroquias.subset,
    data = ec.canton, 
    joinBy = c('DPA_PARROQ',"code"),
    borderWidth = 0.8, dataLabels = list(enabled = FALSE, format = '{point.parroq_label}'),
    tooltip = list( 
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.canton_label}</b>, <i>{point.parroq_label}</i><br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "",
      borderColor = "black",
      nullColor = "#e8e8e8" 
    )) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis( minColor = "#1e8bb0", maxColor = "#ff4f4f")%>% 
  hc_exporting(enabled = TRUE)%>%
  hc_mapNavigation(enabled = TRUE)

#--- Drilldown cantones- parroquias -----

# Vamos a graficar la información de una provincia por cantón, y el segundo 
# gráfico mostrará la información por parroquia dentro de ese cantón.

# Escogemos una provincia, en este caso Loja (11), por un buen amigo, Luis :)

cod_provincia<-"11"

# Datos de la provincia de interés
datos_provincia_desagregado<- datos|>
  filter(id_provincia == cod_provincia) %>%
  select(-c(id_provincia,nombre_provincia))

# Función para identificar los valores correspondientes a una provincia en la base mapa.cantones.ec
indice_provincial <- unlist(lapply(mapa.cantones.ec$features, function (x){
  x$properties$DPA_PROVIN
}))

#~~~ Selección de los datos geográficos provinciales
# (Se extrae de mapa.cantones.ec la información geográfica de la provincia de interés)
mapa.cantones.subset <- mapa.cantones.ec
mapa.cantones.subset$features <- mapa.cantones.ec$features[which(indice_provincial == cod_provincia)]

#~~~ Base del primer gráfico
# Nombre de la provincia seleccionada
provincia_interes<-datos|>
  dplyr::filter(id_provincia==cod_provincia)|>
  distinct(nombre_provincia)|>
  as.character()

# (Se extrae la información estadística de la base datos_provincia_desagregado para la provincia de interés por cantones)
datos_cantones.ts<-datos_provincia_desagregado|>
  group_by(id_canton,nombre_canton)|>
  summarise(valor=sum(valor))|>
  transmute(code = id_canton, value= valor, drilldown =id_canton, prov_label=provincia_interes, canton_label=nombre_canton)

datos_cantones.st<-list_parse(datos_cantones.ts)

#~~~ Base del segundo gráfico
# Función para identificar los valores correspondientes a un cantón en la base mapa.parroquias.ec
indice_cantonal <- unlist(lapply(mapa.parroquias.ec$features, function (x){
  x$properties$DPA_CANTON
}))

build_series <- function(cod_canton) {
  
  # En cod_canton se debe ingresar la variable de drilldown del gráfico principal
  
  # Selección de los datos geográficos cantonales
  mapa.parroquias.subset <- mapa.parroquias.ec
  mapa.parroquias.subset$features <- mapa.parroquias.ec$features[which(indice_cantonal == cod_canton)]
  
  # Selección de los datos estadísticos cantonales
  ec.canton <- datos_provincia_desagregado|>
    filter(id_canton == cod_canton) %>% 
    transmute( code = id_parroquia, value = valor, canton_label= nombre_canton, parroq_label = nombre_parroquia) %>%
    list_parse()
  
  # Construcción del mapa cantonal
  list(
    id = cod_canton,
    mapData = mapa.parroquias.subset,
    data = ec.canton,
    joinBy = c('DPA_PARROQ',"code"),
    dataLabels = list(enabled = FALSE, format = '{point.parroq_label'),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.canton_label}</b>, <i>{point.parroq_label}</i><br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "")
  )
}

# Se crean las series para cada cantón en base a la variable de drilldown
series.list <- lapply(datos_cantones.ts$drilldown, build_series)

#~~~ Implementación del gráfico
highchart(type = 'map') %>%
  hc_chart(backgroundColor = "#161C20") %>%
  hc_add_series(
    mapData = mapa.cantones.subset,
    data = datos_cantones.st, 
    joinBy = c("DPA_CANTON", "code"),
    borderWidth = 0.8, dataLabels = list(enabled = FALSE, format = '{point.canton_label}'),
    tooltip = list( 
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.prov_label}</b>, <i>{point.canton_label}</i><br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "",
      borderColor = "black",
      nullColor = "#e8e8e8"
    )) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis( minColor = "#1e8bb0", maxColor = "#ff4f4f")%>%
  hc_add_theme(hc_theme_smpl())%>% 
  hc_exporting(enabled = TRUE)%>%
  hc_title(text = "Información Estadística",style = list(color = "white")) %>%
  hc_subtitle(text = "Índice interno <br> Año 2024" ,style = list(color = "white")) %>% 
  hc_legend(title=list(text="Valor", 
                       style = list(color = "white")),
            layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom',
            itemStyle=list(color="white")) %>%
  hc_caption(text = " Resumen Ejecutivo <br> Empresa XXX",
             useHTML = TRUE, 
             style = list(color = "#999999", fontWeight = "normal", textAlign = "left"), 
             align = "left", 
             verticalAlign = "bottom")%>%
  # hc_mapNavigation(enabled = TRUE)%>%
  hc_drilldown(
    series = series.list
  )

rm(list=ls())
gc()
