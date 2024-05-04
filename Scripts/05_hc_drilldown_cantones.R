#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% hc_drilldown cantones %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, veremos cómo desplegar otro gráfico al hacer clic en el gráfico principal.
# En este caso, el gráfico secundario también es de tipo mapa.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--- Librerias ----

library(data.table)
library(tidyverse)
library(jsonlite)
library(highcharter) 

#--- Lectura datos ----

# El gráfico principal mostrará la información por provincia, mientras que en el segundo 
# gráfico se mostrará la información por cantón dentro de esa provincia.

#~~~ Lectura del mapa del Ecuador por provincias, en formato lista
mapa.ec <- jsonlite::fromJSON("BDD/ec-all.geo.json",
                              simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por provincias, en formato data.frame
mapa_ec <- jsonlite::fromJSON("BDD/ec-all.geo.json") %>% 
  as.data.frame()

#~~~ Lectura del mapa del Ecuador por cantones, en formato lista
mapa.cantones.ec<-jsonlite::fromJSON("BDD/cantones-ecuador.geojson",
                                     simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por cantones, en formato data.frame
mapa_cantones_ec <- jsonlite::fromJSON("BDD/cantones-ecuador.geojson") %>% 
  as.data.frame()

# Vamos a crear datos aleatorios para graficar en el mapa. Para ello, extraeremos el código provincial,
# nombre provincial, código cantonal y nombre cantonal de la base mapa_cantones_ec. Si dispones de una base
# de datos real, aquí se incluiría esa base y se recomienda que contenga al menos las 4 variables mencionadas.
# Cuando se trabaja a nivel cantonal, se recomienda utilizar los códigos provinciales y cantonales
# para el cruce de las bases, ya que recodificar los nombres de cada cantón puede ser tedioso. Los
# códigos provinciales y cantonales del mapa del Ecuador son del año 2011 y se pueden visualizar en la
# siguiente base:

set.seed(12345)

datos<-data.frame(id_provincia= mapa_cantones_ec$features.properties$DPA_PROVIN,
                  nombre_provincia = mapa_cantones_ec$features.properties$DPA_DESPRO,
                  id_canton=mapa_cantones_ec$features.properties$DPA_CANTON,
                  nombre_canton=mapa_cantones_ec$features.properties$DPA_DESCAN,
                  valor=rpois(224,80))

#--- Preparación: Datos Gráfico Principal ----

datos_provinciales<-datos|>
  group_by(id_provincia,nombre_provincia)|>
  summarise(valor=sum(valor))|>
  ungroup()|>
  as.data.table()

datos.provinciales.ts<-datos_provinciales|>
  transmute(code = id_provincia, value= valor, drilldown =id_provincia, prov_label=nombre_provincia)

datos.provinciales.st<-list_parse(datos.provinciales.ts)

#--- Preparación: Datos Gráfico Secundario ----

# Función para identificar los valores correspondientes a una provincia en la base mapa.cantones.ec
indice_provincial <- unlist(lapply(mapa.cantones.ec$features, function (x){
  x$properties$DPA_PROVIN
}))

build_series <- function(cod_provincia) {
  
  # cod_provincia <- "01" # Aquí se debe ingresar la variable de drilldown del gráfico principal
  
  # Selección de los datos geográficos provinciales
  # (Se extrae de mapa.cantones.ec la información geográfica de la provincia de interés)
  mapa.cantones.ec.subset <- mapa.cantones.ec
  mapa.cantones.ec.subset$features <- mapa.cantones.ec$features[which(indice_provincial == cod_provincia)]
  
  # Selección de los datos estadísticos provinciales
  # (Se extrae la información estadística de la base datos para la provincia de interés)
  ec.prov <- datos %>%
    filter(id_provincia == cod_provincia) %>% 
    transmute( code = id_canton, 
               value = valor, 
               prov_label = nombre_provincia, 
               canton_label = nombre_canton) %>%
    list_parse()
  
  # Construcción del mapa provincial
  list(
    id = cod_provincia, # Variable de drilldown
    mapData = mapa.cantones.ec.subset, # Mapa provincial
    data = ec.prov, # Datos estadísticos en formato lista
    joinBy = c('DPA_CANTON', "code"), # Realiza un cruce de bases, donde DPA_CANTON y code son las variables que contienen los códigos de los cantones en las bases mapa.cantones.ec.subset y ec.prov, respectivamente
    dataLabels = list(enabled = FALSE, format = '{point.code}'),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.prov_label} </b>, <i>{point.canton_label}</i><br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "")
  )
}

# Se crean las series para cada provincia en base a la variable de drilldown
series.list <- lapply(datos.provinciales.ts$drilldown, build_series)

#--- Implementación del gráfico ----

ecmap<- highchart(type = 'map') %>%
  hc_chart(backgroundColor = "#161C20") %>% # Color del fondo del gráfico
  hc_add_series(
    mapData = mapa.ec,
    data = datos.provinciales.st, 
    joinBy = c("fips", "code"), # Realiza un cruce de bases, donde fips y code son las variables que contienen los códigos de las provincias en las bases mapa.ec y datos.provinciales.st, respectivamente
    borderWidth = 0.8, 
    dataLabels = list(enabled = FALSE, format = '{point.code}'),
    tooltip = list( 
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = paste0("<b>{point.prov_label}</b><br>",
                           "<b style=\"color:#1518AF\"> Valor:</b> {point.value:,.0f}<br>"),
      footerFormat = "",
      borderColor = "black",
      nullColor = "#e8e8e8"  # Color para regiones con valores nulos o no presentes en la base
    )
    ) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis( minColor = "#1e8bb0", 
                maxColor = "#ff4f4f")%>% 
  hc_exporting(enabled = TRUE)%>%
  hc_add_theme(hc_theme_smpl())%>%
  #hc_mapNavigation(enabled = TRUE)%>%
  hc_title(text = "Información Estadística",style = list(color = "white")) %>%
  hc_subtitle(text = "Índice interno <br> Año 2024" ,style = list(color = "white")) %>% 
  hc_legend(title=list(text="Valor", # Nombre de la leyenda
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
  hc_drilldown(
    series = series.list # Serie de las provincias
  )

ecmap

rm(list=ls())
gc()
