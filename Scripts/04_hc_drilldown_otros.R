#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% hc_drilldown otros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, veremos cómo desplegar otro gráfico al hacer clic en el gráfico principal.
# En este caso, el gráfico secundario no es de tipo mapa.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--- Librerias ----

library(data.table)
library(tidyverse)
library(jsonlite)
library(highcharter) 

#--- Lectura datos ----

# En el gráfico principal se muestra la información de la cantidad total de afiliados por provincia.
# Mientras que en el segundo gráfico se presenta el desglose por tipo de afiliado.

#~~~ Lectura del archivo afiliados.RData
load(file="BDD/afiliados.RData")

#~~~ Lectura del mapa del Ecuador por provincias, en formato lista
mapa.ec <- jsonlite::fromJSON("BDD/ec-all.geo.json",
                              simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por provincias, en formato data.frame
mapa_ec <- jsonlite::fromJSON("BDD/ec-all.geo.json") %>% 
  as.data.frame()

#--- Preparación: Base gráfico principal ----

# Se crea una nueva base de datos, en la que se definen las siguientes variables:
# - code: Representa los nombres de las provincias.
# - value: Corresponde a la variable que se graficará.
# - drilldown: Indica la variable de drilldown, en este caso, la provincia.
# Esta variable determina qué sucede al hacer clic en una provincia 
# (en este caso, despliega otro gráfico al hacer click en una provincia).

afiliados.ts <- afiliados %>%
  transmute(code = Provincia,
            value = Total_Afiliados,
            drilldown = Provincia)

# Se convierte la base de datos anterior a un formato de lista
afiliados.st <- list_parse(afiliados.ts)

# --- Principal: mapa - Secundario: anillo ----

# En este caso, el segundo gráfico será uno de anillo

#~~~ Base segundo gráfico

# Preparación de la información por provincia para el gráfico secundario
afiliados_prov<-afiliados|>
  select(Provincia,A_SSC,A_TNRH,A_CSDependencia)|>
  pivot_longer(c(A_SSC,A_TNRH,A_CSDependencia),names_to = "Tipo_Afiliado")|>
  rename(Cantidad=value)|>
  mutate(Tipo_Afiliado:=case_when(Tipo_Afiliado=="A_SSC" ~ "SSC",
                                  Tipo_Afiliado=="A_TNRH" ~ "TNRH",
                                  TRUE ~ "SGO"))|>
  as.data.table()

# Generación de la base en el formato adecuado para el segundo gráfico
afiliados_drilldown<-afiliados_prov|>
  rename(value=Cantidad)|> # Para poder visualizar la información de la variable Cantidad en la ventana emergente, se la renombra con value
  select(Provincia,Tipo_Afiliado,value)|>
  group_nest(Provincia)|>
  mutate(
    id=Provincia, # En id, se debe colocar la variable de drilldown del primer gráfico.
    type="pie", # Se especifica el tipo de gráfico
    data=map(data,mutate,name=Tipo_Afiliado,y=value),
    data=map(data,list_parse),
    innerSize="90%" # Área que ocupa el circulo interno del anillo
  )

#~~~ Implementación del gráfico
highchart(type = 'map') %>%
  hc_add_series(
    mapData = mapa.ec, # mapa.ec, es la base del mapa en formato json
    data = afiliados.st,  # Base del gráfico principal en formato lista
    joinBy = c("name", "code"), # Realiza un cruce de bases, donde name y code son las variables que contienen los nombres de las provincias en las bases mapa.ec y afiliados.st, respectivamente
    name="Mapa",
    borderWidth = 0.8, 
    dataLabels = list(enabled = FALSE)
    )%>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis(stops = color_stops(5, c("#FF69B4", "#800080")))%>% 
  # hc_mapNavigation(enabled = TRUE)%>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>%
  hc_subtitle(text = "Afiliados <br> Año 2022") %>% 
  hc_legend(layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom')%>%
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE",
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom")%>%
  hc_drilldown( # Aquí, se añade la información para el segundo gráfico
    allowPointDrilldown = TRUE,
    series = list_parse(afiliados_drilldown) # Aquí, va la base del segundo gráfico, igualmente en formato lista
  )%>%
  hc_tooltip(
    formatter = JS( # Con este codigo JavaScript, se logra que se muestre el valor de la variable y el porcentaje en ambos gráficos
      "function() {
        var pointValue = this.point.value;
        var total = this.series.data.reduce((sum, point) => sum + point.value, 0);
        var percentage = (pointValue / total) * 100;
        return '<b>' + this.point.name + '</b>: ' + pointValue.toLocaleString() + ' (' + percentage.toFixed(2) + '%)';
      }"
    ),
    useHTML = TRUE,
    valueDecimals = 0
  )

# # Si solo desea valores, utilice este hc_tooltip: 
# hc_tooltip(formatter = JS("function() {
#                         return '<b>' + this.point.name + '</b>: ' + this.point.value.toLocaleString();
#                     }"))

# --- Principal: mapa -  Secundario: column ----

# En este caso, el segundo gráfico será uno de columnas

#~~~ Base segundo gráfico

# Lo unico que cambia es que se especifica el tipo de grafico al construir la segunda base
afiliados_drilldown<-afiliados_prov|>
  rename(value=Cantidad)|>
  select(Provincia,Tipo_Afiliado,value)|>
  arrange(desc(value))|>
  group_nest(Provincia)|>
  mutate(
    id=Provincia,
    type="column",
    data=map(data,mutate,name=Tipo_Afiliado,y=value),
    data=map(data,list_parse),
  )

#~~~ Implementación del gráfico
highchart(type = 'map') %>%
  hc_add_series(
    mapData = mapa.ec, 
    data = afiliados.st, joinBy = c("name", "code"),
    name="Mapa",
    borderWidth = 0.8, 
    dataLabels = list(enabled = FALSE)
    )%>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis(stops = color_stops(5, c("#FF69B4", "#800080")))%>% 
  # hc_mapNavigation(enabled = TRUE)%>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>%
  hc_subtitle(text = "Afiliados <br> Año 2022") %>% 
  hc_legend(layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom')%>% 
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE",
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom")%>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(afiliados_drilldown)
  )%>%
  hc_tooltip(
    formatter = JS(
      "function() {
        var pointValue = this.point.value;
        var total = this.series.data.reduce((sum, point) => sum + point.value, 0);
        var percentage = (pointValue / total) * 100;
        return '<b>' + this.point.name + '</b>: ' + pointValue.toLocaleString() + ' (' + percentage.toFixed(2) + '%)';
      }"
    ),
    useHTML = TRUE,
    valueDecimals = 0
  )%>%
  hc_xAxis(type = "category")# Dado que la variable Tipo_Afiliado es categórica, se utiliza esta línea para mostrar las categorías en el eje.

rm(list=ls())
gc()
