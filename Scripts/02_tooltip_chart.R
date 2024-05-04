#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% tooltip_chart %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, aprenderemos a agregar un gráfico adicional en la ventana emergente que aparece 
# al pasar el cursor sobre el gráfico principal, utilizando la función tooltip_chart.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--- Librerias ----

library(data.table)
library(tidyverse)
library(jsonlite) 
library(highcharter) 

#--- Lectura de datos -----

#~~~ Lectura del mapa del Ecuador por provincias, en formato lista
mapa.ec <- jsonlite::fromJSON("BDD/ec-all.geo.json",
                              simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por provincias, en formato data.frame
mapa_ec <- jsonlite::fromJSON("BDD/ec-all.geo.json") %>% 
  as.data.frame()

#~~~ Lectura del archivo afiliados.RData
load(file="BDD/afiliados.RData")

#--- Gráfico emergente: anillo ----

# Graficaremos el total de afiliados por provincia en el mapa del Ecuador, con un gráfico de anillo 
# en la ventana emergente para visualizar el desglose por tipo de afiliado.

# Preparación de la información por provincia para el gráfico emergente
afiliados_prov <- afiliados %>%
  select(Provincia, A_SSC, A_TNRH, A_CSDependencia) %>%
  pivot_longer(cols = c(A_SSC, A_TNRH, A_CSDependencia), names_to = "Tipo_Afiliado") %>%
  rename(Cantidad = value) %>%
  mutate(Tipo_Afiliado = case_when(
    Tipo_Afiliado == "A_SSC" ~ "SSC",
    Tipo_Afiliado == "A_TNRH" ~ "TNRH",
    TRUE ~ "SGO"
  )) %>%
  as.data.table()

# Generación de la base en el formato adecuado para el gráfico de la ventana emergente
afiliados_temp <- afiliados_prov %>%
  select(Provincia, Tipo_Afiliado, Cantidad) %>% # Seleccionamos las variables necesarias
  arrange(Cantidad) %>% 
  group_nest(Provincia) %>% # Los datos se almacenan en una variable llamada data en formato de lista tibble por Provincia
  mutate(
    data = map(data, mutate, name = Tipo_Afiliado, y = Cantidad, drop = TRUE), # En la base tibble para cada provincia, creamos 2 variables, name= variable no numérica & y = variable numérica. Además, se descartan las demás variables
    data = map(data, list_parse) # Se le da un formato de lista parseada a la columna data por provincia
  ) %>%
  rename(ttdata = data) # Renombramos la columna data


# Combinación de las bases del gráfico principal y el de la ventana emergente
afiliados_tool <- afiliados %>%
  left_join(afiliados_temp, by = "Provincia")

# Generamos el mapa del Ecuador con el gráfico dentro de la ventana emergente utilizando tooltip_chart en hc_tooltip
highchart() %>% 
  hc_add_series_map(map = mapa.ec,
                    df = afiliados_tool,
                    value = "Total_Afiliados",
                    joinBy = c("name", "Provincia"),
                    dataLabels=list(enabled=TRUE, # ¿Deseas mostrar algo sobre las provincias?
                                    format='{point.value:,.0f}', # Para mostrar el valor de la variable graficada
                                    style = list(
                                      fontSize = "8px" # Tamaño de las etiquetas
                                    )
                                    ),
                    states = list(hover = list(color='#75EAC8')),
                    borderColor = "#FFFFFF")%>% 
  hc_colorAxis(stops = color_stops(3, c("#C0D6F5","#3A30FB", "#595877"))) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>%
  hc_subtitle(text = "Afiliados Totales <br> Año 2022") %>% 
  hc_legend(layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom')%>%
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE",
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom")%>%
  hc_mapNavigation(enabled = TRUE)%>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_exporting(enabled = TRUE, filename ="Estadisticas_Afiliados")%>% # Hasta aquí como se ha venido trabajando
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b> ", # Encabezado del grafico, con point.key accedemos al nombre de la provincia
    pointFormatter = tooltip_chart(
      width=250, # ancho de la  ventana emergente
      height = 200, # altura de la  ventana emergente
      accesor = "ttdata", # Nombre de la columna en la base de datos afiliados_tool utilizada para el gráfico emergente
      hc_opts = list( # Aquí se personaliza el gráfico emergente
        chart = list(type = "pie"), # tipo de gráfico
        title = list(text = "Tipo Afiliado", align = "center"), # Alineación del título
        plotOptions = list(
          pie = list(
            innerSize = "80%", # Área que ocupa el circulo interior del gráfico de anillo
            dataLabels = list(
              format = '<b>{point.name}</b>: {point.percentage:.1f}%',# Con esto se muestra el nombre de la categoria así como el porcentaje correspondiente, si se lo quita, únicamente se muestra el nombre
              style = list(
                fontSize = "8px" # tamaño de las etiquetas
              )
            )
          )
        )
      )
    )
  )

#--- Gráfico emergente: anillo personalizado----

# En este gráfico, el valor de la variable a graficar no se muestra inicialmente al cargar el gráfico.
# Sin embargo, al pasar el cursor sobre una provincia, el valor se visualiza. Esto se logra mediante el
# código JavaScript añadido después del último %>%. Además, al establecer display="none" en style de 
# dataLabels, los valores se ocultan inicialmente. Este enfoque puede aplicarse también a gráficos
# de columnas y otros tipos al trabajar con mapas para mostrar el valor de la variable graficada.

highchart() %>% 
  hc_add_series_map(map = mapa.ec,
                    df = afiliados_tool,
                    value = "Total_Afiliados",
                    joinBy = c("name", "Provincia"),
                    dataLabels = list(
                      enabled = TRUE,
                      format = '{point.value:,.0f}',
                      style = list(
                        fontSize = "10px",
                        display = "none"  # Inicialmente ocultamos los dataLabels
                      )
                    ),
                    states = list(hover = list(color='#75EAC8')),
                    borderColor = "#FFFFFF")%>% 
  hc_colorAxis(stops = color_stops(3, c("#C0D6F5","#3A30FB", "#595877"))) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>%
  hc_subtitle(text = "Afiliados Totales <br> Año 2022") %>% 
  hc_legend(layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom')%>%
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE",
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom")%>%
  hc_mapNavigation(enabled = TRUE)%>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_exporting(enabled = TRUE, filename ="Estadisticas_Afiliados")%>% # Hasta aquí como se ha venido trabajando
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b> ", 
    pointFormatter = tooltip_chart( 
      width=250, 
      height = 200, 
      accesor = "ttdata", 
      hc_opts = list( 
        chart = list(type = "pie"), 
        title = list(text = "Tipo Afiliado", align = "center"), 
        plotOptions = list(
          pie = list(
            innerSize = "80%",
            dataLabels = list(
              format = '<b>{point.name}</b>: {point.percentage:.1f}%',
              style = list(
                fontSize = "8px"
              )
            )
          )
        )
      )
    )
  )%>%
  hc_chart(events = list(
    load = JS("function() {
      var chart = this;
      chart.series[0].points.forEach(function(point) {
        point.graphic.on('mouseover', function() {
          point.dataLabel.css({display: 'block'});
        });
        point.graphic.on('mouseout', function() {
          point.dataLabel.css({display: 'none'});
        });
      });
    }")
  )) 

#--- Gráfico emergente: barras ----

# Graficaremos la cantidad total de afiliados en el mapa del Ecuador. Además, en la 
# ventana emergente, se mostrará un gráfico de barras desglosado por las categorías de afiliados.

highchart() %>% 
  hc_add_series_map(map = mapa.ec,
                    df = afiliados_tool,
                    value = "Total_Afiliados",
                    joinBy = c("name", "Provincia"),
                    dataLabels = list(
                      enabled = TRUE,
                      format = '{point.value:,.0f}',
                      style = list(
                        fontSize = "10px",
                        display = "none"
                      )
                    ),
                    states = list(hover = list(color='#75EAC8')),
                    borderColor = "#FFFFFF")%>% 
  hc_colorAxis(stops = color_stops(3, c("#C0D6F5","#3A30FB", "#595877"))) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>%
  hc_subtitle(text = "Afiliados Totales <br> Año 2022") %>% 
  hc_legend(layout= 'vertical',
            align= 'right',
            verticalAlign= 'bottom')%>%
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE",
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom")%>%
  hc_mapNavigation(enabled = TRUE)%>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_exporting(enabled = TRUE, filename ="Estadisticas_Afiliados")%>% 
  hc_tooltip(
    borderWidth=2, # ancho del borde de la ventana
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b>",
    pointFormatter = tooltip_chart(
      width = 200, 
      height = 225,
      accesor = "ttdata",
      hc_opts = list(
        chart = list(type = "bar"),  # Se cambia a un grafico de barras
        title = list(text = "Tipo Afiliado", align = "center"),
        yAxis = list(
          title=list(text="Cantidad",
                     style=list(fontSize="8px")),
          labels = list(
            style = list(
              fontSize = "8px"  # Para ajustar el tamaño de las etiquetas del eje 
            )
          )
        ),
        xAxis = list(
          type = "category",# Como la variable name (Tipo_Afiliado) es no numérica, se pone type="category" para poder visualizar las categorias en los ejes
          labels = list(
          style = list(
            fontSize = "8px"  # Para ajustar el tamaño de las etiquetas del eje 
            )
          )
        ) 
      )
    )
  )%>%
  hc_chart(events = list(
    load = JS("function() {
      var chart = this;
      chart.series[0].points.forEach(function(point) {
        point.graphic.on('mouseover', function() {
          point.dataLabel.css({display: 'block'});
        });
        point.graphic.on('mouseout', function() {
          point.dataLabel.css({display: 'none'});
        });
      });
    }")
  )) 

rm(list=ls())
gc()
