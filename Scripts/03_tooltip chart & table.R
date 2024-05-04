#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% tooltip_chart & tooltip_table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, veremos como  añadir una tabla y un gráfico en la ventana emergente que 
# se despliega al pasar el cursor sobre el gráfico principal, mediante las funciones
# tooltip_chart & tooltip_table

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# Graficaremos el total de afiliados por provincia en el mapa del Ecuador, con un gráfico de anillo 
# en la ventana emergente para visualizar el desglose por tipo de afiliado, además de una tabla en la
# que se mostrara la provincia y su total de afiliados

#--- Gráfico emergente: anillo ----

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
  select(Provincia, Tipo_Afiliado, Cantidad) %>% 
  arrange(Cantidad) %>% 
  group_nest(Provincia) %>% 
  mutate(
    data = map(data, mutate, name = Tipo_Afiliado, y = Cantidad, drop = TRUE), 
    data = map(data, list_parse)
  ) %>%
  rename(ttdata = data) 


# Combinación de las bases del gráfico principal y el de la ventana emergente
afiliados_tool <- afiliados %>%
  left_join(afiliados_temp, by = "Provincia")

#--- Tabla ----

# Se genera la tabla con la información a mostrar en la ventana emergente. En este caso, la tabla se
# crea directamente en lenguaje HTML, se puede hacer también con tooltip_table

library(glue)

# Creamos una nueva variable (tttext) en la base de datos afiliados_tool para almacenar la tabla HTML 
# que se agregará en la ventana emergente.

afiliados_tool <- afiliados_tool %>% 
  mutate(tttext = glue("<table style='font-size: 11px; border: 1px solid black; border-collapse: collapse;'>\n",
                       "  <tr style='border: 1px solid black;'>\n",
                       "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Provincia</th>\n",
                       "    <td style='border: 1px solid black; text-align: center;'>{Provincia}</td>\n",
                       "  </tr>\n",
                       "  <tr style='font-size: 11px; border: 1px solid black;'>\n",
                       "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Afiliados Totales</th>\n",
                       "    <td style='border: 1px solid black; text-align: center;'>{format(Total_Afiliados, big.mark = ' ', scientific = FALSE)}</td>\n",
                       "  </tr>\n",
                       "</table>"))

# !!! Importante: Al crear la tabla, ya no es necesario utilizar point. para acceder a los 
# valores de la base de datos, estos se extraen utilizando la función glue.

# #~~~ Ejemplo: tooltip_table
# # Creamos la tabla usando tooltip_table, descomente este codigo y corralo, lo demás sigue igual 
# x <- c("Provincia", "Total_Afiliados")
# y <- str_c("{point.", x, "}")
# x <- str_replace_all(x, "_", " ")
# tt <- tooltip_table(x, y,
#                       style="font-size: 11px; background-color: #b3e5fc; border-top: 1px solid black; border-bottom: 1px solid black; text-align: center;")
# 
# tt
# 
# # Removemos los point. de la tabla creada
# tt<-tt%>% 
#   str_remove_all("point\\.")
# 
# # Se crea una variable (tttext) en la base con los datos a graficar
# afiliados_tool<-afiliados_tool|>mutate(
#   tttext = str_glue_data(afiliados_tool, tt) # Se concatena la información de varias columnas y se utiliza para llenar la tabla con los datos correspondientes para cada provincia
# )

#--- Gráfico Final ----

highchart() %>% 
  hc_add_series_map(map = mapa.ec,
                    df = afiliados_tool,
                    value = "Total_Afiliados",
                    joinBy = c("name", "Provincia"),
                    dataLabels = list(
                      enabled = TRUE,
                      format = '{point.value}',
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
  hc_exporting(enabled = TRUE, filename ="Estadisticas_Afiliados")%>% # Hasta aquí como se ha venido trabajando
  hc_tooltip(
    useHTML = TRUE,
    headerFormat =  "<b>Información Afiliados</b>",
    pointFormatter = tooltip_chart( # Para agregar el gráfico en la subventana
      width=250, 
      height = 250,
      accesor = "ttdata", 
      hc_opts = list(
        chart = list(type = "pie"), 
        title = list(text = "point.tttext", useHTML = TRUE, align="center"), # Se utiliza la variable tttext, que contiene las tablas HTML en afiliados_tool en el parámetro title
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
  )

rm(list=ls())
gc()
