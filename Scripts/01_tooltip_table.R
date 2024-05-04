#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% tooltip_table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, veremos cómo agregar una tabla dentro de la ventana emergente que aparece al pasar 
# el cursor sobre el gráfico, utilizando la función tooltip_table

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--- Librerias ----

library(data.table) # Maneja grandes volúmenes de datos
library(tidyverse) # Meta librería para análisis de datos
library(jsonlite) # Librería para trabajar con formatos de tipo JSON (lectura del mapa)
library(highcharter) # Librería para gráficos interactivos
library(readxl) # Para leer archivos Excel

#--- Lectura de datos ----

#~~~ Lectura del mapa del Ecuador por provincias, en formato lista
mapa.ec <- jsonlite::fromJSON("BDD/ec-all.geo.json",
                              simplifyVector = FALSE)

#~~~ Lectura del mapa del Ecuador por provincias, en formato data.frame
mapa_ec <- jsonlite::fromJSON("BDD/ec-all.geo.json") %>% 
  as.data.frame()

#~~~ Lectura del archivo IESS_DAIE_capitulo_asegurados_bol_2022.xlsx

# Vamos a trabajar con la hoja 3 de este archivo, que contiene información de afiliados y pensionistas del IESS por provincia para el año 2022
asegurados <- read_excel("BDD/IESS_DAIE_capitulo_asegurados_bol_2022.xlsx", sheet = 3, range = "A8:N36", col_names = FALSE)

asegurados_variables <- c("Provincia", 
                          "A_SSC", "A_TNRH", "A_CSDependencia", "A_SVoluntario", "Total_Afiliados",
                          "P_SSC", "P_IVM", "A_Riesgos", "Total_Pensionistas",
                          "Total_AP",
                          "Dep_SSC", "Cobertura_Salud", "R_Asegurados")

colnames(asegurados) <- asegurados_variables
asegurados <- asegurados |> slice(1:24) # Nos quedamos únicamente con la información de Ecuador
asegurados <- asegurados |> as.data.table()

# Recodificación de las Provincias

# Se recodifican los nombres de las provincias en la base asegurados para que coincidan con los nombres 
# de las provincias en el archivo ec-all.geo.json, para poder graficar la información en el mapa del Ecuador.
# Se acceden a los nombres de las provincias en el archivo JSON a través de: 

mapa_ec$features.properties$name

asegurados$Provincia

asegurados[,Provincia:=case_when(Provincia=="Bolívar" ~ "Bolivar",
                                 Provincia=="Galápagos" ~ "Galapagos",
                                 Provincia=="Manabí" ~ "Manabi",
                                 Provincia=="Santo Domingo" ~ "Santo Domingo De Los Tsachilas",
                                 Provincia=="Sucumbíos" ~ "Sucumbios",
                                 Provincia=="Tunguragua" ~ "Tungurahua",
                                 TRUE ~ Provincia)]

# Vamos a trabajar únicamente con la información de afiliados por provincia
afiliados<-asegurados|>select(Provincia,A_SSC,A_TNRH,A_CSDependencia)
afiliados[,Total_Afiliados:=A_SSC+A_TNRH+A_CSDependencia]

# Guardamos la base de afiliados en un archivo RData en la carpeta BDD ya que se la va a usar a lo largo del proyecto
save(afiliados, file = "BDD/afiliados.RData")
rm("asegurados") # Eliminamos la base 'asegurados'

#--- Mapa del Ecuador: escala tricolor ----

# Graficamos el total de afiliados por provincia en el mapa del Ecuador

mapa_afiliados <- highchart() %>% 
  hc_add_series_map(map = mapa.ec, # Mapa del Ecuador
                    df = afiliados, # Datos a graficar
                    name= 'Afiliados Totales', 
                    value = "Total_Afiliados", # Variable de la base df a graficar en el mapa
                    joinBy = c("name", "Provincia"), # Emparejamiento de la información del mapa (map) y los datos (df), name es donde se almacenan los nombres de las provincias en map y Provincia es la variable que almacena los nombres de las provincias en df
                    states = list(hover = list(color='#75EAC8')),
                    borderColor = "#FFFFFF") %>% 
  hc_colorAxis(stops = color_stops(3, c("#C0D6F5","#3A30FB", "#595877"))) %>% 
  hc_title(text = "Estadísticas del Sistema de Pensiones") %>% 
  hc_subtitle(text = "Afiliados Totales <br> Año 2022") %>%  
  hc_legend(layout= 'vertical', 
            align= 'right',
            verticalAlign= 'bottom') %>% 
  hc_caption(text = "Boletín Estadístico <br> IESS - DAIE", 
             useHTML = TRUE, 
             align = "left", 
             verticalAlign = "bottom") %>% 
  hc_mapNavigation(enabled = TRUE) %>%  
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_exporting(enabled = TRUE, filename ="Estadisticas_Afiliados")

mapa_afiliados

#--- Mapa del Ecuador: tabla ----

# En el gráfico anterior, se añadirá una tabla en la ventana emergente que aparece al pasar el cursor sobre una provincia. 
# Esta tabla muestra el nombre de la provincia y la cantidad de afiliados: SSC, SGO, TNRH y Total.

style <- "background-color: #b3e5fc; border-top: 1px solid black; border-bottom: 1px solid black;" # Estilo CSS para la tabla
tt <- tooltip_table(# Con tooltip_table se crea una tabla en lenguaje HTML
  x = c("Provincia", "Afiliados SSC", "Afiliados SGO","Afiliados TNRH","Afiliados Totales"),
  y = c("{point.Provincia}", "{point.A_SSC:,.0f}", "{point.A_CSDependencia:,.0f}", "{point.A_TNRH:,.0f}", "{point.value:,.0f}"), 
  style = style
)

tt

# Al usar {point.nombre_variable}, se extrae el valor de esa variable para ese punto, en este caso, para la provincia
# en la que se encuentre el cursor.
# !!! Importante: Para acceder al valor de la variable que se está graficando en el mapa, se utiliza point.value

# Al grafico mapa_afiliados, se le añade la tabla tt en la ventana emergente
mapa_afiliados%>%
  hc_tooltip(pointFormat = tt, # En el parámetro pointFormat de hc_tooltip se especifica la tabla tt creada
             useHTML = TRUE,
             headerFormat = "")

#--- Mapa del Ecuador: tabla personalizada ----

# Se agregará la misma tabla pero con un formato más personalizado, escribiéndola directamente en lenguaje HTML.

tt <- str_c(
  "<table style='border: 1px solid black; border-collapse: collapse;'>\n",
  "  <tr style='border: 1px solid black;'>\n",
  "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Provincia</th>\n",
  "    <td style='border: 1px solid black;'>{point.Provincia}</td>\n",
  "  </tr>\n",
  "  <tr style='border: 1px solid black;'>\n",
  "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Afiliados SSC</th>\n",
  "    <td style='border: 1px solid black;'>{point.A_SSC:,.0f}</td>\n",
  "  </tr>\n",
  "  <tr style='border: 1px solid black;'>\n",
  "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Afiliados SGO</th>\n",
  "    <td style='border: 1px solid black;'>{point.A_CSDependencia:,.0f}</td>\n",
  "  </tr>\n",
  "  <tr style='border: 1px solid black;'>\n",
  "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Afiliados TNRH</th>\n",
  "    <td style='border: 1px solid black;'>{point.A_TNRH:,.0f}</td>\n",
  "  </tr>\n",
  "  <tr style='border: 1px solid black;'>\n",
  "    <th style='border: 1px solid black; background-color: #b3e5fc;'>Afiliados Totales</th>\n",
  "    <td style='border: 1px solid black;'>{point.value:,.0f}</td>\n",
  "  </tr>\n",
  "</table>"
)

tt

mapa_afiliados%>%
  hc_tooltip(pointFormat = tt, useHTML = TRUE,
             headerFormat = "")

rm(list=ls())
gc()
