# Mapa Interactivo de Ecuador

Este repositorio muestra cómo visualizar y personalizar el mapa del Ecuador utilizando la librería `highcharter` de R.

## Personalización del Mapa

Este repositorio aborda las siguientes herramientas de personalización:

- `hc_tooltip`: Permite incluir tablas y gráficos en la ventana emergente al pasar el cursor sobre el mapa.
- `hc_drilldown`: Permite desplegar gráficos adicionales al hacer clic en una zona geográfica (provincia, cantón o parroquia).
  
## Ejemplos

A continuación, se muestran algunos ejemplos de los mapas que se generan en este repositorio:

https://github.com/RENE-2312/Mapa-Interactivo-de-Ecuador/assets/157751389/8926ef65-6ad1-499e-8971-f641cf779eca

## Importancia y Aplicaciones

Estas herramientas ayudan a ampliar el análisis de datos en el mapa, permitiendo desagregar la información y facilitar su comprensión. Los gráficos resultantes pueden integrarse en informes elaborados con R Markdown o Quarto, así como en dashboards desarrollados con Shiny. Además, los códigos son adaptables y pueden ser utilizados para crear mapas de otras zonas geográficas. Este contenido es una recopilación del curso de R que impartí en abril de 2024 a través de la AsoiMat.

## Repositorio

La estructura del repositorio es la siguiente:

### BDD

Esta carpeta alberga las bases de datos, tanto estadísticas como geoespaciales. El archivo Excel, *IESS_DAIE_capitulo_asegurados_bol_2022.xlsx*, es parte del boletín estadístico producido por la Dirección Actuarial, de Investigación y Estadística (DAIE) del Instituto Ecuatoriano de Seguridad Social (IESS) para el año 2022. Por otro lado, los conjuntos de datos geoespaciales se obtuvieron de diversas fuentes:
- *ec-all.geo.json*: Proviene del repositorio [Mapa-Ecuador](https://github.com/zpio/Mapa-Ecuador) creado por [Francisco Zambrano](https://github.com/zpio).
- *cantones-ecuador.geojson*: Fue obtenido del [repositorio](https://gist.github.com/emamut/25912e117ab46fa00a63c6e890575201) de [Faber Vergara](https://gist.github.com/emamut).
- *SHP*: Se descargó del [Geoportal INEC](https://www.ecuadorencifras.gob.ec/documentos/web-inec/Geografia_Estadistica/Micrositio_geoportal/index.html#geografia_estad) para el año 2012.

### Scripts

Esta carpeta contiene los siguientes scripts:

- `01_tooltip_table.R`: Agrega una tabla en la ventana emergente al pasar el cursor sobre el mapa.
- `02_tooltip_chart.R`: Agrega un gráfico en la ventana emergente al pasar el cursor sobre el mapa.
- `03_tooltip chart & table.R`: Agrega un gráfico y una tabla en la ventana emergente al pasar el cursor sobre el mapa.
- `04_hc_drilldown_otros.R`: Despliega otro gráfico (no mapa) al hacer clic en el gráfico principal.
- `05_hc_drilldown_cantones.R`: Despliega otro gráfico (mapa) al hacer clic en el gráfico principal.
- `06_shp_cantones_parroquias.R`: Trabaja con mapas a nivel provincial, cantonal y parroquial.

## Recursos Adicionales

Para explorar diferentes opciones de mapas, te sugiero revisar el repositorio de [Francisco Zambrano](https://github.com/zpio), [Mapa-Ecuador](https://github.com/zpio/Mapa-Ecuador), que ofrece una amplia variedad de mapas para Ecuador. Además, los códigos para los gráficos de drilldown fueron desarrollados basándome en el código descrito por [Nahuel Bargas](https://github.com/NahuelBargas) en el siguiente [enlace](https://github.com/jbkunst/highcharter/issues/507).

## Contacto

Si tienes alguna pregunta o comentario, ¡no dudes en ponerte en contacto conmigo! :)
