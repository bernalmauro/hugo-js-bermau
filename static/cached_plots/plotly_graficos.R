packages <- c(
  # IMPORTAR DATOS
  "rio",         # Facilita la lectura y escritura de varios formatos de datos.
  
  # MANEJO DATE
  "lubridate",   # Ofrece herramientas para trabajar con fechas de manera sencilla.
  "anytime",     # Convierte texto en fechas de manera intuitiva.
  
  # MANEJO DE DATOS
  "janitor",     # Realiza limpieza y preparación de datos de manera eficiente.
  "magrittr",    # Proporciona operadores de tubería adicionales.
  "rlang",       # Facilita la creación de funciones y manipulación de expresiones.
  "tidyverse",   # Conjunto de paquetes para manipulación y visualización de datos.
  "unpivotr",    # Ayuda a transformar datos no tabulares al formato tidyverse.
  "scales",      # Genera escalas para gráficos y visualizaciones.
  "tibble",      # Mejora las data frames con tibbles.
  "vctrs",       # Proporciona clases de vectores.
  "tidyselect",  # Facilita la selección de variables en tidyverse.
  
  # VISUALIZACIÓN
  "gt",          # Crea tablas bonitas y formateadas.
  "patchwork",   # Combina y organiza gráficos de manera flexible.
  "unikn",       # Ofrece paletas de colores únicas.
  "paletteer",   # Proporciona paletas de colores para gráficos.
  "ggh4x",       # Extiende ggplot2 con funcionalidades adicionales.
  "webshot",     # Captura imágenes de gráficos para su uso en informes web.
  "extrafont",   # Facilita el uso de fuentes adicionales en gráficos.
  "ggnewscale",  # Agrega escalas adicionales a gráficos ggplot.
  "scales", #Proporciona funciones para dar formato a los ejes y las leyendas en los gráficos, facilitando la manipulación de las escalas de los datos
  "viridis",
  "RColorBrewer",
  "Polychrome",
  
  # ESTADÍSTICAS
  "sjmisc",      # Proporciona funciones estadísticas y de manipulación de datos.
  "sparklyr",    # Facilita el manejo de datos grandes y la conexión con Spark.
  
  # OTROS
  "conflicted",  # Ayuda a manejar conflictos de nombres de funciones.
  "fs",          # Facilita la manipulación de archivos y directorios.
  "glue",        # Concatenación de cadenas de manera más sencilla.
  "knitr",       # Herramienta para la generación de informes y documentos.
  "remotes",     # Ayuda en la instalación y carga de paquetes.
  "fst",         # Ofrece almacenamiento rápido de datos en formato fst.
  "name",        # Organiza funciones para el nombre de columnas en data frames.
  "extraInserts",# Proporciona funciones adicionales para insertar en ggplot2.
  "testthat",    # Framework para pruebas unitarias en R.
  "grkstyle",    # Aplica estilos específicos a gráficos ggplot.
  "writexl",     # Permite exportar datos a Excel de manera sencilla.
  "purrr",       # Simplifica la programación funcional.
  "readxl",      # Importa datos de archivos Excel.
  "rvest",
  "httr",
  "RSelenium",
  
  # DYGRAPHS
  "dygraphs",    # Crea gráficos interactivos de series temporales.
  "tsbox",       # Manipula series temporales de manera eficiente.
  "zoo",         # Maneja objetos de series temporales.
  "highcharter", # Genera gráficos interactivos con Highcharts.
  "xts",         # Extiende zoo para manipulación de series temporales.
  "tibbletime",  # Ofrece manipulación de series temporales con tibbles.
  "kableExtra",  # Mejora la visualización de tablas en R Markdown.
  "formattable", # Formatea tablas de manera atractiva.
  "ifultools",   # Proporciona funciones adicionales para gráficos interactivos.
  "plotrix",
  
  # ACTUARIOS
  "survival",    # Ofrece funciones para el análisis de supervivencia.
  "survminer",   # Facilita el análisis y visualización de datos de supervivencia.
  "tidyquant",    # Proporciona herramientas para el análisis cuantitativo de datos.
  "rmarkdown",
  "diagram",
  "visNetwork",
  "grDevices",
  "tempR",
  "jcolors",
  "Rilostat",
  "plotly",
  "tools",
  "listviewer"
)

base::lapply(packages, library, character.only = TRUE)

#FUNCIONES PLOTLY GENERALES####
datos <-  function(file, sheet, range, col_names, from, to, by, each) {
  
  datos <- 
    rio::import(
      file,
      setclass = "tbl_df",
      range = range,
      sheet = sheet,
      col_names = col_names,
      .name_repair = "unique_quiet") %!>%
    remove_empty(c("rows", "cols")) %!>%
    replace(is.na(.), 0)
  
  fecha <- fun_fecha(from,to,by,each)
  data <- cbind(fecha, datos) 
  data <- as_tibble(data)
  
}
datos_sf <-  function(file, sheet, range, col_names) {
  
  datos <- 
    rio::import(
      file,
      setclass = "tbl_df",
      range = range,
      sheet = sheet,
      col_names = col_names,
      .name_repair = "unique_quiet") %!>%
    remove_empty(c("rows", "cols")) %!>%
    replace(is.na(.), 0)
  
  
}
plotly_bs <-  function(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y) {
  
  conjunto_datos_plotly <- conjunto_datos_bs %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = "scatter", mode = "lines", colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 20, r = 20, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"
      ), 
      yaxis = list( 
        zerolinewidth = 1,
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickformat = tickformat_y,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_bs, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
}
plotly_usd <-  function(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y) {
  
  conjunto_datos_plotly <- conjunto_datos_usd %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = "scatter", mode = "lines", colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 20, r = 20, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"   
      ), 
      yaxis = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickformat = tickformat_y,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_usd, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
  
}

plotly_solo_bs <-  function(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y, graph_type, barmode) {
  
  conjunto_datos_plotly <- conjunto_datos_bs %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = graph_type, colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 20, r = 20, b = 20, t = 40),
      barmode = barmode,
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"
      ), 
      yaxis = list( 
        zerolinewidth = 1,
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickformat = tickformat_y,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_bs, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
}
plotly_solo_usd <-  function(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y, graph_type, barmode) {
  
  conjunto_datos_plotly <- conjunto_datos_usd %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = graph_type, colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      barmode = barmode,
      margin = list(l = 20, r = 20, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"   
      ), 
      yaxis = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickformat = tickformat_y,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_usd, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
  
}
plotly_nomi_crec_bs <-  function(fig,titulo_plotly_bs,titulo_plotly_bs_y_1,titulo_plotly_bs_y_2,tickformat_y_1,tickformat_y_2) {
  
  
  # Mostrar el gráfico
  fig%>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 60, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"   
      ), 
      yaxis = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = titulo_plotly_bs_y_1,
                     font = list(color = '#e3e3e3', family = "Constantia", size = 13)
        ), 
        tickformat = tickformat_y_1,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      yaxis2 = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = titulo_plotly_bs_y_2,
                     font = list(color = '#e3e3e3', family = "Constantia", size = 13)
        ),
        tickformat = tickformat_y_2,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_bs, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
  
}
plotly_nomi_crec_usd <-  function(fig,titulo_plotly_usd,titulo_plotly_usd_y_1,titulo_plotly_usd_y_2,tickformat_y_1,tickformat_y_2) {
  
  
  # Mostrar el gráfico
  fig%>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'custom_image',
                                               height=  500,
                                               width= 700,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 60, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%b %Y"   
      ), 
      yaxis = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = titulo_plotly_usd_y_1,
                     font = list(color = '#e3e3e3', family = "Constantia", size = 13)
        ), 
        tickformat = tickformat_y_1,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      yaxis2 = list( 
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = titulo_plotly_usd_y_2,
                     font = list(color = '#e3e3e3', family = "Constantia", size = 13)
        ),
        tickformat = tickformat_y_2,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ), 
      title = list(text = titulo_plotly_usd, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)), 
      legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),  
      annotations = list(
        list(
          x = -0,  
          y = 1.07,  
          xref = "paper",
          yref = "paper",
          text = "bernalmauricio.com", 
          showarrow = FALSE,
          font = list(color = "#e3e3e3", family = "Arial")
        )
      )
    ) %>%
    plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))
  
  
}



generar_layout_menus <- function(plotly_lineal, graph_type, barmode) {
  plotly_lineal %!>% 
    
    plotly::layout(
      updatemenus = list(
        list(
          x = 0.20, 
          y = 1.20,
          buttons = list(
            list(
              args = list(list(type = "scatter", mode = "lines", stackgroup = FALSE), 
                          list(visible = c(TRUE, FALSE))), 
              label = 'Sin Apilar', 
              method = 'update'
            ),
            list(
              args = list(list(type = graph_type, mode = "lines", stackgroup = 'one'), 
                          list(visible = c(FALSE, TRUE), barmode = barmode)), 
              label = 'Apilado', 
              method = 'update'
            )
          )
        )
      )
    )
  
  
}
fun_excel <-function(file, range, sheet, col_names, from, to, by, each) {
  
  data <- 
    rio::import(
      file, 
      setclass = "tbl_df",
      sheet=sheet,
      range = range,
      col_names = col_names,
      .name_repair = "unique_quiet") %!>% 
    remove_empty(c("rows", "cols")) %!>%
    replace(is.na(.), 0)
  data %<>%
    pivot_longer(!...1) %<>%
    pivot_wider(names_from = "...1",
                values_from = "value") %<>%
    dplyr::select(-name)
  
  fecha <- fun_fecha(from,to,by,each)
  data <- cbind(fecha, data) 
  data <- as_tibble(data)
  
}
fun_excel_sf <-function(file, range, sheet, col_names) {
  
  data <- 
    rio::import(
      file, 
      setclass = "tbl_df",
      sheet=sheet,
      range = range,
      col_names = col_names,
      .name_repair = "unique_quiet") %!>% 
    remove_empty(c("rows", "cols")) %!>%
    replace(is.na(.), 0)
  data %<>%
    pivot_longer(!...1) %<>%
    pivot_wider(names_from = "...1",
                values_from = "value") %<>%
    dplyr::select(-name)
  
  
}
fun_fecha <-function(from, to, by, each) {
  rep(seq(anydate(from),
          anydate(to),
          by = by), each = each)
}
pal_plotly <- function(num) {
  usecol(c("#ff0000","#00ff00","#0000ff","#ffff00"),
         n = num)
}


#1.PRODUCCION ESTATAL DE MINERALES MENSUAL####

file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/hidrocarburos_mineria/2.mineria/1. Bolivia - Produccion Nacional de Minerales por Tipo de Mineral segun Año y Mes 1990 - 2024.xlsx"
range <- "B4:J485"
sheet <- "M01"
col_names <-TRUE
from <- "01Jan1990"
to <- "01Mar2024"
by <- "month"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)

data %<>%  
  dplyr::filter(!(PERIODO %in% c("1990", "1991","1992", "1993","1994","1995","1996", "1997","1998","1999","2000", 
                                 "2001", "2002","2003", "2004","2005","2006","2007", "2008","2009","2010","2011",
                                 "2012", "2013","2014", "2015","2016","2017","2018", "2019","2020","2021","2022(p)",
                                 "2023(p)", "2024(p)"))) %<>%  
  select(-PERIODO,-CADMIO)

conjunto_datos_usd <- cbind(fecha,data)

titulo_plotly_usd <- "<b>Producción Nacional de Minerales</b><br>(En toneladas métricas)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

mi_paleta_plotly <- createPalette(7, c("#ff0000", "#00ff00", "#0000ff"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_lineal_prod_minerales_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)

saveRDS(plotly_grafico_lineal_prod_minerales_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_lineal_prod_minerales_y_stacked_usd.rds", compress = TRUE)

#1.INDICE DE VOLUMEN DE PRODUCCION####

file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/industria_manufacturera_comercio/1.coyunturales/Bolivia - Indice de Volumen de Producción de la Industria Manufacturera, 1990 - 2019.xls"
range <- "A10:D190"
sheet <- "INDICE"
col_names <-TRUE
from <- "01Dec1991"
to <- "01Dec2019"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)

data%<>%    
  dplyr::filter(!(PERÍODO %in% c("I Trimestre", "II Trimestre", "III Trimestre", "IV Trimestre"))) %<>% 
  select(-PERÍODO,-`ÍNDICE GENERAL`,-`VARIACIÒN PORCENTUAL A PERÍODO ANTERIOR`)%<>%  
  mutate(across(where(is.numeric), ~  . /100 )) 

conjunto_datos_usd <- cbind(fecha,data)

titulo_plotly_usd <- "<b>Indice de Volumen de Producción Industrial</b><br>(Variación interanual)"
tickformat_y <- ".0%"
mi_paleta_plotly <- pal_plotly(1)

plotly_grafico_lineal_var_indice_indus_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)

saveRDS(plotly_grafico_lineal_var_indice_indus_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_lineal_var_indice_indus_y_stacked_usd.rds", compress = TRUE)


#1.INDICE DE VOLUMEN DE VENTAS####

file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/industria_manufacturera_comercio/1.coyunturales/Bolivia - Indice de Volumen de Ventas de la Industria Manufacturera, 1990 - 2019.xls"
range <- "A10:D190"
sheet <- "INDICE"
col_names <-TRUE
from <- "01Dec1991"
to <- "01Dec2019"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)

data%<>%    
  dplyr::filter(!(PERÍODO %in% c("I Trimestre", "II Trimestre", "III Trimestre", "IV Trimestre"))) %<>% 
  select(-PERÍODO,-`ÍNDICE GENERAL`,-`VARIACIÒN PORCENTUAL A PERÍODO ANTERIOR`)%<>%  
  mutate(across(where(is.numeric), ~  . /100 )) 

conjunto_datos_usd <- cbind(fecha,data)

titulo_plotly_usd <- "<b>Indice de Volumen de Ventas de la Industria</b><br>(Variación interanual)"
tickformat_y <- ".0%"
mi_paleta_plotly <- pal_plotly(1)

plotly_grafico_lineal_var_indice_indus_ventas_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)

saveRDS(plotly_grafico_lineal_var_indice_indus_ventas_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_lineal_var_indice_indus_ventas_y_stacked_usd.rds", compress = TRUE)

#1.Nal-2024_05_1_Bolivia_Indicegeneral_Var_Mensual_12_Meses_Acumulado####

file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/ipc/ipc/2016/1.nacional/Nal-2024_05_1_Bolivia_Indicegeneral_Var_Mensual_12_Meses_Acumulado.xlsx"
col_names <-TRUE
from <- "01Jan2018"
to <- "01May2024"
by <- "month"
each <- 1
fecha <- fun_fecha(from,to,by,each)

range <- "A5:H18"
sheet <- "CUADRO Nº 1.3 VAR ACUMULADA"

data <- datos_sf(file,sheet,range,col_names)

data_1 <- data%<>%    
  pivot_longer(cols = -MES, names_to = "Año", values_to = "Acumulada en el año") %<>%    
  mutate(Año = as.numeric(Año)) %<>%    
  arrange(Año) %<>%    
  select(-MES, -Año) %<>%    
  dplyr::filter(`Acumulada en el año` != 0)

range <- "A5:H18"
sheet <- "CUADRO Nº 1.4 VAR 12 MESES"

data <- datos_sf(file,sheet,range,col_names)

data_2 <- data%<>%    
  pivot_longer(cols = -MES, names_to = "Año", values_to = "Variación Interanual") %<>%    
  mutate(Año = as.numeric(Año)) %<>%    
  arrange(Año) %<>%    
  select(-MES, -Año) %<>%    
  dplyr::filter(`Variación Interanual` != 0)

conjunto_datos_usd <- cbind(fecha, data_1,data_2)
conjunto_datos_usd
titulo_plotly_usd <- "<b>Indice de Precios al Consumidor</b><br>(Porcentaje)"

mi_paleta_plotly <- pal_plotly(2)


# Filtrar los datos para cada tipo de gráfico
datos_bar <- conjunto_datos_plotly %>%
  dplyr::filter(variables == "Acumulada en el año")

datos_linea <- conjunto_datos_plotly %>%
  dplyr::filter(variables == "Variación Interanual")

# Crear gráfico combinado
combinacion_graficos <- plot_ly()

# Agregar línea con relleno
combinacion_graficos <- combinacion_graficos %>%
  add_lines(data = datos_linea, x = ~fecha, y = ~valores, name = "Variación Interanual", fill = "tozeroy", fillcolor='rgba(0,100,80,0.2)')

# Agregar barras
combinacion_graficos <- combinacion_graficos %>%
  add_bars(data = datos_bar, x = ~fecha, y = ~valores,name = "Acumulada en el año")

tickformat_y <- ",.2f"
# Estilo y diseño del gráfico
plotly_grafico_lineal_var_indice_consumidor_y_stacked_usd <- combinacion_graficos %>%
  plotly::layout(
    barmode = "overlay",
    margin = list(l = 20, r = 20, b = 20, t = 40),
    paper_bgcolor = '#1d1d1d',
    plot_bgcolor = '#1d1d1d',
    xaxis = list(
      zerolinewidth = F,
      zerolinecolor = 'transparent',
      showgrid = F,
      title = list(text = " "),
      tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
      showline = TRUE,
      linecolor = F,
      ticks = 'outside',
      hoverformat = "%b %Y"
    ),
    yaxis = list(
      zerolinewidth = 1,
      zerolinecolor = 'transparent',
      showgrid = F,
      title = list(text = " "),
      tickformat = tickformat_y,
      tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
      showline = T,
      linecolor = F,
      ticks = 'outside'
    ),
    title = list(text = titulo_plotly_usd, x = 0.9, font = list(color = '#e3e3e3', family = "Constantia", size=15)),
    legend = list(font = list(color = '#e3e3e3',size = 12, family = "Constantia"), orientation="h", traceorder= "normal"),
    annotations = list(
      list(
        x = -0,
        y = 1.07,
        xref = "paper",
        yref = "paper",
        text = "bernalmauricio.com",
        showarrow = FALSE,
        font = list(color = "#e3e3e3", family = "Arial")
      )
    )
  ) %>%
  plotly::style(hoverlabel = list(namelength = -1, font = list(family = "Constantia")))

saveRDS(plotly_grafico_lineal_var_indice_consumidor_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_lineal_var_indice_consumidor_y_stacked_usd.rds", compress = TRUE)


#2.PRECIPITACION ANUAL####
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/medio_ambiente/1.clima_atmosfera/1.mensuales/BOLIVIA - PRECIPITACIÓN ACUMULADA POR CIUDADES, SEGÚN AÑO Y MES, 1990 - 2024.xlsx"
range <- "B9:L473"
sheet <- "BOLIVIA"
col_names <-TRUE
from <- "01Dec1990"
to <- "01Dec2023"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

data %<>%     
  dplyr::filter(!(PERIODO %in% meses))%<>% 
  select(-PERIODO) 

conjunto_datos_usd <- cbind(fecha,data)

titulo_plotly_usd <- "<b>Precipitación Anual Acumulada Por Ciudad</b><br>(En milimetros)"
tickformat_y <- ",.2f"
mi_paleta_plotly <- createPalette(10, c("#ff0000", "#00ff00", "#0000ff"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_lineal_precip_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)

saveRDS(plotly_grafico_lineal_precip_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_lineal_precip_y_stacked_usd.rds", compress = TRUE)
