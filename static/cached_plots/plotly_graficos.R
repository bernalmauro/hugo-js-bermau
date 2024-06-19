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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 40, r = 60, b = 20, t = 40),
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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 40, r = 60, b = 20, t = 40),
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

plotly_combi_bs <-  function(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y) {
  
  conjunto_datos_plotly <- conjunto_datos_bs %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = "scatter", mode = "lines", colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 40, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        tickformat = "%Y",  # Formato de tick para mostrar solo el año
        tickmode = 'linear',
        tick0 = min(conjunto_datos_usd$fecha),
        dtick = 'M36',  # Saltos mensuales para asegurar que se muestren todos los años
        type = 'date',
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%Y"  
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
plotly_combi_usd <-  function(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y) {
  
  conjunto_datos_plotly <- conjunto_datos_usd %>%
    pivot_longer(!fecha, names_to = "variables", values_to = "valores")
  
  plot_ly(data = conjunto_datos_plotly, x = ~fecha, y = ~valores,
          color = ~variables, type = "scatter", mode = "lines", colors = mi_paleta_plotly) %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 40, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        tickformat = "%Y",  # Formato de tick para mostrar solo el año
        tickmode = 'linear',
        tick0 = min(conjunto_datos_usd$fecha),
        dtick = 'M36',  # Saltos mensuales para asegurar que se muestren todos los años
        type = 'date',
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%Y"    
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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      margin = list(l = 40, r = 60, b = 20, t = 40),
      barmode = barmode,
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        tickformat = "%Y",  # Formato de tick para mostrar solo el año
        tickmode = 'linear',
        tick0 = min(conjunto_datos_usd$fecha),
        dtick = 'M36',  # Saltos mensuales para asegurar que se muestren todos los años
        type = 'date',
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%Y"
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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      barmode = barmode,
      margin = list(l = 40, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list( 
        tickformat = "%Y",  # Formato de tick para mostrar solo el año
        tickmode = 'linear',
        tick0 = min(conjunto_datos_usd$fecha),
        dtick = 'M36',  # Saltos mensuales para asegurar que se muestren todos los años
        type = 'date',
        zerolinewidth = F, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " "), 
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline =TRUE,  
        linecolor = F,
        ticks = 'outside',
        hoverformat = "%Y" 
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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
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
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
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

plotly_nomi_crec_usd_1 <- function(conjunto_datos_usd, name_1,name_2,fecha,y_1,y_2,type_2,type_1,titulo_plotly_usd, mi_paleta_plotly,fecha_min,fecha_max,tickformat_y_1,tickformat_y_2) {
  
  # Crear el gráfico con dos ejes
  fig <- plot_ly(x = ~fecha)
  
  # Agregar la serie de "SALARIO MÍNIMO NACIONAL"
  fig <- fig %>% add_trace(
    y = y_2,
    type = type_2,
    mode = 'lines',
    yaxis = 'y2',
    name = name_2,
    fill = "tozeroy",
    data = conjunto_datos_usd
  )
  
  # Agregar la serie de "VARIACIÓN PORCENTUAL"
  fig <- fig %>% add_trace(
    y = y_1,
    type = type_1,
    name = name_1,
    yaxis = 'y1',
    data = conjunto_datos_usd
  )
  
  # Configurar los ejes
  fig <- fig %>% 
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
    plotly::layout(
      barmode= "overlay",
      margin = list(l = 60, r = 60, b = 20, t = 40),
      paper_bgcolor = '#1d1d1d',
      plot_bgcolor = '#1d1d1d',
      xaxis = list(
        tickformat = "%Y",  # Formato de tick para mostrar solo el año
        tickmode = 'linear',
        tick0 = min(conjunto_datos_usd$fecha),
        dtick = 'M12',  # Saltos mensuales para asegurar que se muestren todos los años
        type = 'date',
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
        title = ' ',
        side = 'left',
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " ",
                     font = list(color = '#e3e3e3', family = "Constantia", size = 13)), 
        tickformat = tickformat_y_1,
        tickfont = list(color = '#e3e3e3', family = "Constantia", size = 13),
        showline = T,  
        linecolor = F, 
        ticks = 'outside'  
      ),
      yaxis2 = list(
        title = ' ',
        overlaying = 'y',
        side = 'right',
        zerolinewidth = 1, 
        zerolinecolor = 'transparent',
        showgrid = F,
        title = list(text = " ",
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
    plotly::config(scrollZoom = FALSE, responsive = TRUE, displaylogo = FALSE,
                   toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                               filename= 'bernalmauricio',
                                               height=  550,
                                               width= 750,
                                               scale= 1 )) %>% 
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

#Banco Central de Bolivia (BCB)####
#Sector Monetario##
#Balance BCB#

#BALANCE ACTIVO BCB
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/03a.activos_bcb.xlsx"
range <- "B6:Q305"
sheet <- "MENSUAL"
col_names <- FALSE
from <- "01Jan1998"
to <- "01Dec2022"
by <- "month"
each <- 1

data <- datos(file,sheet,range,col_names, from, to, by, each)

colnames(data) <- c(
  "fecha",
  "Reservas Internacionales Brutas",
  "Oro",
  "Divisas",
  "Otros Activos",
  "Aportes a Organismos Internacionales",
  "Otros Activos Externos de Mediano y Largo Plazo",
  "Crédito al Sector Público",
  "Al Gobierno Central",
  "A la Seguridad Social",
  "A Locales y Regionales",
  "A Empresas Públicas",
  "Crédito al Sector Financiero",
  "A Bancos Comerciales y en Liquidación",
  "A Bancos Especializados y Otras Entidades Financieras",
  "Otras Cuentas de Activo",
  "Total Activo"
)

conjunto_datos_bs <- data %>%
  select(
    `fecha`,
    `Reservas Internacionales Brutas`,
    `Aportes a Organismos Internacionales`,
    `Otros Activos Externos de Mediano y Largo Plazo`,
    `Crédito al Sector Público`,
    `Crédito al Sector Financiero`,
    `Otras Cuentas de Activo`
  )%>%
  mutate(across(where(is.numeric), ~  . / 1000))

mi_paleta_plotly <- pal_plotly(6)
titulo_plotly_bs <- "<b>Balance del Activo del BCB - Mensual</b><br>(Millones Bs.)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

plotly_grafico_activo_lineal_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_activo_lineal_bs
plotly_grafico_activo_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_activo_lineal_y_stacked_bs

saveRDS(plotly_grafico_activo_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_activo_lineal_y_stacked_bs.rds", compress = TRUE)

#BALANCE PASIVO BCB
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/03p.pasivos_bcb.xlsx"

range <- "B29:AE317"
sheet <- "Mensual"
col_names <- FALSE
from <- "31Dec1998"
to <- "31Dec2022"
by <- "month"
each <- 1

data <- datos(file,sheet,range,col_names, from, to, by, each)
colnames(data) <- c(
  "fecha",
  "Emisión Monetaria",
  
  "Depósitos Bancarios",
  "Bancos Comerciales",
  "Bancos Especializados y Otras Entidades Financieras",
  
  "Obligaciones Externas de Corto Plazo",
  "Giros sobre el FMI",
  "Con Bancos y Otros Organismos",
  
  "Depósito de Organismos Internacionales",
  
  "Obligaciones Externas a Mediano y Largo Plazo",
  
  "Otras Cuentas Pasivo",
  
  "Certificado de Devolución de Depósito",
  "En Moneda Extranjera",
  "En Moneda Nacional con Mantenimiento de Valor",
  
  "Patrimonio Neto",
  
  "Depósitos del Sector Público",
  
  "Del Gobierno Central (GC)",
  "Del GC en Moneda Nacional",
  "Del GC en Unidad de Fomento de Valor",
  "Del GC en Moneda Extranjera",
  "Del GC en Moneda Nacional con Mantenimiento de Valor",
  
  "De la Seguridad Social (SS)",
  "De la SS en Moneda Nacional",
  "De la SS en Moneda Nacional con Mantenimiento de Valor",
  
  "De Gobiernos Locales y Regionales (GLyR)",
  "De GLyR en Moneda Nacional",
  "De GLyR en Moneda Extranjera",
  
  "De Empresas Públicas (EP)",
  "De EP en Moneda Nacional",
  "De EP en Moneda Extranjera",
  
  "Total Pasivo"
)

conjunto_datos_bs <- data %>%
  select(`fecha`,
         `Emisión Monetaria`,
         `Depósitos Bancarios`,
         
         
         `Obligaciones Externas a Mediano y Largo Plazo`,
         `Otras Cuentas Pasivo`,
         
         `Patrimonio Neto`,
         `Depósitos del Sector Público`) %>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_bs <- "<b>Balance del Pasivo del BCB - Mensual</b><br>(Millones Bs.)"

plotly_grafico_pasivo_lineal_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_pasivo_lineal_bs
plotly_grafico_pasivo_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_pasivo_lineal_y_stacked_bs

saveRDS(plotly_grafico_pasivo_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_pasivo_lineal_y_stacked_bs.rds", compress = TRUE)

#Sector Externo##
#Balanza Cambiaria#

#INGRESO DIVISAS
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/2.externo/33.balanza_cambiaria.xlsx"
range <- "A9:O84"
sheet <- "hoja_anual"
col_names <- FALSE
from <- "01Dec2010"
to <- "01Dec2023"
by <- "year"
each <- 1

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `I.A. Por Exportaciones`,
         `I.B. Renta`,
         `I.D. Bancos - Cheques`,
         `I.E. Desembolso Deuda Externa`,
         `I.J. Otros`)

titulo_plotly_usd <- "<b>Balanza Cambiaria (Ingreso Divisas) - Anual</b><br>(En millones $us)"
mi_paleta_plotly <- pal_plotly(5)
graph_type <-  "bar"
barmode <-  "stack"

plotly_grafico_ingreso_cambiaria_lineal_usd <-  plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_lineal <- plotly_grafico_ingreso_cambiaria_lineal_usd
plotly_grafico_ingreso_cambiaria_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ingreso_cambiaria_lineal_y_stacked_usd

saveRDS(plotly_grafico_ingreso_cambiaria_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_ingreso_cambiaria_lineal_y_stacked_usd.rds", compress = TRUE)

#EGRESO DIVISAS
conjunto_datos_bs <- data %>%
  select(fecha, 
         `II.A. Por Importación`,
         `II.B. Servicio Deuda Externa`,
         `II.C Sector Privado-Bancos`,
         `II.G Otros`,
         `II.H YPFB Costos Recuperables y Retribuciones a Empresas`)%>%
  mutate(across(where(is.numeric), ~  . *6.86 ))

conjunto_datos_usd <- data %>%
  select(fecha, 
         `II.A. Por Importación`,
         `II.B. Servicio Deuda Externa`,
         `II.C Sector Privado-Bancos`,
         `II.G Otros`,
         `II.H YPFB Costos Recuperables y Retribuciones a Empresas`)

titulo_plotly_usd <- "<b>Balanza Cambiaria (Egreso Divisas) - Anual</b><br>(En millones $us)"

plotly_grafico_egreso_cambiaria_lineal_usd <-  plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_lineal <- plotly_grafico_egreso_cambiaria_lineal_usd

plotly_grafico_egreso_cambiaria_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_egreso_cambiaria_lineal_y_stacked_usd

saveRDS(plotly_grafico_egreso_cambiaria_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_egreso_cambiaria_lineal_y_stacked_usd.rds", compress = TRUE)

#Base Monetaria#

#ORIGEN BASE MONETARIA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/01.base_monetaria.xlsx"
range <- "D24:S313"
col_names <- FALSE
from <- "01Dec1998"
to <- "01Jan2023"
by <- "month"
each <- 1

data <- 
  rio::import(
    file,
    setclass = "tbl_df",
    range = range,
    col_names = col_names,
    .name_repair = "unique_quiet") %!>%
  remove_empty(c("rows", "cols")) %!>%
  replace(is.na(.), 0)

colnames(data) <- c(
  
  "Reservas Internacionales Netas",
  "Reservas Internacionales Brutas",
  "Obligaciones Externas a Corto Plazo",
  "Crédito Neto al Sector Público",
  "Crédito a Bancos",
  "BCL",
  "BE Y OEF",
  "Títulos Regulación Monetaria",
  "SRD",
  "Otras Cuentas Netas",
  "Base Monetaria",
  "Billetes y Monedas en poder del Público",
  "Reservas Bancarias en Moneda Nacional",
  "Reservas Bancarias en UFV",
  "Reservas Bancarias en Moneda Extranjera",
  "Total Reservas Bancarias"
)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Reservas Internacionales Netas`,
         `Crédito Neto al Sector Público`,
         `Crédito a Bancos`,
         `Títulos Regulación Monetaria`,
         `Otras Cuentas Netas`)%>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_bs <- "<b>Origen de la Base Monetaria - Mensual</b><br>(En millones Bs.)"
graph_type <-  "scatter"
barmode <-  FALSE

plotly_grafico_base_origen_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_base_origen_lineal_bs

plotly_grafico_base_origen_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_base_origen_lineal_y_stacked_bs

saveRDS(plotly_grafico_base_origen_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_base_origen_lineal_y_stacked_bs.rds", compress = TRUE)

#DESTINO BASE MONETARIA
conjunto_datos_bs <- data %>%
  select(fecha, 
         `Billetes y Monedas en poder del Público`,
         `Reservas Bancarias en Moneda Nacional`,
         `Reservas Bancarias en UFV`,
         `Reservas Bancarias en Moneda Extranjera`)%>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_bs <- "<b>Destino de la Base Monetaria - Mensual</b><br>(En millones Bs.)"

plotly_grafico_base_destino_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_base_destino_lineal_bs
plotly_grafico_base_destino_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_base_destino_lineal_y_stacked_bs

saveRDS(plotly_grafico_base_destino_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_base_destino_lineal_y_stacked_bs.rds", compress = TRUE)

#CREDITO SECTOR PRIVADO
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/12.credito_sector_privado.xlsx"
sheet <- "Hoja1"
range <- "B61:F279"
col_names <- FALSE
from <- "01Jan2006"
to <- "01Mar2024"
by <- "month"
each <- 1

data <- 
  rio::import(
    file,
    setclass = "tbl_df",
    range = range,
    sheet = sheet,
    col_names = col_names,
    .name_repair = "unique_quiet") %!>%
  remove_empty(c("rows", "cols")) %!>%
  replace(is.na(.), 0)

colnames(data) <- c(
  
  "La Paz",
  "Santa Cruz",
  "Cochabamba",
  "Resto del País",
  "Total"
)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `La Paz`,
         `Santa Cruz`,
         `Cochabamba`,
         `Resto del País`)%>%
  mutate(across(where(is.numeric), ~  . / 1000))

conjunto_datos_usd <- data %>%
  select(fecha, 
         `La Paz`,
         `Santa Cruz`,
         `Cochabamba`,
         `Resto del País`)%>%
  mutate(across(where(is.numeric), ~  . / 1000/6.86))

titulo_plotly_bs <- "<b>Crédito al Sector Privado - Mensual</b><br>(En millones Bs.)"
titulo_plotly_usd <- "<b>Crédito al Sector Privado - Mensual</b><br>(En millones $us)"

mi_paleta_plotly <- pal_plotly(4)

plotly_grafico_credito_sector_privado_depto_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_credito_sector_privado_depto_lineal_bs
plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_bs

saveRDS(plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_bs.rds", compress = TRUE)

plotly_grafico_credito_sector_privado_depto_lineal_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_lineal <- plotly_grafico_credito_sector_privado_depto_lineal_usd
plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_usd

saveRDS(plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_credito_sector_privado_depto_lineal_y_stacked_usd.rds", compress = TRUE)

#DEUDA SECTOR PUBLICO BCB
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/18A.titulos_tgn_bcb.xlsx"
sheet <- "Hoja2"
range <- "B325:V376"
col_names <- FALSE
#FORMATO FECHA
from <- "01Dec2019"
to <- "01Mar2024"
by <- "month"
each <- 1

data <- 
  rio::import(
    file,
    setclass = "tbl_df",
    range = range,
    sheet = sheet,
    col_names = col_names,
    .name_repair = "unique_quiet") %!>%
  remove_empty(c("rows", "cols")) %!>%
  replace(is.na(.), 0)

colnames(data) <- c(
  
  "Letras MN",
  "Letras UFV",
  "Letras ME",
  "Letras",
  
  "Bonos MN",
  "Bonos UFV",
  "Bonos ME",
  "Bonos",
  
  "BCB Directo MN",
  "BCB Directo UFV",
  "BCB Directo ME",
  "BCB Directo",
  
  "CDDS",
  "Bonos BCB (Fianza)",
  "CDS ME y MVDOL",
  
  "RAL MN",
  "RAL UFV",
  "Fondo Ral",
  
  "CDS AFP (MN)",
  "Reservas Complementarias",
  "Deuda Interna del BCB"
)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Letras`,
         `Bonos`,
         `BCB Directo`,
         `Fondo Ral`,
         `CDS AFP (MN)`)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Letras`,
         `Bonos`,
         `BCB Directo`,
         `Fondo Ral`,
         `CDS AFP (MN)`) %>% 
  mutate(across(where(is.numeric), ~  . /6.86))

titulo_plotly_bs <- "<b>Deuda Interna del BCB con Sector Privado - Mensual</b><br>(En millones Bs.)"
titulo_plotly_usd <- "<b>Deuda Interna del BCB con Sector Privado - Mensual</b><br>(En millones $us)"

mi_paleta_plotly <- pal_plotly(5)

plotly_grafico_deuda_bcb_privados_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_deuda_bcb_privados_lineal_bs

plotly_grafico_deuda_bcb_privados_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_deuda_bcb_privados_lineal_y_stacked_bs

saveRDS(plotly_grafico_deuda_bcb_privados_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_deuda_bcb_privados_lineal_y_stacked_bs.rds", compress = TRUE)

plotly_grafico_deuda_bcb_privados_lineal_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_lineal <- plotly_grafico_deuda_bcb_privados_lineal_usd

plotly_grafico_deuda_bcb_privados_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_deuda_bcb_privados_lineal_y_stacked_usd

saveRDS(plotly_grafico_deuda_bcb_privados_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_deuda_bcb_privados_lineal_y_stacked_usd.rds", compress = TRUE)

#CREDITO AL SECTOR PUBLICO
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/1.monetario/06.financiamiento_bcb_sector_publico.xlsx"
range <- "B6:P314"
sheet <- "Hoja1"
col_names <- FALSE
from <- "01Jan1998"
to <- "01Sep2023"
by <- "month"
each <- 1

data <- 
  rio::import(
    file,
    setclass = "tbl_df",
    range = range,
    sheet = sheet,
    col_names = col_names,
    .name_repair = "unique_quiet") %!>%
  remove_empty(c("rows", "cols")) %!>%
  replace(is.na(.), 0)

colnames(data) <- c(
  
  "Crédito Bruto a Gobierno Central",
  "Depósitos de Gobierno Central",
  "Crédito Neto a Gobierno Central",
  
  "Crédito Bruto a la Seguridad Social",
  "Depósitos de Seguridad Social",
  "Crédito Neto a Seguridad Social",
  
  "Crédito Bruto a Gobiernos Locales y Regionales",
  "Depósitos de Gobiernos Locales y Regionales",
  "Crédito Neto a Gobiernos Locales y Regionales",
  
  "Crédito Bruto a Empresas Públicas",
  "Depósitos de Empresas Públicas",
  "Crédito Neto a Empresas Públicas",
  
  "Crédito Bruto Total",
  "Depósitos Totales",
  "Crédito Neto Total"
)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Crédito Bruto a Gobierno Central`,
         `Crédito Bruto a Empresas Públicas`)%>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_bs <- "<b>Financiamiento del BCB al Sector Público - Mensual</b><br>(En millones Bs.)"


plotly_grafico_credito_sector_publico_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_credito_sector_publico_lineal_bs

plotly_grafico_credito_sector_publico_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_credito_sector_publico_lineal_y_stacked_bs

saveRDS(plotly_grafico_credito_sector_publico_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_credito_sector_publico_lineal_y_stacked_bs.rds", compress = TRUE)

#DEPOSITOS DEL SECTOR PUBLICO EN EL BCB
conjunto_datos_bs <- data %>%
  select(fecha, 
         `Depósitos de Empresas Públicas`,
         `Depósitos de Gobierno Central`,
         `Depósitos de Gobiernos Locales y Regionales`,
         `Depósitos de Seguridad Social`)%>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_bs <- "<b>Depósitos del Sector Público en el BCB - Mensual</b><br>(En millones Bs.)"
mi_paleta_plotly <- pal_plotly(4)

plotly_grafico_depositos_sector_publico_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_depositos_sector_publico_lineal_bs

plotly_grafico_depositos_sector_publico_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_depositos_sector_publico_lineal_y_stacked_bs

saveRDS(plotly_grafico_depositos_sector_publico_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_depositos_sector_publico_lineal_y_stacked_bs.rds", compress = TRUE)


#RESERVAS INTERNACIONALES
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/2.externo/29.reservas_internacionales_bcb.xlsx"

#IMPORTAR DATOS DE EXCEL
range <- "D120:G316"
sheet <- "Hoja1"
col_names <- FALSE
#FORMATO FECHA
from <- "01Dec2007"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- 
  rio::import(
    file,
    setclass = "tbl_df",
    sheet = sheet,
    range = range,
    col_names = col_names,
    .name_repair = "unique_quiet") %!>%
  remove_empty(c("rows", "cols")) %!>%
  replace(is.na(.), 0)

colnames(data) <- c(
  "Oro",
  "Divisas",
  "DEG",
  "Tramo de Reservas del FMI"
)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
conjunto_datos <- as_tibble(data)

conjunto_datos_bs <- conjunto_datos %>%
  mutate(across(where(is.numeric), ~  . * 6.86))

conjunto_datos_usd <- conjunto_datos

titulo_plotly_bs <- "<b>Reservas Internacionales del BCB - Mensual</b><br>(En millones Bs.)"
titulo_plotly_usd <- "<b>Reservas Internacionales del BCB - Mensual</b><br>(En millones $us)"
mi_paleta_plotly <- pal_plotly(4)
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE
plotly_grafico_lineal_reservas_bs  <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_lineal_reservas_bs

plotly_grafico_reservas_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_reservas_lineal_y_stacked_bs

saveRDS(plotly_grafico_reservas_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_reservas_lineal_y_stacked_bs.rds", compress = TRUE)

plotly_grafico_lineal_reservas_usd  <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_lineal <- plotly_grafico_lineal_reservas_usd

plotly_grafico_reservas_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_reservas_lineal_y_stacked_usd

saveRDS(plotly_grafico_reservas_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_reservas_lineal_y_stacked_usd.rds", compress = TRUE)

#REMESAS ORIGEN
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/2.externo/24.remesas_recibidas_del_exterior.xlsx"
sheet <- "Hoja1"
range <- "C6:N24"
col_names <- TRUE
#FORMATO FECHA
from <- "01Dec2006"
to <- "01Dec2023"
by <- "year"
each <- 1

data <- 
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
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  mutate(across(where(is.numeric), ~  . *6.86))

conjunto_datos_usd <- data

titulo_plotly_bs <- "<b>Remesas Recibidas - Anual</b><br>(En millones Bs.)"
titulo_plotly_usd <- "<b>Remesas Recibidas - Anual</b><br>(En millones $us)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

mi_paleta_plotly <- createPalette(12, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_remesas_pais_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_remesas_pais_lineal_bs

plotly_grafico_remesas_pais_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_remesas_pais_lineal_y_stacked_bs

saveRDS(plotly_grafico_remesas_pais_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_remesas_pais_lineal_y_stacked_bs.rds", compress = TRUE)

plotly_grafico_remesas_pais_lineal_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_lineal <- plotly_grafico_remesas_pais_lineal_usd

plotly_grafico_remesas_pais_lineal_y_stacked_usd <-  generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_remesas_pais_lineal_y_stacked_usd

saveRDS(plotly_grafico_remesas_pais_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_remesas_pais_lineal_y_stacked_usd.rds", compress = TRUE)


#TIPO DE CAMBIO REAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/3.tipo_cambio_y_precios/36.indice_cambio_real.xlsx"
sheet <- "Hoja1"
range <- "D158:AF406"
col_names <- TRUE
from <- "01Aug2003"
to <- "01Mar2024"
by <- "month"
each <- 1

data <- 
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
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Argentina`,
         `Brasil`,
         `Chile`,
         `China`,
         `Estados Unidos`,
         `Paraguay`,
         `Perú`,
         `Zona del Euro`,
         `Multilateral`)


titulo_plotly_bs <- "<b>Índices de Tipo de Cambio Real - Mensual</b><br>(Agosto de 2003 = 100)"

mi_paleta_plotly <- pal_plotly(9)

plotly_grafico_tipo_real_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_grafico_tipo_real_lineal_bs
saveRDS(plotly_grafico_tipo_real_lineal_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_tipo_real_lineal_bs.rds", compress = TRUE)

#PONDERACIONES DE COMERCIO INTERNACIONAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/3.tipo_cambio_y_precios/37.ponderadores_comercio_internacional.xlsx"
sheet <- "Hoja1"
range <- "D7:AD40"
col_names <- TRUE
#FORMATO FECHA
from <- "01Dec1991"
to <- "01Dec2023"
by <- "year"
each <- 1

data <- 
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
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Argentina`,
         `Brasil`,
         
         `China`,
         
         `Emiratos Árabes Unidos`,
         `Estados Unidos`,
         `India`,
         `Japón`,
         
         `Perú`,
         
         `Zona del Euro`)


titulo_plotly_bs <- "<b>Ponderadores Comercio Internacional - Anual</b><br>(En porcentaje)"

mi_paleta_plotly <- brewer.pal(9, "Set1")

plotly_grafico_ponderadores_comercio_lineal_bs <-  plotly_combi_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_grafico_ponderadores_comercio_lineal_bs

saveRDS(plotly_grafico_ponderadores_comercio_lineal_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_ponderadores_comercio_lineal_bs.rds", compress = TRUE)

#TASAS PASIVAS AHORRO
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/4.monetario_crediticio/44.tasas_pasivas_cah.xlsx"
sheet <- "Hoja1"
range <- "B4:I115"
col_names <- TRUE
#FORMATO FECHA
from <- "01Jan2015"
to <- "01Mar2024"
by <- "month"
each <- 1

data <- 
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
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Bancos Múltiples`,
         `Cooperativas`,
         `Ent. Esp. En Microcrédito`,
         `Entidades Financieras de Vivienda`
  )%>%
  mutate(across(where(is.numeric), ~  . / 100))
conjunto_datos_usd <- data %>%
  select(fecha, 
         `Bancos Múltiples.`,
         `Cooperativas.`,
         `Ent. Esp. En Microcrédito.`,
         `Entidades Financieras de Vivienda.`
  )%>%
  mutate(across(where(is.numeric), ~  . / 100))

titulo_plotly_bs <- "<b>Tasas de Interés Pasivas - Mensual</b><br>(Caja de Ahorros Bs.)"
titulo_plotly_usd <- "<b>Tasas de Interés Pasivas - Mensual</b><br>(Caja de Ahorros $us)"

mi_paleta_plotly <- pal_plotly(4)
tickformat_y <- ".1%"
graph_type <-  "scatter"

plotly_grafico_tasa_pasiva_cah_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_grafico_tasa_pasiva_cah_lineal_bs
saveRDS(plotly_grafico_tasa_pasiva_cah_lineal_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_tasa_pasiva_cah_lineal_bs.rds", compress = TRUE)

plotly_grafico_tasa_pasiva_cah_lineal_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_tasa_pasiva_cah_lineal_usd
saveRDS(plotly_grafico_tasa_pasiva_cah_lineal_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_tasa_pasiva_cah_lineal_usd.rds", compress = TRUE)


#TASAS PASIVAS DPF
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/banco_central/4.mensual/4.monetario_crediticio/43.tasas_pasivas_dpf_mn_me.xlsx"
range <- "B4:E115"
col_names <- TRUE

sheet <- "multiples"
data_1 <- datos_sf(file,sheet,range,col_names)

sheet <- "cooperativas"
data_2 <- datos_sf(file,sheet,range,col_names)

sheet <- "microcreditos"
data_3 <- datos_sf(file,sheet,range,col_names)

sheet <- "efv"
data_4 <- datos_sf(file,sheet,range,col_names)

from <- "01Jan2015"
to <- "01Mar2024"
by <- "month"
each <- 1

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data_1,data_2,data_3,data_4) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(`fecha`,
         `Bancos Múltiples`,
         `Cooperativas`,
         `Ent. Esp. En Microcrédito Nomin`,
         `Entidades Financieras de Vivienda`) %>%
  mutate(across(where(is.numeric), ~  . / 100))

conjunto_datos_usd <- data %>%
  select(`fecha`,
         `Bancos Múltiples.`,
         `Cooperativas.`,
         `Ent. Esp. En Microcrédito Nomin.`,
         `Entidades Financieras de Vivienda.`) %>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <- pal_plotly(5)

titulo_plotly_bs <- "<b>Tasas Pasivas DPF - Mensual (Bs.)</b><br>1 a 30 días"
titulo_plotly_usd <- "<b>Tasas Pasivas DPF - Mensual ($us.)</b><br>1 a 30 días"
tickformat_y <- ".1%"

plotly_grafico_dpf_30_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_grafico_dpf_30_bs
saveRDS(plotly_grafico_dpf_30_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_dpf_30_bs.rds", compress = TRUE)

plotly_grafico_dpf_30_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_dpf_30_usd
saveRDS(plotly_grafico_dpf_30_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_dpf_30_usd.rds", compress = TRUE)

#de 181-360 dias####

range <- "R4:U115"
col_names <- TRUE

sheet <- "multiples"
data_1 <- datos_sf(file,sheet,range,col_names)

sheet <- "cooperativas"
data_2 <- datos_sf(file,sheet,range,col_names)

sheet <- "microcreditos"
data_3 <- datos_sf(file,sheet,range,col_names)

sheet <- "efv"
data_4 <- datos_sf(file,sheet,range,col_names)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data_1,data_2,data_3,data_4) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(`fecha`,
         `Bancos Múltiples`,
         `Cooperativas`,
         `Ent. Esp. En Microcrédito Nomin`,
         `Entidades Financieras de Vivienda`) %>%
  mutate(across(where(is.numeric), ~  . / 100))

conjunto_datos_usd <- data %>%
  select(`fecha`,
         `Bancos Múltiples.`,
         `Cooperativas.`,
         `Ent. Esp. En Microcrédito Nomin.`,
         `Entidades Financieras de Vivienda.`) %>%
  mutate(across(where(is.numeric), ~  . / 100))

titulo_plotly_bs <- "<b>Tasas Pasivas DPF - Mensual (Bs.)</b><br>181 a 360 días"
titulo_plotly_usd <- "<b>Tasas Pasivas DPF - Mensual ($us)</b><br>181 a 360 días"

plotly_grafico_dpf_181_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_grafico_dpf_181_bs
saveRDS(plotly_grafico_dpf_181_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_dpf_181_bs.rds", compress = TRUE)

plotly_grafico_dpf_181_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_dpf_181_usd
saveRDS(plotly_grafico_dpf_181_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_dpf_181_usd.rds", compress = TRUE)

#COMERCIO EXTERIOR
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/comercio_exterior/exportaciones/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx"
range <- "B7:NX101"
sheet <- "ExpActProdMes 92-24 Valor"
col_names <-FALSE
from <- "01Jan1992"
to <- "01Feb2024"
by <- "month"
each <- 1

mi_paleta_plotly <-  pal_plotly(4)
tickformat_y <- ",.0d"
graph_type <-  "scatter"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `AGRICULTURA, GANADERÍA, CAZA SILVICULTURA Y PESCA`,
         `EXTRACCIÓN DE HIDROCARBUROS`,
         `EXTRACCIÓN  DE MINERALES`,
         `INDUSTRIA MANUFACTURERA`)

titulo_plotly_usd <- "<b>Exportaciones por Actividad Económica - Mensual</b><br>(En millones $us.)"
plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_expo_actividad_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_expo_actividad_lineal_y_stacked_usd
saveRDS(plotly_grafico_expo_actividad_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_expo_actividad_lineal_y_stacked_usd.rds", compress = TRUE)

sheet <- "ExpActProdMes 92-24 Peso"
data <- fun_excel(file,range,sheet,col_names,from,to,by,each)
conjunto_datos_usd <- data %>%
  select(fecha, 
         `AGRICULTURA, GANADERÍA, CAZA SILVICULTURA Y PESCA`,
         `EXTRACCIÓN DE HIDROCARBUROS`,
         `EXTRACCIÓN  DE MINERALES`,
         `INDUSTRIA MANUFACTURERA`)

titulo_plotly_usd <- "<b>Exportaciones por Actividad Económica - Mensual</b><br>(En toneladas)"
plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_expo_actividad_ton_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_expo_actividad_ton_lineal_y_stacked_usd

saveRDS(plotly_grafico_expo_actividad_ton_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_expo_actividad_ton_lineal_y_stacked_usd.rds", compress = TRUE)


#IMPORTACIONES CUODE USD####
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/comercio_exterior/importaciones/Bolivia - Importaciones Agregadas segun Clasificacion Uso o Destino Economico por Año y Mes, 1992 - 2024.xlsx"
range <- "B7:NX26"
sheet <- "IMP_VAL_MES_CUODE"
col_names <-FALSE
from <- "01Jan1992"
to <- "01Feb2024"
by <- "month"
each <- 1

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0d"
graph_type <-  "scatter"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `BIENES DE CONSUMO`,
         `MATERIAS PRIMAS Y PRODUCTOS INTERMEDIOS`,
         `BIENES DE CAPITAL`)

titulo_plotly_usd <- "<b>Importaciones (CUODE) - Mensual</b><br>(En millones $us)"
plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_impo_cuode_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_impo_cuode_lineal_y_stacked_usd
saveRDS(plotly_grafico_impo_cuode_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_impo_cuode_lineal_y_stacked_usd.rds", compress = TRUE)



#IMPORTACIONES ACTIVIDAD ECONOMICA PESO#

sheet <- "IMP_VOL_MES_CUODE"

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `BIENES DE CONSUMO`,
         `MATERIAS PRIMAS Y PRODUCTOS INTERMEDIOS`,
         `BIENES DE CAPITAL`)

titulo_plotly_usd <- "<b>Importaciones (CUODE) - Mensual</b><br>(En toneladas)"
plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_impo_cuode_ton_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_impo_cuode_ton_lineal_y_stacked_usd
saveRDS(plotly_grafico_impo_cuode_ton_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_impo_cuode_ton_lineal_y_stacked_usd.rds", compress = TRUE)



#SALDO COMERCIAL CUCI REV
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/comercio_exterior/saldo_comercial/Bolivia - Comercio Exterior segun Clasificacion Uniforme para el Comercio Internacional, 1992 - 2024.xlsx"
range <- "C8:CU93"
sheet <- "SALDO CUCI ANUAL 92-24"
col_names <-FALSE
from <- "01Dec1992"
to <- "01Dec2023"
by <- "year"
each <- 3
mi_secuencia <- rep(c("exportaciones", "importaciones", "saldo comercial"), times = 32)
tickformat_y <- ",.0d"
graph_type <-  "scatter"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

data <- cbind(mi_secuencia, data)

conjunto_datos_usd <- data %!>% 
  dplyr::filter(mi_secuencia == "saldo comercial") %!>% 
  select(fecha, 
         `ANIMALES Y PRODUCTOS ALIMENTICIOS`,
         `MATERIALES CRUDOS NO COMESTIBLES`,
         `COMBUSTIBLES Y LUBRICANTES`,
         `ACEITES, GRASAS ANIMAL Y VEGETAL`,
         `PRODUCTOS QUÍMICOS Y CONEXOS`,
         `ARTÍCULOS MANUFACTURADOS`,
         `MAQUINARIA Y EQUIPO DE TRANSPORTE`,
         `ARTÍCULOS MANUFACTURADOS DIVERSOS`,
         `ORO`)

titulo_plotly_usd <- "<b>Saldo Comercial - Anual (CUCI Rev.3)</b><br>(En millones $us)"
mi_paleta_plotly <- createPalette(9, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_saldo_cuci_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_saldo_cuci_lineal_y_stacked_usd

saveRDS(plotly_grafico_saldo_cuci_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_saldo_cuci_lineal_y_stacked_usd.rds", compress = TRUE)

#SALDO COMERCIAL GCE USD
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/comercio_exterior/saldo_comercial/Bolivia - Comercio Exterior segun Grandes Categorias Economicas, 1992 - 2024.xlsx"
range <- "B7:CT36"
sheet <- "COMEX GCE ANUAL 92-24"
col_names <-FALSE
from <- "01Dec1992"
to <- "01Dec2023"
by <- "year"
each <- 3
mi_secuencia <- rep(c("exportaciones", "importaciones", "saldo comercial"), times = 32)
tickformat_y <- ",.0d"
graph_type <-  "scatter"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

data <- cbind(mi_secuencia, data)

conjunto_datos_usd <- data %!>% 
  dplyr::filter(mi_secuencia == "saldo comercial") %!>% 
  select(fecha, 
         `ALIMENTOS Y BEBIDAS`,
         `SUMINISTROS INDUSTRIALES`,
         `COMBUSTIBLES Y LUBRICANTES`,
         `BIENES DE CAPITAL`,
         `EQUIPO DE TRANSPORTE Y SUS PIEZAS`,
         `ARTÍCULOS DE CONSUMO`)

mi_paleta_plotly <- createPalette(6, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL
titulo_plotly_usd <- "<b>Saldo Comercial - Anual (GCE Rev.3)</b><br>(En millones $us)"
plotly_grafico_saldo_gce_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_saldo_gce_lineal_y_stacked_usd

saveRDS(plotly_grafico_saldo_gce_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_saldo_gce_lineal_y_stacked_usd.rds", compress = TRUE)

#PRODUCCION CEMENTO ANUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/construccion/1.produccion_venta_consumo/1. Bolivia - Produccion de Cemento por Departamento segun Año y Mes 1991 - 2024.xlsx"
range <- "B4:I466"
sheet <- "C1"
col_names <-TRUE
from <- "01Dec1991"
to <- "01Dec2023"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)
mi_paleta_plotly <-  pal_plotly(6)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"


data <- datos_sf(file,sheet,range,col_names)

data %<>%  
  dplyr::filter(!(PERÍODO %in% c("Enero", "Febrero","Marzo", "Abril","Mayo", "Junio","Julio", "Agosto","Septiembre", 
                                 "Octubre","Noviembre","Diciembre")))%<>%  
  select(`CHUQUISACA`,
         `LA PAZ`,
         `COCHABAMBA`,
         `ORURO`,
         `TARIJA`,
         `SANTA CRUZ`)%<>%  
  mutate(across(where(is.numeric), ~  . /1000 )) 

conjunto_datos_usd <- cbind(fecha,data)  
titulo_plotly_usd <- "<b>Producción de Cemento - Anual</b><br>(En miles de toneladas)"

plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_prod_cemento_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_prod_cemento_lineal_y_stacked_usd

saveRDS(plotly_grafico_prod_cemento_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_prod_cemento_lineal_y_stacked_usd.rds", compress = TRUE)


#VENTA CEMENTO ANUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/construccion/1.produccion_venta_consumo/2. Bolivia - Ventas de Cemento por Departamento segun Año y Mes 1991 - 2024.xlsx"
range <- "B4:I466"
sheet <- "C2"
fecha <- fun_fecha(from,to,by,each)
mi_paleta_plotly <-  pal_plotly(6)

data <- datos_sf(file,sheet,range,col_names)

data %<>%  
  dplyr::filter(!(PERIODO %in% c("Enero", "Febrero","Marzo", "Abril","Mayo", "Junio","Julio", "Agosto","Septiembre", 
                                 "Octubre","Noviembre","Diciembre")))%<>%  
  select(`CHUQUISACA`,
         `LA PAZ`,
         `COCHABAMBA`,
         `ORURO`,
         `TARIJA`,
         `SANTA CRUZ`)%<>%  
  mutate(across(where(is.numeric), ~  . /1000 )) 

conjunto_datos_usd <- cbind(fecha,data)  
titulo_plotly_usd <- "<b>Venta de Cemento - Anual</b><br>(En miles de toneladas)"
plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_venta_cemento_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_venta_cemento_lineal_y_stacked_usd

saveRDS(plotly_grafico_venta_cemento_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_venta_cemento_lineal_y_stacked_usd.rds", compress = TRUE)

#SUPERFICIE PERMISOS DE CONSTRUCCION ANUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/construccion/2.permisos_construccion/1. Bolivia - Superficie Permisos de Construcción desagregada por Tipo de Trámite según año y mes 2008 - 2024.xls"
range <- "B9:F217"
sheet <- "Sup.porTipodeTrám.segúnAñoyMes"
col_names <-TRUE
from <- "01Dec2008"
to <- "01Dec2023"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)
mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"

data <- datos_sf(file,sheet,range,col_names)

data %<>%  
  dplyr::filter(!(PERIODO %in% c("Enero", "Febrero","Marzo", "Abril","Mayo", "Junio","Julio", "Agosto","Septiembre", 
                                 "Octubre","Noviembre","Diciembre")))%<>%  
  select(`APROBACIÓN DE PLANOS DE CONSTRUCCIÓN`,
         `LEGALIZACIÓN Y REGULARIZACIÓN`,
         `OTROS`)

conjunto_datos_usd <- cbind(fecha,data)  
titulo_plotly_usd <- "<b>Superficie en Permisos de Construcción - Anual</b><br>(En metros cuadrados)"
plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_super_permis_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_super_permis_lineal_y_stacked_usd

saveRDS(plotly_grafico_super_permis_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_super_permis_lineal_y_stacked_usd.rds", compress = TRUE)

#NRO DE PERMISOS DE CONSTRUCCION ANUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/construccion/2.permisos_construccion/2. Bolivia - Número de Permisos de Construcción desagregado por Tipo de Trámite según año y mes 2008 - 2024.xls"
sheet <- "Núm.porTipodeTrám.segúnAñoyMes"
data <- datos_sf(file,sheet,range,col_names)

data %<>%  
  dplyr::filter(!(PERIODO %in% c("Enero", "Febrero","Marzo", "Abril","Mayo", "Junio","Julio", "Agosto","Septiembre", 
                                 "Octubre","Noviembre","Diciembre")))%<>%  
  select(`APROBACIÓN DE PLANOS DE CONSTRUCCIÓN`,
         `LEGALIZACIÓN Y REGULARIZACIÓN`,
         `OTROS`)

conjunto_datos_usd <- cbind(fecha,data)  
titulo_plotly_usd <- "<b>Número de Permisos de Construcción - Anual</b><br>(En número de registros)"
plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_nro_permis_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_nro_permis_lineal_y_stacked_usd

saveRDS(plotly_grafico_nro_permis_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_nro_permis_lineal_y_stacked_usd.rds", compress = TRUE)

#CRECIMIENTO PIB CONSTANTE DEMANDA FINAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/cuentas_nacionales/1.pib_anual/1.oferta_final_demanda_final/03.01.02.xlsx"
range <- "B12:AK19"
sheet <- "03.01.02"
col_names <-FALSE
from <- "01Dec1989"
to <- "01Dec2023"
by <- "year"
each <- 1
mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ".1%"
graph_type <-  "scatter"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %<>%  
  select(`fecha`,
         `PRODUCTO  INTERNO  BRUTO`,
         `CONSUMO  FINAL`,
         `FORMACIÓN  BRUTA  DE  CAPITAL  FIJO`)%<>%  
  mutate(across(where(is.numeric), ~  . /100 )) 

titulo_plotly_usd <- "<b>Crecimiento PIB, FBKF y Consumo - Anual</b><br>(En porcentaje)"
plotly_grafico_crec_pib_final_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_crec_pib_final_lineal_y_stacked_usd
saveRDS(plotly_grafico_crec_pib_final_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_crec_pib_final_lineal_y_stacked_usd.rds", compress = TRUE)


#PIB PER CAPITA BOLIVIA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/cuentas_nacionales/3.pib_departamental/10.bolivia/100106.xlsx"
range <- "B12:AK22"
sheet <- "100106"
col_names <-FALSE
from <- "01Dec1988"
to <- "01Dec2022"
by <- "year"
each <- 1

mi_paleta_plotly <- createPalette(9, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL

tickformat_y <- ",.0d"
graph_type <-  "scatter"
barmode <-  FALSE


data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %!>% 
  select(`fecha`,
         `Chuquisaca`,
         `La Paz`,
         `Cochabamba`,
         `Oruro`,
         `Potosí`,
         `Tarija`,
         `Santa Cruz`,
         `Beni`,
         `Pando`)

titulo_plotly_usd <- "<b>PIB per cápita Precios de Mercado - Anual</b><br>(En Bs.)"
plotly_grafico_nom_pib_per_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_nom_pib_per_lineal_y_stacked_usd

saveRDS(plotly_grafico_nom_pib_per_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_nom_pib_per_lineal_y_stacked_usd.rds", compress = TRUE)

#CRECIMIENTO PIB CONSTANTE DEMANDA FINAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/cuentas_nacionales/4.formacion_bruta_capital_fijo/1.constante/01.02.xlsx"
range <- "A11:AI19"
sheet <- "01.02"
col_names <-FALSE
from <- "01Dec1989"
to <- "01Dec2022"
by <- "year"
each <- 1
mi_paleta_plotly <-  pal_plotly(2)
tickformat_y <- ".0%"
graph_type <-  "bar"
barmode <-  FALSE

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %<>%  
  select(`fecha`,
         `PÚBLICO`,
         `PRIVADO`)%<>%  
  mutate(across(where(is.numeric), ~  . /100 )) 
colnames(conjunto_datos_usd) <-  c("fecha","FBKF Estatal","FBKF Privado")
titulo_plotly_usd <- "<b>Crecimiento Real FBKF - Anual</b><br>(En porcentaje)"
plotly_grafico_fbkf_lineal_y_stacked_usd <- plotly_solo_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y,graph_type,barmode)
plotly_grafico_fbkf_lineal_y_stacked_usd

saveRDS(plotly_grafico_fbkf_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_fbkf_lineal_y_stacked_usd.rds", compress = TRUE)

#CRECIMIENTO PIB CONSTANTE DEMANDA FINAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/cuentas_nacionales/9.indice_global_actividad_economica/01.04.xlsx"
range <- "A12:GK31"
sheet <- "01.04"
col_names <-FALSE
from <- "01Jan2008"
to <- "01Dec2023"
by <- "month"
each <- 1
mi_paleta_plotly <-  pal_plotly(2)
tickformat_y <- ".0%"

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %!>%   
  select(`fecha`,
         `INDICE GENERAL`,
         `INDUSTRIA MANUFACTURERA`)%!>% 
  mutate(across(where(is.numeric), ~  . /100 )) 

titulo_plotly_usd <- "<b>Variación Interanual IGAE - Mensual</b><br>(En porcentaje)"
plotly_grafico_igae_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_igae_lineal_y_stacked_usd

saveRDS(plotly_grafico_igae_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_igae_lineal_y_stacked_usd.rds", compress = TRUE)

#1.PRODUCCION DE PETROLEO Y GAS NATURAL MENSUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/hidrocarburos_mineria/1.hidrocarburos/8. Bolivia - Indice de Volumen y Variaciones de Producción de Petroleo y Gas Natural segun Año y Mes 1990 - 2024.xlsx"
range <- "B4:E485"
sheet <- "INDICE"
col_names <-TRUE
from <- "01Jan1990"
to <- "01Mar2024"
by <- "month"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)
data %<>%  
  dplyr::filter(!(PERÍODO %in% c("1990", "1991","1992", "1993","1994","1995","1996", "1997","1998","1999","2000", 
                                 "2001", "2002","2003", "2004","2005","2006","2007", "2008","2009","2010","2011",
                                 "2012", "2013","2014", "2015","2016","2017","2018", "2019","2020","2021","2022(p)",
                                 "2023(p)", "2024(p)")))%<>%  
  select(`GAS NATURAL`)

data_a <- cbind(fecha,data)
colnames(data_a) <- c("fecha", "Gas Natural")
range <- "B8:E476"
sheet <- "Variacion a Similar Periodo Ant"

data <- datos_sf(file,sheet,range,col_names)
data %<>%  
  dplyr::filter(!(PERÍODO %in% c("1991","1992", "1993","1994","1995","1996", "1997","1998","1999","2000", 
                                 "2001", "2002","2003", "2004","2005","2006","2007", "2008","2009","2010","2011",
                                 "2012", "2013","2014", "2015","2016","2017","2018", "2019","2020","2021","2022(p)",
                                 "2023(p)", "2024(p)")))%<>%  
  select(`GAS NATURAL`)%<>%  
  mutate(across(where(is.numeric), ~  . /100 )) 

data_1991_to_1999 <- rep(0, 12)
nueva_columna <- c(data_1991_to_1999, data$`GAS NATURAL`)
data <- tibble::tibble(TOTAL = nueva_columna)
data_b <- cbind(fecha,data)
colnames(data_b) <- c("fecha","Variación Interanual Índice Volúmen de Producción")

fig <- plot_ly()

fig <- fig %>% add_trace(
  x = data_a$fecha,
  y = data_a$`Gas Natural`,
  type = 'scatter',
  mode = 'lines',
  name = 'Gas Natural',
  yaxis = 'y1'
)

fig <- fig %>% add_trace(
  x = data_b$fecha,
  y = data_b$`Variación Interanual Índice Volúmen de Producción`,
  type = 'scatter',
  mode = 'lines',
  name = 'Índice de Volumen de Producción Internual',
  yaxis = 'y2'
)

fig <- fig %>% plotly::layout(
  xaxis = list(title = 'Fecha'),
  yaxis = list(
    side = 'left'
  ),
  yaxis2 = list(
    overlaying = 'y',
    side = 'right'
  )
)

titulo_plotly_bs <- "<b>Producción de Gas Natural - Mensual</b><br>"
titulo_plotly_bs_y_1 <- "Millones de metros cúbicos"
titulo_plotly_bs_y_2 <- "Porcentaje"
tickformat_y_1 <- ",.0d"
tickformat_y_2 <- ".0%"

plotly_grafico_gas_indice_lineal_y_stacked_usd <- plotly_nomi_crec_bs(fig,titulo_plotly_bs,titulo_plotly_bs_y_1,titulo_plotly_bs_y_2,tickformat_y_1,tickformat_y_2)
plotly_grafico_gas_indice_lineal_y_stacked_usd

saveRDS(plotly_grafico_gas_indice_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_gas_indice_lineal_y_stacked_usd.rds", compress = TRUE)

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

titulo_plotly_usd <- "<b>Producción Nacional de Minerales - Mensual</b><br>(En toneladas métricas)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

mi_paleta_plotly <- createPalette(7, c("#ff0000", "#00ff00", "#0000ff"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_prod_minerales_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_prod_minerales_lineal_y_stacked_usd
saveRDS(plotly_grafico_prod_minerales_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_prod_minerales_lineal_y_stacked_usd.rds", compress = TRUE)

#1.INDICE DE VOLUMEN DE PRODUCCION

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
conjunto_datos_usd
titulo_plotly_usd <- "<b>Indice de Volumen de Producción Industrial - Anual</b><br>(Variación interanual)"
tickformat_y <- ".0%"
mi_paleta_plotly <- pal_plotly(1)

plotly_grafico_var_indice_indus_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_var_indice_indus_lineal_y_stacked_usd
saveRDS(plotly_grafico_var_indice_indus_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_var_indice_indus_lineal_y_stacked_usd.rds", compress = TRUE)

#1.INDICE DE VOLUMEN DE VENTAS

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

titulo_plotly_usd <- "<b>Indice de Volumen de Ventas de la Industria - Anual</b><br>(Variación interanual)"
tickformat_y <- ".0%"
mi_paleta_plotly <- pal_plotly(1)

plotly_grafico_var_indice_indus_ventas_lineal_y_stacked_usd <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_var_indice_indus_ventas_lineal_y_stacked_usd
saveRDS(plotly_grafico_var_indice_indus_ventas_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_var_indice_indus_ventas_lineal_y_stacked_usd.rds", compress = TRUE)


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
titulo_plotly_usd <- "<b>Indice de Precios al Consumidor - Mensual</b><br>(Porcentaje)"

mi_paleta_plotly <- pal_plotly(2)

conjunto_datos_plotly <-  conjunto_datos_usd%!>%     
  pivot_longer(cols = -fecha, names_to = "variables", values_to = "valores")

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
plotly_grafico_var_indice_consumidor_lineal_y_stacked_usd <- combinacion_graficos %>%
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


plotly_grafico_var_indice_consumidor_lineal_y_stacked_usd
saveRDS(plotly_grafico_var_indice_consumidor_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_var_indice_consumidor_lineal_y_stacked_usd.rds", compress = TRUE)

#1.SALARIO MINIMO####
file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ine/estadistica_economica/salario_remuneraciones/1.salario_minimo/BOLIVIA - SALARIO MÍNIMO NACIONAL, 1991 - 2024.xlsx"
range <- "C7:D40"
sheet <- "CUADRO"
col_names <-TRUE
from <- "01Dec1992"
to <- "01Dec2024"
by <- "year"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file,sheet,range,col_names)
conjunto_datos_usd <- cbind(fecha,data)
colnames(conjunto_datos_usd) <- c("fecha","Salario Mínimo Nacional","Variación Porcentual")

titulo_plotly_usd <- "<b>Salario Mínimo Nacional - Anual</b><br>(En bolivianos)"
mi_paleta_plotly <- createPalette(2, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL

# Ajustar el rango de fechas para mostrar solo los datos disponibles
fecha_min <- min(conjunto_datos_usd$fecha)
fecha_max <- max(conjunto_datos_usd$fecha)

# Establecer el formato de los ticks en los ejes y
tickformat_y_1 <- ".0%"
tickformat_y_2 <- ",.0f"

y_1 <- conjunto_datos_usd$`Variación Porcentual`
y_2 <-  conjunto_datos_usd$`Salario Mínimo Nacional`
name_1 <- 'Variación Porcentual'
name_2 <- 'Salario Mínimo Nacional'
type_1 <- 'bar'
type_2 <- "scatter"

plotly_grafico_sal_nom_lineal_y_stacked_usd <- plotly_nomi_crec_usd_1(conjunto_datos_usd, name_1,name_2,fecha,y_1,y_2,type_2,type_1,titulo_plotly_usd, mi_paleta_plotly,fecha_min,fecha_max,tickformat_y_1,tickformat_y_2)
plotly_grafico_sal_nom_lineal_y_stacked_usd

saveRDS(plotly_grafico_sal_nom_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_sal_nom_lineal_y_stacked_usd.rds", compress = TRUE)

#1. INGRESOS ejecucion_spnf####

file <- "C:/Users/Mauro/Desktop/bases_de_datos/bolivia/ministerio_economia/1.estadisticas_fiscales/1.cifras_fiscales_operativas_caja/1.ejecucion_spnf/1.ejecucion_spnf.xlsx"
col_names <-FALSE

from <- "01Dec1990"
to <- "01Dec2022"
by <- "year"
each <- 1

range <- "A6:AH51"
sheet <- "anual"

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_bs <- data %!>% 
  select(fecha, `INGRESOS TRIBUTARIOS`, `IMPUESTOS SOBRE HIDROCARBUROS`, `VENTA DE HIDROCARBUROS`,`VENTA DE OTRAS EMPRESAS`,`TRANSFERENCIAS CORRIENTES (Ingresos)`,`OTROS INGRESOS CORRIENTES`, `INGRESOS DE CAPITAL`) 

mi_paleta_plotly <- createPalette(7, c("#ff0000","#0000ff","#00ff00","#ffff00"))
names(mi_paleta_plotly) <- NULL

titulo_plotly_bs <- "<b>Ingresos Totales SPNF - Anual (1990-2022)</b><br>(En millones Bs.)"
tickformat_y <- ",.0f"
graph_type <-  "bar"
barmode <-  "stack"

plotly_grafico_spnf_lineal_bs <-  plotly_combi_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_spnf_lineal_bs

plotly_grafico_ingre_spnf_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ingre_spnf_lineal_y_stacked_bs

saveRDS(plotly_grafico_ingre_spnf_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_ingre_spnf_lineal_y_stacked_bs.rds", compress = TRUE)


#1.EGRESOS ejecucion_spnf####

conjunto_datos_bs <- data %!>% 
  select(fecha, `SERVICIOS PERSONALES`, `BIENES Y SERVICIOS`, `INTERESES Y COMIS. DEUDA EXTERNA`,`INTERESES Y COMIS. DEUDA INTERNA`,`TRANSFERENCIAS CORRIENTES (Egresos)`,`OTROS EGRESOS CORRIENTES`, `EGRESOS DE CAPITAL`) 
conjunto_datos_bs
titulo_plotly_bs <- "<b>Egresos Totales SPNF - Anual (1990-2022)</b><br>(En millones Bs.)"
tickformat_y <- ",.0f"
graph_type <-  "bar"
barmode <-  "stack"

plotly_grafico_spnf_lineal_bs <-  plotly_combi_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_spnf_lineal_bs

plotly_grafico_egre_spnf_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_egre_spnf_lineal_y_stacked_bs

saveRDS(plotly_grafico_egre_spnf_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/plotly_grafico_egre_spnf_lineal_y_stacked_bs.rds", compress = TRUE)


