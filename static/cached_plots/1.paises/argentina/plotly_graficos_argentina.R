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
        zeroline = T,
        zerolinewidth = 1,
        zerolinecolor = "gray",
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
        tick0 = min(conjunto_datos_bs$fecha),
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

#Banco Central de Argentina (BCRA)####
#Sector Monetario##
#Balance BCRA#

#BALANCE ACTIVO BCRA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/banco_central/Serieanual.xls"
range <- "A8:P111"
sheet <- "anual"
col_names <- FALSE
fecha <- dmy(c("31-dec-10", "31-dec-11", "31-dec-12", "31-dec-13", "31-dec-14", 
               "31-dec-15", "31-dec-16", "31-dec-17", "31-dec-18", "31-dec-19", 
               "31-dec-20", "31-dec-21", "31-dec-22", "31-dec-23", "15-jun-24"))
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

data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(
    `fecha`,
    `RESERVAS INTERNACIONALES`,
    `TITULOS PUBLICOS`,
    `ADELANTOS TRANSITORIOS AL GOBIERNO NACIONAL`,
    `APORTES A ORGANISMOS INTERNACIONALES`,
    `DERECHOS POR OPERACIONES DE PASES`
  )%>%
  mutate(across(where(is.numeric), ~  . / 1000000000))

mi_paleta_plotly <- pal_plotly(5)
titulo_plotly_bs <- "<b>Balance del Activo del BCRA</b><br>(Billones de $.)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

plotly_grafico_activo_lineal_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_activo_lineal_bs
plotly_grafico_ar_activo_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_activo_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_activo_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_activo_lineal_y_stacked_bs.rds", compress = TRUE)

#BALANCE PASIVO BCRA

conjunto_datos_bs <- data %>%
  select(
    `fecha`,
    `BASE MONETARIA`,
    `CUENTAS CORRIENTES EN OTRAS MONEDAS`,
    `DEPOSITOS DEL GOBIERNO NACIONAL Y OTROS`,
    `TITULOS EMITIDOS POR EL B.C.R.A.`,
    `OBLIGACIONES POR OPERACIONES DE PASE`,
    `OTROS PASIVOS`,
    `TOTAL DEL PATRIMONIO NETO`
  )%>%
  mutate(across(where(is.numeric), ~  . / 1000000000))

mi_paleta_plotly <- pal_plotly(7)
titulo_plotly_bs <- "<b>Balance del Pasivo del BCRA</b><br>(Billones de $.)"

plotly_grafico_pasivo_lineal_bs <- plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_pasivo_lineal_bs
plotly_grafico_ar_pasivo_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_pasivo_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_pasivo_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_pasivo_lineal_y_stacked_bs.rds", compress = TRUE)

#Sector Externo##
#Balanza Cambiaria#

#INGRESO DIVISAS
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/banco_central/Nuevo-anexo-MC.xlsx"
range <- "B15:BA267"
sheet <- "Balance Cambiario"
col_names <- FALSE
from <- "01Apr2003"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- datos(file,sheet,range,col_names, from, to, by, each)

colnames(data) <- c(
  
              "fecha",  
  
             "Saldo Cuenta Corriente Cambiaria", 
             
             "Saldo Total Bienes", 
             "Cobros de exportaciones de bienes","Pagos de importaciones de bienes", 
             
             "Saldo Total Servicios", 
             "Ingreso Servicios", "Egreso Servicios", 
             
             "Saldo Ingreso Primario", 
             "Saldo Total Intereses", 
             
             "Ingresos Intereses", "Egresos Intereses",
             
             "Pagos de intereses al FMI", "Pagos de intereses a otros Org. Int. Y otros bilaterales", "Otros pagos de intereses", "Otros pagos de Gobierno Nacional",
             
             "Saldo Utilidades y Dividendos y otras Rentas",
             "Ingresos Utilidades y dividendos y otras rentas", "Egresos Utilidades y dividendos y otras rentas",
             
             "Saldo Ingreso Secundario",
             "Ingreso Ingreso Secundario", "Egreso Ingreso Secundario",
             
             "Cuenta de capital cambiaria",
             
             "Saldo Cuenta financiera",
             
             "Saldo Inversión Directa de no Residentes", 
             "Ingresos Inversión directa de no residentes", "Egresos Inversión directa de no residentes",
             
             "Saldo Inversión de Portafolio de no Residentes",
             "Ingresos Inversión de portafolio de no residentes","Egresos Inversión de portafolio de no residentes",
             
             "Saldo Préstamos Financieros, Títulos de deuda y Líneas de Crédito", 
             "Ingresos Préstamos financieros, títulos de deuda y líneas de crédito", "Egresos Préstamos financieros, títulos de deuda y líneas de crédito",
             
             "Saldo Operaciones con el FMI",
             "Ingresos Operaciones con el FMI", "Egresos Operaciones con el FMI",
             
             "Saldo Préstamos de Otros Org. Int. y Otros Bilaterales",
             "Ingresos Préstamos de otros Org. Int. y otros bilaterales", "Egresos Préstamos de otros Org. Int. y otros bilaterales",
             
             "Saldo Formación  de Activos Externos del Sector Privado no Financiero",
             "Ingresos Formación  de activos externos del sector privado no financiero", "Egresos Formación  de activos externos del sector privado no financiero",
             
             "Operaciones de canje por transferencias con el exterior",
             
             "Formación de activos externos del sector financiero (PGC)",
             
             "Formación de activos externos del sector público",
             
             "Compra-venta de títulos valores",
             
             "Otras operaciones del sector público nacional (neto)",
             
             "Otros movimientos netos",
             
             "Concepto no informado por el cliente (neto)",
             
             "Variación de Reservas Internacionales por transacciones",
             
             "Variación contable de Reservas Internacionales del BCRA",
             
             "Ajuste por tipo de pase y valuación",
             
             "Item de Memorandum: Pago del saldo en moneda extranjera por uso de tarjetas en el exterior(7)"
             )

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Saldo Total Bienes`,
         `Saldo Total Servicios`,
         `Saldo Ingreso Primario`,
         `Saldo Ingreso Secundario`)

titulo_plotly_usd <- "<b>Balance Cambiario</b><br>(Millones $us)"
mi_paleta_plotly <- createPalette(4, c("#ff0000","#0000ff","#00ff00"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_ar_ingreso_cambiaria_lineal_y_stacked_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_ar_ingreso_cambiaria_lineal_y_stacked_usd

saveRDS(plotly_grafico_ar_ingreso_cambiaria_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_ingreso_cambiaria_lineal_y_stacked_usd.rds", compress = TRUE)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Saldo Inversión Directa de no Residentes`,
         `Saldo Inversión de Portafolio de no Residentes`,
         `Saldo Préstamos Financieros, Títulos de deuda y Líneas de Crédito`,
         `Saldo Operaciones con el FMI`,
         `Saldo Préstamos de Otros Org. Int. y Otros Bilaterales`,
         `Saldo Formación  de Activos Externos del Sector Privado no Financiero`)

titulo_plotly_usd <- "<b>Balance Cambiario (Cuenta Financiera)</b><br>(Millones $us)"
mi_paleta_plotly <- createPalette(6, c("#ff0000","#0000ff","#00ff00", "#ffff00"))
names(mi_paleta_plotly) <- NULL

plotly_grafico_ar_egreso_cambiaria_lineal_usd <-  plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_ar_egreso_cambiaria_lineal_usd
saveRDS(plotly_grafico_ar_egreso_cambiaria_lineal_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_egreso_cambiaria_lineal_usd.rds", compress = TRUE)

#Base Monetaria#

#ORIGEN BASE MONETARIA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/banco_central/1.balance_consolidado_sistema_financiero/balbcrhis.xls"
range <- "G886:AP1124"
col_names <- FALSE
from <- "01Jan2006"
to <- "01May2024"
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
  
  "Total Activos Externos Netos",
  "Oro y Divisas",
  "Resultados de Operaciones de Cambio",
  "Aportes a Organismos Internacionales",
  "Asignaciones DEG",
  "Obligacioens con Org. Internacionales",
  "Total Sector Oficial",
  "Sector Oficial en Moneda Nacional",
  "Gobierno Nacional en Moneda Nacional",
  "Adelantos Transitorios",
  "Total Valores Públicos",
  "Art. 51 C.O.",
  "Otros Valores Públicos",
  "Rec. dev sobre cred.",
  "Sector Oficial en Moneda Extranjera",
  "Total Valores Públicos en Moneda Extranjera",
  "Financ. Ext. al Gobierno Nacional",
  "Créditos a Entidades Financieras en Moneda Nacional",
  "Total Fuentes de Creación",
  "Total Fuentes de Absorción",
  "Depósitos Totales",
  "Depósitos Oficiales",
  "Depósitos Oficiales en Moneda Nacional",
  "Depósitos Oficiales en Moneda Extranjera",
  "Dirección Nacional de Rec.Previs.",
  "Diversos",
  "Otras Obligaciones en Moneda Nacional",
  "Depósitos de Entidades Financieras en Moneda Extranjera",
  "Depósitos de Entidades Financieras por Cuenta y Orden del BCRA",
  "Títulos Emitidos por el BCRA",
  "Cuentas Varias",
  "Total Base Monetaria",
  "Total Circulación Monetaria",
  "Circulación Monetaria fuera del Sistema Financiero",
  "Circulación Monetaria en Entidades Financieras",
  "Depósitos de Entidades Financieras en Cuenta Corriente"
  )

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

#ORIGEN BASE MONETARIA
conjunto_datos_bs <- data %>%
  select(fecha, 
         `Total Activos Externos Netos`,
         `Sector Oficial en Moneda Nacional`,
         `Sector Oficial en Moneda Extranjera`,
         `Créditos a Entidades Financieras en Moneda Nacional`)%>%
  mutate(across(where(is.numeric), ~  . / 1000000000))

titulo_plotly_bs <- "<b>Creación Base Monetaria</b><br>(Billones de $.)"
graph_type <-  "bar"
barmode <-  "stack"
mi_paleta_plotly <- pal_plotly(4)


plotly_grafico_base_origen_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_base_origen_lineal_bs

plotly_grafico_ar_base_origen_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_base_origen_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_base_origen_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_base_origen_lineal_y_stacked_bs.rds", compress = TRUE)

#DESTINO BASE MONETARIA
conjunto_datos_bs <- data %>%
  select(fecha, 
         `Depósitos Oficiales en Moneda Nacional`,
         `Depósitos Oficiales en Moneda Extranjera`,
         `Depósitos de Entidades Financieras en Moneda Extranjera`,
         `Títulos Emitidos por el BCRA`,
         `Cuentas Varias`)%>%
  mutate(across(where(is.numeric), ~  . / 1000000000))

titulo_plotly_bs <- "<b>Absorción Base Monetaria</b><br>(Billones de $.)"
mi_paleta_plotly <- pal_plotly(5)

plotly_grafico_base_destino_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs,tickformat_y)
plotly_lineal <- plotly_grafico_base_destino_lineal_bs
plotly_grafico_ar_base_destino_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_base_destino_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_base_destino_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_base_destino_lineal_y_stacked_bs.rds", compress = TRUE)


#DEUDA BCRA SECTOR PRIVADO
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/banco_central/panhis.xls"
sheet <- "Cuadro"
range <- "L990:O1124"
col_names <- FALSE
#FORMATO FECHA
from <- "01Jan2014"
to <- "01May2024"
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
  
"Letras y Notas del BCRA (en pesos)",
"Letras y Notas del BCRA (en dólares estadounidenses)",
"Total posición neta de pases",
"Pases Pasivos"

)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(fecha, 
         `Letras y Notas del BCRA (en pesos)`,
         `Letras y Notas del BCRA (en dólares estadounidenses)`,
         `Pases Pasivos`)%>%
  mutate(across(where(is.numeric), ~  . / 1000000))


titulo_plotly_bs <- "<b>Pasivos Remunerados BCRA</b><br>(Billones de $.)"

mi_paleta_plotly <- pal_plotly(3)

plotly_grafico_deuda_bcb_privados_lineal_bs <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_deuda_bcb_privados_lineal_bs

plotly_grafico_deuda_bcra_privados_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_deuda_bcra_privados_lineal_y_stacked_bs

saveRDS(plotly_grafico_deuda_bcra_privados_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_deuda_bcra_privados_lineal_y_stacked_bs.rds", compress = TRUE)

#RESERVAS INTERNACIONALES BCRA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/banco_central/Serieanual.xls"
range <- "A8:P111"
sheet <- "anual"
col_names <- FALSE
fecha <- dmy(c("31-dec-10", "31-dec-11", "31-dec-12", "31-dec-13", "31-dec-14", 
               "31-dec-15", "31-dec-16", "31-dec-17", "31-dec-18", "31-dec-19", 
               "31-dec-20", "31-dec-21", "31-dec-22", "31-dec-23", "15-jun-24"))
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

data <- cbind(fecha, data) 
data <- as_tibble(data)

conjunto_datos_bs <- data %>%
  select(
    `fecha`,
    `Oro (Neto de Previsiones)`,
    `Divisas`,
    `Colocaciones realizables en divisas`,
    `Instrumentos Derivados sobre Reservas Internacionales`,
    `Convenios Multilaterales de Crédito`
  )%>%
  mutate(across(where(is.numeric), ~  . / 1000000000))

mi_paleta_plotly <- pal_plotly(5)
titulo_plotly_bs <- "<b>Reservas Internacionales Brutas</b><br>(Billones de $.)"
tickformat_y <- ",d"
graph_type <-  "scatter"
barmode <-  FALSE

plotly_grafico_lineal_reservas_bs  <-  plotly_bs(conjunto_datos_bs, mi_paleta_plotly, titulo_plotly_bs, tickformat_y)
plotly_lineal <- plotly_grafico_lineal_reservas_bs

plotly_grafico_ar_reservas_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_reservas_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_reservas_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_reservas_lineal_y_stacked_bs.rds", compress = TRUE)

#INDEC ARGENTINA####

#EXPORTACIONES POR ACTIVIDAD ECONOMICA USD
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/3.comercio_exterior/1.complejos_exportadores/complexp_variacion_1993_2023.xls"
range <- "A7:T71"
sheet <- "2006-2023"
col_names <-FALSE
from <- "01Dec2005"
to <- "01Dec2023"
by <- "year"
each <- 1

mi_paleta_plotly <-  pal_plotly(7)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Sector oleaginoso`,
         `Sector cerealero`,
         `Complejo automotriz`,
         `Complejo petrolero petroquímico`,
         `Sector minero metalífero y litio`,
         `Sector bovino`,
         `Resto de exportaciones`)

titulo_plotly_usd <- "<b>Complejos Exportadores</b><br>(Millones $us)"
plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_ar_expo_actividad_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_expo_actividad_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_expo_actividad_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_expo_actividad_lineal_y_stacked_usd.rds", compress = TRUE)

#IMPORTACIONES POR ACTIVIDAD ECONOMICA USD
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/3.comercio_exterior/2.intercambio_comercial_argentino/imp_uso_econ_80_23.xls"
range <- "A5:T11"
sheet <- "2005-2023"
col_names <-FALSE
from <- "01Dec2005"
to <- "01Dec2023"
by <- "year"
each <- 1

mi_paleta_plotly <-  pal_plotly(7)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"

data <- fun_excel(file,range,sheet,col_names,from,to,by,each)

conjunto_datos_usd <- data %>%
  mutate(across(where(is.numeric), ~  . / 1000))

titulo_plotly_usd <- "<b>Importaciones usos económicos</b><br>(Millones $us.)"
plotly_lineal <- plotly_combi_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd,tickformat_y)
plotly_grafico_ar_impo_cuode_lineal_y_stacked_usd <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_impo_cuode_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_impo_cuode_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_impo_cuode_lineal_y_stacked_usd.rds", compress = TRUE)

#CONSTRUCCION
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/4.construccion/sh_isac_2024.xls"
range <- "K8:K166"
sheet <- "Cuadro 1"
col_names <-FALSE
from <- "01Feb2012"
to <- "01Apr2024"
by <- "month"
each <- 1

mi_paleta_plotly <-  pal_plotly(1)
tickformat_y <- ",.0%"

data <- datos(file, sheet, range, col_names, from, to, by, each)

colnames(data) <- c("fecha","Serie Tendencia Ciclo")

conjunto_datos_usd <- data %>%
  mutate(across(where(is.numeric), ~  . / 100))

titulo_plotly_usd <- "<b>ISAC Serie Tendencia-Ciclo</b><br>Variación intermensual"
plotly_grafico_ar_prod_cemento_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_prod_cemento_lineal_y_stacked_usd

saveRDS(plotly_grafico_ar_prod_cemento_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_prod_cemento_lineal_y_stacked_usd.rds", compress = TRUE)

#CRECIMIENTO REAL PIB 
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/6.cuentas_nacionales/1.agregados_macroeconomicos/1.pib/sh_oferta_demanda_06_24.xls"
range <- "A7:DJ15"
sheet <- "cuadro 2"
col_names <-FALSE
from <- "01Dec2005"
to <- "01Dec2023"
by <- "year"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

data %<>%
  pivot_longer(!...1)  %<>%
  pivot_wider(names_from = "...1",
              values_from = "value")  %<>%
  dplyr::select(-name)


# Vector con los años
anios <- c(
  "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", 
  "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", 
  "2021", "2022", "2023"
)

# Vector con los trimestres y Total
trimestres <- c(
  "1º trimestre", "2º trimestre", "3º trimestre", "4º trimestre", "Total"
)

# Crear el vector combinando años con trimestres
fecha <- c()

for (anio in anios) {
  for (trimestre in trimestres) {
    fecha <- c(fecha, paste(anio, trimestre))
  }
}


data <- cbind(fecha, data) 
data <- as_tibble(data)

data %<>%
  dplyr::filter((fecha %in% c("2005 Total","2006 Total", "2007 Total","2008 Total","2009 Total","2010 Total", 
                              "2011 Total","2012 Total","2013 Total","2014 Total","2015 Total","2016 Total",
                              "2017 Total","2018 Total","2019 Total","2020 Total","2021 Total","2022 Total",
                              "2023 Total"))) %<>%
  select(-fecha)
fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Producto Interno Bruto`,
         `Consumo privado`,
         `Consumo público`,
         `Formación bruta de capital fijo`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(4)
tickformat_y <- ",.0%"


titulo_plotly_usd <- "<b>Oferta y Demanda Globales</b><br>Variación interanual"
plotly_grafico_ar_crec_pib_final_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_crec_pib_final_lineal_y_stacked_usd

saveRDS(plotly_grafico_ar_crec_pib_final_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_crec_pib_final_lineal_y_stacked_usd.rds", compress = TRUE)


#AHORRO E INGRESO NACIONAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/6.cuentas_nacionales/1.agregados_macroeconomicos/2.ingreso_ahorro_nacional/sh_ingreso_ahorro_nac_08_23.xls"

range <- "B7:CY13"
sheet <- "c3_estructura%_PIB"
col_names <-FALSE
from <- "01Dec2006"
to <- "01Dec2022"
by <- "year"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

data %<>%
  pivot_longer(!...1)  %<>%
  pivot_wider(names_from = "...1",
              values_from = "value")  %<>%
  dplyr::select(-name)


# Vector con los años
anios <- c(
  "2006", "2007", "2008", "2009", "2010", "2011", "2012", 
  "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", 
  "2021", "2022"
)

# Vector con los trimestres y Total
trimestres <- c(
  "1º trimestre", "2º trimestre", "3º trimestre", "4º trimestre", "Total"
)

# Crear el vector combinando años con trimestres
fecha <- c()

for (anio in anios) {
  for (trimestre in trimestres) {
    fecha <- c(fecha, paste(anio, trimestre))
  }
}


data <- cbind(fecha, data) 
data <- as_tibble(data)

data %<>%
  dplyr::filter((fecha %in% c("2006 Total", "2007 Total","2008 Total","2009 Total","2010 Total", 
                              "2011 Total","2012 Total","2013 Total","2014 Total","2015 Total","2016 Total",
                              "2017 Total","2018 Total","2019 Total","2020 Total","2021 Total","2022 Total"
  ))) %<>%
  select(-fecha)
fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
colnames(data)
conjunto_datos_usd <- data %>%
  select(fecha, 
         `Ahorro nacional bruto`,
         `Formación bruta de capital`,
         `Ahorro (-) / desahorro (+) del resto del mundo`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0%"

titulo_plotly_usd <- "<b>Ahorro e Ingreso Nacional Bruto</b><br>En % al PIB"
plotly_grafico_ar_fbkf_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_fbkf_lineal_y_stacked_usd

saveRDS(plotly_grafico_ar_fbkf_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_fbkf_lineal_y_stacked_usd.rds", compress = TRUE)

#IGAE ARGENTINA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/6.cuentas_nacionales/4.estimador_mensual_actividad_economica/sh_emae_mensual_base2004.xls"
range <- "C18:H249"
sheet <- "Hoja1"
col_names <-FALSE
from <- "01Jan2005"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

colnames(data) <- c("fecha","Índice Serie Original 2004 = 100","Var % respecto a igual período del año anterior","Índice Serie Desestacionalizada 2004=100","Var % respecto al mes anterior","Índice Serie Tendencia-Ciclo 2004=100","Var % respecto al mes anterior")

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Var % respecto a igual período del año anterior`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(1)
tickformat_y <- ",.0%"

titulo_plotly_usd <- "<b>EMAE</b><br>Variación interanual"
plotly_grafico_ar_igae_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_igae_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_igae_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_igae_lineal_y_stacked_usd.rds", compress = TRUE)

#IPI MINERO GENERAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/10.mineria/2.indice_produccion_minero/serie_ipi_minero.xlsx"
range <- "D21:L96"
sheet <- "Cuadro 1"
col_names <-FALSE
from <- "01Jan2018"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

colnames(data) <- c("fecha",
                    "Nivel General",
                    "Original (Variación Interanual)",
                    "Original (Variación Acumulada)",
                    "Desestacionalizada",
                    "Desestacionalizada (Variación Interanual)",
                    "Tendencia-Ciclo",
                    "Tendencia-Ciclo (Variación Interanual)"
)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Original (Variación Interanual)`,
         `Desestacionalizada (Variación Interanual)`,
         `Tendencia-Ciclo (Variación Interanual)`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0%"

titulo_plotly_usd <- "<b>IPI Minero Nivel General</b><br>Variación interanual<br>Base 2016=100"
plotly_grafico_ar_dos_prod_minerales_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_dos_prod_minerales_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_dos_prod_minerales_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_dos_prod_minerales_lineal_y_stacked_usd.rds", compress = TRUE)

#CAPACIDAD INSTALADA
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/9.industria_manufacturera/1.capacidad_instalada/sh_capacidad_06_24.xls"
range <- "B10:N133"
sheet <- "UCI - NG y bloques"
col_names <-FALSE
from <- "01Jan2016"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

colnames(data) <- c("fecha",
                    "Nivel general",
                    "Productos alimenticios y bebidas",
                    "Productos del tabaco",
                    "Productos textiles",
                    "Papel y cartón",
                    "Edición e impresión",
                    "Refinación del petróleo",
                    "Sustancias y productos químicos",
                    "Productos de caucho y plástico",
                    "Productos minerales no metálicos",
                    "Industrias metálicas básicas",
                    "Industria automotriz",
                    "Metalmecánica excluida industria automotriz"
)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Metalmecánica excluida industria automotriz`,
         `Industrias metálicas básicas`,
         `Industria automotriz`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0%"

titulo_plotly_usd <- "<b>Utilización Capacidad Instalada</b><br>(En porcentaje)"
plotly_grafico_ar_var_indice_indus_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_var_indice_indus_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_var_indice_indus_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_var_indice_indus_lineal_y_stacked_usd.rds", compress = TRUE)


#INDICE DE PRODUCCION INDUSTRIAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/9.industria_manufacturera/3.indice_produccion_industrial/sh_ipi_manufacturero_2024.xls"
range <- "D21:L108"
sheet <- "Cuadro 1"
col_names <-FALSE
from <- "01Jan2017"
to <- "01Apr2024"
by <- "month"
each <- 1

data <- datos_sf(file,sheet,range,col_names)

fecha <- fun_fecha(from,to,by,each)
data <- cbind(fecha, data) 
data <- as_tibble(data)

colnames(data) <- c("fecha",
                    "Nivel General",
                    "Original (Variación Interanual)",
                    "Original (Variación Acumulada)",
                    "Desestacionalizada",
                    "Desestacionalizada (Variación Interanual)",
                    "Tendencia-Ciclo",
                    "Tendencia-Ciclo (Variación Interanual)"
)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `Original (Variación Interanual)`,
         `Desestacionalizada (Variación Interanual)`,
         `Tendencia-Ciclo (Variación Interanual)`)%>%
  mutate(across(where(is.numeric), ~  . / 100))

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0%"

titulo_plotly_usd <- "<b>IPI Manufacturero</b><br>Variación interanual<br>Base 2004=100"
plotly_grafico_ar_var_indice_indus_ventas_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_var_indice_indus_ventas_lineal_y_stacked_usd

saveRDS(plotly_grafico_ar_var_indice_indus_ventas_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_var_indice_indus_ventas_lineal_y_stacked_usd.rds", compress = TRUE)

#INDICE DE PRECIOS AL CONSUMIDOR VARIACION INTERANUAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/indec/3.economia/12.precios/3.indice_precios_consumidor/sh_ipc_06_24.xls"
range <- "A25:CA27"
sheet <- "Var. interanual IPC Nacional"
col_names <-FALSE
from <- "01Dec2017"
to <- "01May2024"
by <- "month"
each <- 1

data <- fun_excel(file, range, sheet, col_names, from, to, by, each)

conjunto_datos_usd <- data 

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0d"

titulo_plotly_usd <- "<b>Índice Precios Consumidor</b><br>Variación interanual"
plotly_grafico_ar_var_indice_consumidor_lineal_y_stacked_usd <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_var_indice_consumidor_lineal_y_stacked_usd
saveRDS(plotly_grafico_ar_var_indice_consumidor_lineal_y_stacked_usd, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_var_indice_consumidor_lineal_y_stacked_usd.rds", compress = TRUE)

#DEFICIT FISCAL
file <- "C:/Users/Mauro/Desktop/bases_de_datos/1.paises/argentina/mecon/1.economia_en_numeros/apendice6.xlsx"
range <- "B484:CR595"
sheet <- "SPN"
col_names <-FALSE
from <- "01Jan2015"
to <- "01Apr2024"
by <- "month"
each <- 1
fecha <- fun_fecha(from,to,by,each)

data <- datos_sf(file, sheet, range, col_names)
data <- cbind(fecha, data) 
data <- as_tibble(data)

colnames(data) <- c(
  
  "fecha",
  "(I) INGRESOS CORRIENTES",
  
  "INGRESOS TRIBUTARIOS",
  
  "APORTES Y CONTRIB. A LA SEG. SOCIAL",
  
  "INGRESOS NO TRIBUTARIOS",
  
  "VENTAS DE BS. Y SERV. DE LAS ADM. PUB.",
  
  "INGRESOS CORRIENTES - INGRESOS DE OPERACION",
  
  "TOTAL RENTAS DE LA PROPIEDAD",
  "INGRESOS CORRIENTES - RENTAS DE LA PROPIEDAD - RENTAS DE LA PROPIEDAD PERCIBIDAS DEL BCRA",
  "INGRESOS CORRIENTES - RENTAS DE LA PROPIEDAD - RENTAS PÚBLICAS PERCIBIDAS POR EL FGS",
  "INGRESOS CORRIENTES - RENTAS DE LA PROPIEDAD - RENTAS PÚBLICAS PERCIBIDAS POR EL FGS Y OTROS",
  "INGRESOS CORRIENTES - RENTAS DE LA PROPIEDAD - OTRAS RENTAS DE LA PROPIEDAD",
  "INGRESOS CORRIENTES - RENTAS DE LA PROPIEDAD - RENTAS DE LA PROPIEDAD NETAS",
  
  "TRANSFERENCIAS CORRIENTES DE INGRESOS",
  "OTROS INGRESOS",
  "INGRESOS CORRIENTES - INGRESOS EXTRAPRESUPUESTARIOS",
  "INGRESOS CORRIENTES - SUPERAVIT OPERATIVO EMPRESAS PUB.",
  
  "(II) GASTOS CORRIENTES",
  
  "REMUNERACIONES",
  "BIENES Y SERVICIOS",
  "GASTOS CORRIENTES - GASTOS DE CONSUMO Y OPERACION - Otros Gastos",
  
  "GASTOS RENTAS DE PROPIEDAD",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses en Moneda Local",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses Deuda Interna",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses en Moneda Extranjera",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses Deuda Externa",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses pagados intra sector público",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Intereses Netos",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Otras Rentas",
  "GASTOS CORRIENTES - INTERESES Y OTRAS RENTAS DE LA PROP. - Pagadas sobre Deuda",
  
  "PRESTACIONES DE LA SEGURIDAD SOCIAL",
  
  "OTROS GASTOS CORRIENTES",
  
  "TRANSFERENCIAS CORRIENTES DE GASTOS - Total al Sector Privado",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Privado - Instituciones de Enseñanza",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Privado - Otras",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Total Provincias y CABA",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Recursos Coparticipados",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Garantía Acuerdo Provincias",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Leyes Especiales",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Resto",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Universidades",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Público - Otras",
  "GASTOS CORRIENTES - TRANSFERENCIAS CORRIENTES DE GASTOS - Al Sector Externo",
  
  "GASTOS CORRIENTES - OTROS GASTOS",
  
  "GASTOS CORRIENTES - GASTOS EXTRAPRESUPUESTARIOS",
  
  "GASTOS CORRIENTES - DEFICIT OPERATIVO EMPRESAS PUB.",
  
  "(III) RESULTADO ECONOMICO: AHORRO/DESAHORRO",
  
  "RECURSOS DE CAPITAL",
  "RECURSOS DE CAPITAL - Privatizaciones",
  "RECURSOS DE CAPITAL - Otros",
  
  "GASTOS DE CAPITAL",
  "GASTOS DE CAPITAL - INVERSION REAL DIRECTA",
  "GASTOS DE CAPITAL - TRANSFERENCIAS DE CAPITAL - Total Provincias y CABA",
  "GASTOS DE CAPITAL - TRANSFERENCIAS DE CAPITAL - Leyes Especiales",
  "GASTOS DE CAPITAL - TRANSFERENCIAS DE CAPITAL - Resto",
  "GASTOS DE CAPITAL - TRANSFERENCIAS DE CAPITAL - Otras",
  "GASTOS DE CAPITAL - INVERSION FINANCIERA - A Provincias y CABA",
  "GASTOS DE CAPITAL - INVERSION FINANCIERA - Resto",
  
  "INGRESOS ANTES DE FIGURATIVOS",
  "INGRESOS PRIMARIOS ANTES DE FIGURATIVOS",
  "GASTOS ANTES DE FIGURATIVOS",
  "GASTOS PRIMARIOS ANTES DE FIGURATIVOS",
  "RESULTADO FINANCIERO ANTES DE FIGURATIVOS",
  "RESULTADO PRIMARIO SIN RENTAS",
  
  "CONTRIBUCIONES FIGURATIVAS - Del Tesoro Nacional",
  "CONTRIBUCIONES FIGURATIVAS - De Recursos Afectados",
  "CONTRIBUCIONES FIGURATIVAS - De Organismos Descentralizados",
  "CONTRIBUCIONES FIGURATIVAS - De Instituciones de Seguridad Social",
  "CONTRIBUCIONES FIGURATIVAS - De ExCajas Provinciales",
  "CONTRIBUCIONES FIGURATIVAS - De Empresas Públicas y Otros",
  "CONTRIBUCIONES FIGURATIVAS - De PAMI, Fdos. Fiduciarios y Otros",
  
  "GASTOS FIGURATIVOS",
  "INGRESOS DESPUES DE FIGURATIVOS",
  "INGRESOS PRIMARIOS DESPUES DE FIGURATIVOS",
  "GASTOS DESPUES DE FIGURATIVOS",
  "GASTOS PRIMARIOS DESPUES DE FIGURATIVOS",
  "SUPERAVIT PRIMARIO",
  "SUPERAVIT PRIMARIO -sin Privatizaciones-",
  "RESULTADO FINANCIERO",
  "RESULTADO FINANCIERO -sin privatizaciones-",
  
  "FUENTES FINANCIERAS - DISMINUCION DE LA INVERSIÓN FINANCIERA",
  "FUENTES FINANCIERAS - ENDEUDAMIENTO PÚBLICO E INCREMENTO DE OTROS PASIVOS - Endeudamiento en Moneda Local",
  "FUENTES FINANCIERAS - ENDEUDAMIENTO PÚBLICO E INCREMENTO DE OTROS PASIVOS - Endeudamiento Interno",
  "FUENTES FINANCIERAS - ENDEUDAMIENTO PÚBLICO E INCREMENTO DE OTROS PASIVOS - Endeudamiento en Moneda Extranjera",
  "FUENTES FINANCIERAS - ENDEUDAMIENTO PÚBLICO E INCREMENTO DE OTROS PASIVOS - Endeudamiento Externo",
  "FUENTES FINANCIERAS - ENDEUDAMIENTO PÚBLICO E INCREMENTO DE OTROS PASIVOS - Incremento Otros Pasivos",
  "FUENTES FINANCIERAS - INCREMENTO DEL PATRIMONIO",
  "FUENTES FINANCIERAS - CONTRIBUCIONES FIGURATIVAS PARA APLICACIONES FINANCIERAS",
  
  "APLICACIONES FINANCIERAS - INVERSIÓN FINANCIERA",
  "APLICACIONES FINANCIERAS - AMORT.DEUDAS Y DISM. OTROS PASIVOS - Amortización en Moneda Local",
  "APLICACIONES FINANCIERAS - AMORT.DEUDAS Y DISM. OTROS PASIVOS - Amortización Deuda Interna",
  "APLICACIONES FINANCIERAS - AMORT.DEUDAS Y DISM. OTROS PASIVOS - Amortización en Moneda Extranjera",
  "APLICACIONES FINANCIERAS - AMORT.DEUDAS Y DISM. OTROS PASIVOS - Amortización Deuda Externa",
  "APLICACIONES FINANCIERAS - AMORT.DEUDAS Y DISM. OTROS PASIVOS - Disminución Otros Pasivos",
  "APLICACIONES FINANCIERAS - DISMINUCION DEL PATRIMONIO",
  "APLICACIONES FINANCIERAS - GASTOS FIGURAT. PARA APLIC. FINANC."
)



data["INGRESOS TOTALES"] <- rowSums(data[, c(2, 48)], na.rm = TRUE)
data["GASTOS TOTALES"] <- rowSums(data[, c(18, 51)], na.rm = TRUE)

conjunto_datos_usd <- data %>%
  select(fecha, 
         `INGRESOS TOTALES`,
         `GASTOS TOTALES`,
         `RESULTADO FINANCIERO`)

mi_paleta_plotly <-  pal_plotly(3)
tickformat_y <- ",.0d"

titulo_plotly_usd <- "<b>Sector Público no Financiero</b><br>(Miles de $.)"
plotly_grafico_ar_deficit_spnf_lineal_y_stacked_bs <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_deficit_spnf_lineal_y_stacked_bs
saveRDS(plotly_grafico_ar_deficit_spnf_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_deficit_spnf_lineal_y_stacked_bs.rds", compress = TRUE)


#INGRESOS PUBLICOS

conjunto_datos_usd <- data %>%
  select(fecha, 
         `INGRESOS TRIBUTARIOS`,
         `APORTES Y CONTRIB. A LA SEG. SOCIAL`,
         `INGRESOS NO TRIBUTARIOS`,
         `VENTAS DE BS. Y SERV. DE LAS ADM. PUB.`,
         `TOTAL RENTAS DE LA PROPIEDAD`,
         `TRANSFERENCIAS CORRIENTES DE INGRESOS`,
         `OTROS INGRESOS`,
         `RECURSOS DE CAPITAL`)

mi_paleta_plotly <-  pal_plotly(8)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"

titulo_plotly_usd <- "<b>Ingresos del Sector Público no Financiero</b><br>(Miles de $.)"

plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_ingre_spnf_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_ingre_spnf_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_ingre_spnf_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_ingre_spnf_lineal_y_stacked_bs.rds", compress = TRUE)


#GASTO PUBLICO

conjunto_datos_usd <- data %>%
  select(fecha, 
         `REMUNERACIONES`,
         `BIENES Y SERVICIOS`,
         `GASTOS RENTAS DE PROPIEDAD`,
         `PRESTACIONES DE LA SEGURIDAD SOCIAL`,
         `TOTAL RENTAS DE LA PROPIEDAD`,
         `TRANSFERENCIAS CORRIENTES DE GASTOS - Total al Sector Privado`,
         `GASTOS DE CAPITAL`)

mi_paleta_plotly <-  pal_plotly(7)
tickformat_y <- ",.0d"
graph_type <-  "bar"
barmode <-  "stack"


titulo_plotly_usd <- "<b>Gastos del Sector Público no Financiero</b><br>(Miles de $.)"

plotly_lineal <- plotly_usd(conjunto_datos_usd, mi_paleta_plotly, titulo_plotly_usd, tickformat_y)
plotly_grafico_ar_egre_spnf_lineal_y_stacked_bs <- generar_layout_menus(plotly_lineal, graph_type, barmode)
plotly_grafico_ar_egre_spnf_lineal_y_stacked_bs

saveRDS(plotly_grafico_ar_egre_spnf_lineal_y_stacked_bs, 
        file = "C:/Users/Mauro/Desktop/proyectos_hugo/hugo-js-bermau/static/cached_plots/1.paises/argentina/plotly_grafico_ar_egre_spnf_lineal_y_stacked_bs.rds", compress = TRUE)


