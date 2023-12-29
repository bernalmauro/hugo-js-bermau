---
title: "Gráficos"
output: html_document
---

<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<script src="/rmarkdown-libs/jquery/jquery.min.js"></script>
<link href="/rmarkdown-libs/dygraphs/dygraph.css" rel="stylesheet" />
<script src="/rmarkdown-libs/dygraphs/dygraph-combined.js"></script>
<script src="/rmarkdown-libs/dygraphs/shapes.js"></script>
<script src="/rmarkdown-libs/moment/moment.js"></script>
<script src="/rmarkdown-libs/moment-timezone/moment-timezone-with-data.js"></script>
<script src="/rmarkdown-libs/moment-fquarter/moment-fquarter.min.js"></script>
<script src="/rmarkdown-libs/dygraphs-binding/dygraphs.js"></script>
<script src="/rmarkdown-libs/Dygraph.Plugins.Crosshair/crosshair.js"></script>
<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<script src="/rmarkdown-libs/jquery/jquery.min.js"></script>
<link href="/rmarkdown-libs/dygraphs/dygraph.css" rel="stylesheet" />
<script src="/rmarkdown-libs/dygraphs/dygraph-combined.js"></script>
<script src="/rmarkdown-libs/dygraphs/shapes.js"></script>
<script src="/rmarkdown-libs/moment/moment.js"></script>
<script src="/rmarkdown-libs/moment-timezone/moment-timezone-with-data.js"></script>
<script src="/rmarkdown-libs/moment-fquarter/moment-fquarter.min.js"></script>
<script src="/rmarkdown-libs/dygraphs-binding/dygraphs.js"></script>
<script src="/rmarkdown-libs/Dygraph.Plugins.Crosshair/crosshair.js"></script>
<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<script src="/rmarkdown-libs/jquery/jquery.min.js"></script>
<link href="/rmarkdown-libs/dygraphs/dygraph.css" rel="stylesheet" />
<script src="/rmarkdown-libs/dygraphs/dygraph-combined.js"></script>
<script src="/rmarkdown-libs/dygraphs/shapes.js"></script>
<script src="/rmarkdown-libs/moment/moment.js"></script>
<script src="/rmarkdown-libs/moment-timezone/moment-timezone-with-data.js"></script>
<script src="/rmarkdown-libs/moment-fquarter/moment-fquarter.min.js"></script>
<script src="/rmarkdown-libs/dygraphs-binding/dygraphs.js"></script>
<script src="/rmarkdown-libs/Dygraph.Plugins.Crosshair/crosshair.js"></script>

### Estadísticas Mundiales

<!-- Barra de navegación horizontal -->
<ul class="nav nav-tabs" id="nav-tab" role="tablist">
<li class="nav-item">
<a class="nav-link active" id="pibmundial-tab" data-toggle="tab" href="#pibmundial" role="tab" aria-controls="pibmundial" aria-selected="true">PIB Mundial</a>
</li>
<li class="nav-item">
<a class="nav-link" id="pobrezamundial-tab" data-toggle="tab" href="#pobrezamundial" role="tab" aria-controls="pobrezamundial" aria-selected="false">Pobreza Mundial</a>
</li>
<li class="nav-item">
<a class="nav-link" id="esperanzavidamundial-tab" data-toggle="tab" href="#esperanzavidamundial" role="tab" aria-controls="esperanzavidamundial" aria-selected="false">Esperanza de Vida Mundial</a>
</li>
</ul>
<!-- Contenido de las pestañas -->

<div id="nav-tabContent" class="tab-content">

<div id="pibmundial" class="tab-pane fade show active" role="tabpanel" aria-labelledby="pibmundial-tab">

<div class="dygraphs html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-1" style="width:500px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true,"drawGrid":false},"y":{"drawAxis":true,"axisLabelFormatter":"function(d){return \"$\" + d + \" billones\"}","valueFormatter":"function(d){return Math.round(d).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");}","drawGrid":false}},"series":{"Producto Interno Bruto Mundial":{"axis":"y"}},"title":"PIB Mundial de los Últimos dos Milenios","labels":["Year","Producto Interno Bruto Mundial"],"retainDateWindow":false,"colors":["green","green"],"legend":"onmouseover","labelsDivWidth":300,"labelsShowZeroValues":true,"labelsSeparateLines":false,"stackedGraph":true,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"#6b7785","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false,"showRangeSelector":true,"rangeSelectorHeight":30,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel","highlightCircleSize":2,"highlightSeriesBackgroundAlpha":1,"highlightSeriesOpts":[],"hideOverlayOnMouseOut":true},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1,1000,1500,1600,1700,1820,1870,1900,1913,1940,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015],[0.18274103474165,0.21014473481495,0.43052728115293,0.57446865728769,0.6433229243199,1.20236083306954,1.92391732484512,3.41875377452597,4.73867575900573,7.80636856199303,9.25106328451166,9.79563062060308,10.2486951600302,10.7648504702851,11.132816333836,11.8424364378983,12.3993151219929,12.8712015668878,13.2845118842174,13.8933432617901,14.6204308258341,15.1275450029549,15.840374754695,16.5288236949554,17.7274266711053,18.6556197235743,19.6727780214873,20.4048070770476,21.5275961525308,22.7154807852326,23.8667404645702,24.8559369387911,26.0382259438034,27.7663175448469,28.4127369793204,28.8460525283362,30.2531757268988,31.4799138733314,32.8640333749772,34.0390549220292,34.7270633687229,35.4077652872047,35.7991472519796,36.8173468375886,38.4967218991355,39.8236108285131,41.2319711010656,42.8128962218747,44.6496493242313,46.0768423041231,47.0438,47.6509,48.4851,49.4155,50.8866,52.5766,54.6052,56.7721,58.1595,60.2475,63.1009,64.6392,66.4209,68.8949,72.6182,76.0892,80.2026,84.5765,87.0207,86.7501,91.3297,94.9824,98.0323,101.27,104.72,108.12]],"fixedtz":false,"tzone":"","plugins":{"Crosshair":{"direction":"both"}}},"evals":["attrs.axes.y.axisLabelFormatter","attrs.axes.y.valueFormatter","attrs.interactionModel"],"jsHooks":[]}</script>

</div>

<div id="pobrezamundial" class="tab-pane fade show active" role="tabpanel" aria-labelledby="pobrezamundial-tab">

<div class="dygraphs html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-2" style="width:500px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true,"drawGrid":false},"y":{"drawAxis":true,"axisLabelFormatter":"function(d){return Math.round(d).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");}","valueFormatter":"function(d){return Math.round(d).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");}","drawGrid":false}},"series":{"Paises de Altos Ingresos":{"axis":"y"},"América Latina y el Caribe":{"axis":"y"},"Asia Oriental y el Pacífico":{"axis":"y"},"Asia del Sur":{"axis":"y"},"Oriente Medio y África del Norte":{"axis":"y"},"Europa y Asia Central":{"axis":"y"},"Africa Sub-Sahariana":{"axis":"y"}},"title":"Población total que vive en pobreza extrema por región del mundo (millones de habitantes)","labels":["Year","Paises de Altos Ingresos","América Latina y el Caribe","Asia Oriental y el Pacífico","Asia del Sur","Oriente Medio y África del Norte","Europa y Asia Central","Africa Sub-Sahariana"],"retainDateWindow":false,"colors":["#6D3E91","#C05917","#58AC8C","#286BBB","#883039","#BC8E5A","#00295B"],"legend":"onmouseover","labelsDivWidth":300,"labelsShowZeroValues":true,"labelsDiv":false,"labelsSeparateLines":true,"stackedGraph":true,"fillGraph":true,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"#6b7785","axisLineWidth":1.5,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false,"showRangeSelector":true,"rangeSelectorHeight":30,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel","highlightCircleSize":2,"highlightSeriesBackgroundAlpha":1,"highlightSeriesOpts":[],"hideOverlayOnMouseOut":true},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019],[4.08725,4.099177,4.345438,4.688252,4.535552,4.557491,5.004746,5.085103,5.202375,4.945483,4.713282,4.738842,4.852098,5.659049,5.504494,5.610036,5.197648,5.131927,5.303856,5.401669,5.36818,5.974581,6.170179,6.780861,7.290625,7.927579,6.891425,7.683151,6.683155,6.684494],[73.159629,70.21205,71.894971,72.11797,67.734862,68.975115,78.59989,75.71901,72.752688,74.610652,69.829625,69.316873,66.022015,65.237721,60.181526,57.659473,47.589109,45.957291,43.204797,41.162933,37.724349,35.307145,30.306586,27.414889,26.117037,25.843037,27.491608,27.589494,27.265804,27.757316],[1055.536103,1040.573331,1011.052199,974.90417,910.983635,859.153925,790.875756,795.810447,822.503631,769.745943,717.859327,682.537976,614.970645,558.440632,493.489345,409.468673,401.951267,360.805044,341.670254,303.471366,261.804771,205.333227,176.131012,90.48309,72.625708,55.25059,46.227607,39.652165,31.957065,23.641492],[562.991779,567.348151,559.211438,557.983169,560.066553,548.302133,542.033352,549.88815,556.649232,557.568162,564.717519,568.381961,575.662963,568.734692,551.229481,532.375132,519.175845,499.220886,484.673963,474.584083,429.696971,356.937921,332.378235,323.120588,310.562257,292.373936,279.393977,225.68009,181.733329,156.282205],[13.999313,16.046838,13.381707,12.518834,12.180331,13.466237,12.653965,12.324905,11.291096,11.029038,10.126988,9.996771,9.409482,9.53237,8.826162,8.250387,8.095222,7.854492,7.655841,7.285993,6.095625,7.142047,7.154814,7.77441,9.266548,17.805436,20.383538,22.768211,29.077675,32.959255],[14.964693,17.59385,24.950602,29.889086,38.630008,38.761495,38.915802,36.512324,36.014732,46.242157,42.995172,39.172148,34.758446,33.952185,29.484423,29.333894,26.070454,23.387057,20.715958,20.163002,19.949392,18.557102,17.925609,16.382583,17.170333,15.572137,14.054284,13.960518,11.976724,11.766863],[271.488835,289.166731,305.251907,320.992736,333.270456,338.150595,341.568086,348.574951,356.615882,364.874826,371.136657,375.693272,378.611221,380.595514,372.162744,369.58965,369.05456,368.776837,366.223793,372.081833,366.094403,365.48818,369.460174,369.558939,368.74061,378.271517,383.804597,385.303897,384.8485,389.003507]],"fixedtz":false,"tzone":"","plugins":{"Crosshair":{"direction":"both"}}},"evals":["attrs.axes.y.axisLabelFormatter","attrs.axes.y.valueFormatter","attrs.interactionModel"],"jsHooks":[]}</script>

</div>

<div id="esperanzavidamundial" class="tab-pane fade show active" role="tabpanel" aria-labelledby="esperanzavidamundial-tab">

<div class="dygraphs html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-3" style="width:500px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true,"drawGrid":false},"y":{"drawAxis":true,"axisLabelFormatter":"function(d){return Math.round(d).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");}","valueFormatter":"function(d){return Math.round(d).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");}","drawGrid":false}},"series":{"Oceania":{"axis":"y"},"Europa":{"axis":"y"},"America":{"axis":"y"},"Asia":{"axis":"y"},"Mundo":{"axis":"y"},"Africa":{"axis":"y"}},"title":"Esperanza de Vida<br><small>1770-2021<\/small>","labels":["Year","Oceania","Europa","America","Asia","Mundo","Africa"],"retainDateWindow":false,"colors":["#18470f","#6d3e91","#2c8465","#be5915","#cf0a66","#c15065"],"legend":"onmouseover","labelsDivWidth":300,"labelsShowZeroValues":false,"labelsDiv":false,"labelsSeparateLines":true,"stackedGraph":false,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"#6b7785","axisLineWidth":1.5,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false,"showRangeSelector":true,"rangeSelectorHeight":30,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel","highlightCircleSize":2,"highlightSeriesBackgroundAlpha":1,"highlightSeriesOpts":[],"hideOverlayOnMouseOut":true},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[1770,1800,1820,1830,1850,1870,1885,1900,1913,1925,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021],[null,null,null,null,null,34.7,null,47.6,51,null,61.4,60.6,62,62.7,63,63.4,63.6,64,64.6,64.5,65.1,65.4,65.4,65.7,65.5,65.8,65.8,66.3,66.2,66.7,66.6,67.4,67.8,68,68.1,68.8,68.9,69.4,69.8,70.3,70.5,70.9,70.9,71.5,71.7,71.6,72.1,72.2,72.5,72.5,73.2,73.6,73.6,74,74,74.3,74.3,74.6,74.7,75.1,75.4,75.6,75.5,75.8,76,76.4,76.5,76.5,76.8,77,77.4,77.5,77.7,78,78,78.2,78.4,78.5,78.8,78.7,79.5,79.4],[34.3,33.3,35.6,null,36.3,36.2,null,42.7,46.8,null,62.8,62.8,64,64.7,65.5,66,66.9,66.9,68.2,68.1,68.8,69.1,68.9,69.2,69.9,69.8,70,70,69.9,69.6,70,70.1,70.3,70.4,70.6,70.5,70.6,70.9,70.9,71,70.9,71.2,71.5,71.5,71.6,71.7,72.5,72.7,72.8,72.9,72.9,72.9,72.7,72.1,72.1,72.2,72.7,73.2,73.6,73.4,73.5,73.8,73.8,73.8,74.4,74.5,75.2,75.6,75.8,76.3,76.5,77.1,77.3,77.6,77.9,78,78.4,78.7,78.8,79.1,77.7,77],[34.8,null,null,34.8,35.1,35.1,null,41,45.1,null,58.12,58.37,58.83,59.25,59.96,60.29,60.56,60.75,61.18,61.55,61.83,62.24,62.42,62.5,62.86,63.14,63.26,63.57,63.6,63.87,63.97,64.62,64.81,65.14,65.65,66.11,66.32,66.8,67.08,67.39,67.56,68,68.31,68.69,69,69.16,69.54,69.87,70.1,70.44,70.73,70.95,71.3,71.41,71.68,71.94,72.3,72.59,72.85,73.09,73.31,73.57,73.82,73.95,74.34,74.59,74.83,74.96,75.19,75.46,75.34,75.82,75.98,76.16,76.32,76.29,76.28,76.37,76.51,76.71,74.85,74.2],[27.5,null,null,null,null,null,27.5,28,28.1,null,42,43,44.3,44.9,45.8,46.3,46.8,47.3,47.8,44.3,41.7,45.3,49.6,50.3,50.9,50.6,51.5,52.1,53,53.6,53.9,53.4,55.3,55.9,56.4,56.7,57.2,58,58.5,59.1,59.6,60.1,60.6,61.1,61.5,62,62.5,62.9,63.3,63.7,64,64.2,64.8,65.1,65.4,65.7,66,66.4,66.9,67.2,67.6,68.1,68.5,68.9,69.2,69.6,70.1,70.3,70.5,71,71.3,71.7,72,72.4,72.8,73.1,73.4,73.7,74,74.2,73.7,72.5],[28.5,28.5,29,null,29.3,29.7,null,32,34.1,null,46.5,47.1,48.2,48.8,49.6,50.1,50.6,50.9,51.5,49.3,47.7,50.4,53.1,53.6,54.2,53.9,54.5,54.9,55.5,55.8,56.1,55.9,57.1,57.6,58,58.3,58.7,59.4,59.7,60.2,60.6,61,61.4,61.6,61.9,62.2,62.8,63.2,63.3,63.8,64,64.1,64.3,64.4,64.5,64.9,65.1,65.5,65.7,66.1,66.5,66.8,67.1,67.5,67.8,68.2,68.7,69.1,69.3,69.8,70.1,70.5,70.9,71.2,71.6,71.8,72.1,72.3,72.6,72.8,72,71],[26.4,null,null,null,null,null,null,null,null,26.4,37.6,37.9,38.4,38.9,39.3,39.8,40.2,40,40.3,41.3,41.5,41.9,42.3,42.8,43.2,43.4,43.4,43.6,44.1,44.3,44.8,45.3,45.4,46.2,46.5,46.9,47.6,48.2,48.6,49.1,49.5,49.9,50.3,49.6,49.7,50.1,50.6,50.9,50.4,51.7,51.6,51.5,51.2,51.5,50.5,52.1,52.1,52.3,51.9,52.8,53.3,53.6,54,54.4,54.9,55.5,56.1,56.7,57.3,58,58.6,59.3,59.8,60.3,60.7,61.1,61.6,62,62.3,62.7,62.2,61.7]],"fixedtz":false,"tzone":"","plugins":{"Crosshair":{"direction":"both"}}},"evals":["attrs.axes.y.axisLabelFormatter","attrs.axes.y.valueFormatter","attrs.interactionModel"],"jsHooks":[]}</script>

</div>

</div>

<!-- JavaScript para cargar el contenido dinámicamente y manejar los gráficos Dygraph -->
<script>
  $(document).ready(function() {
    // Manejar el cambio de pestaña
    $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
      // Obtener el identificador de la pestaña desde el enlace del menú
      var tabId = $(this).attr('href');
      &#10;      // Mostrar la pestaña correspondiente
      $('.nav-link').removeClass('active');
      $(this).addClass('active');
      $('.tab-pane').removeClass('show active');
      $(tabId).addClass('show active');
&#10;      // Inicializar Dygraphs en la nueva pestaña activa
      initDygraphs(tabId);
    });
&#10;    // Función para inicializar Dygraphs
    function initDygraphs(tabId) {
      // Obtener el contenedor de Dygraphs en la pestaña activa
      var dygraphContainers = $(tabId).find('.dygraph-container');
&#10;      // Iterar sobre cada contenedor y verificar si Dygraphs ya está inicializado
      dygraphContainers.each(function() {
        var dygraphContainer = $(this);
        if (!dygraphContainer.hasClass('initialized')) {
          // Inicializar Dygraphs aquí (puedes llamar a la función de inicialización)
          // Por ejemplo: dygraphContainer.dygraph(options);
          &#10;          // Marcar como inicializado
          dygraphContainer.addClass('initialized');
        }
      });
    }
  });
</script>
