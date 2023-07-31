function getUrlQueryParams()
{
    var queryParams = {}, param;
    //http://example.com/?id=123456&name=amy
    //window.location.search="?id=123456&name=amy"
    var params = window.location.search.substring(1).split("&");
    for(var i = 0; i < params.length; i++)
    {
        param = params[i].split('=');
        queryParams[param[0]]=param[1];
    }
    return params;
}
 
var queryParams = getUrlQueryParams();
console.log(queryParams);

<script>

function getQVU(variable) {
  var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        if (pair[0] == variable) {
            return pair[1];
        }
    }
    return false;
}


var a = window.location.href;

var arr1 = a.split('/');
var b = arr1[5];

var b1 = b.split('?');
var b2 = b1[0];

if(b2=='dashboard.html') {
  var v1 = getQVU('theme'); if(v1>0) { v1="theme="+v1+"&"; } else { v1=""; }
  var v2 = getQVU('indicator_id'); if(v2>0) { v2="&indicator_id="+v2; } else { v2=""; }
  var v3 = getQVU('area_id'); if(v3>0) { v3="&area_id="+v3; } else { v3=""; }
  var v4 = getQVU('temaIndicadores'); if(v4>0) { v4="&temaIndicadores="+v4; } else { v4=""; } // Variable de compatibilidad
  c="dashboard.html?"+v1+"lang=en"+v2+v3+v4;
} else if(b2=='perfil-regional.html') {
  var v1 = getQVU('theme'); if(v1>0) { v1="theme="+v1; } else { v1=""; }
  c="regional-profile.html?"+v1+"&lang=en";
} else if(b2=='perfil-nacional.html') {
  var v1 = getQVU('theme'); if(v1>0) { v1="theme="+v1; } else { v1=""; }
  var v2 = getQVU('country'); if(v2!="") { v2="&country="+v2; } else { v2=""; }
  c="national-profile.html?"+v1+v2+"&lang=en";
} else if(b2=='buscador-indicadores.html') {
  var v1 = getQVU('buscar'); if(v1!="") { v1="buscar="+v1; } else { v1=""; }
  c="buscador-indicadores.html?"+v1+"&lang=en";
} else if(b2=='technical-sheet.html') {
  var v1 = getQVU('indicator_id'); if(v1!="") { v1="indicator_id="+v1; } else { v1=""; }
  c="technical-sheet.html?"+v1+"&lang=en";
} else if(b2=='open-data.html') {
  c="open-data.html?lang=en";
} else if(b2=='publicaciones.html') {
  c="publicaciones.html?lang=en";
} else if(b2=='acerca.html') {
  c="acerca.html?lang=en";
} else if(b2=='indicator-search.html') {
  var v1 = getQVU('type'); if(v1==2) { v1="type=2&"; } else { v1=""; }
  var v2 = getQVU('q');
  c="indicator-search.html?"+v1+"q="+v2+"&lang=en";
} else { c="index.html?lang=en"; }

document.getElementById("link_lang").innerHTML = "<a href='"+ c +"' >English</a>";
document.getElementById("link_lang_2").innerHTML = "<a href='"+ c +"' >English</a>";

</script>



<script type="text/javascript">

function getQueryVariable(variable) {
    // Estoy asumiendo que query es window.location.search.substring(1);
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        if (pair[0] == variable) {
            return pair[1];
        }
    }
    return false;
}


$(document).ready(function () {

      var country = getQueryVariable('country');
      var langData = getQueryVariable('lang');
      var get_tema = getQueryVariable('tema');
      if(get_tema>0) { } else  { var get_tema = getQueryVariable('theme'); };
      if(langData=="") { var langData="es"; } else { }

      if(langData=="es") {
        var nombre_tema_1="perfil nacional social-demográfico";
        var nombre_tema_2="perfil nacional económico";
        var nombre_tema_3="perfil nacional ambiental";
        var nombre_tema_4="perfil nacional energético";
      } else {
        var nombre_tema_1="national socio-demographic profile";
        var nombre_tema_2="national economic profile";
        var nombre_tema_3="national environmental profile";
        var nombre_tema_4="national energy profile";
      }



      if(get_tema==1) {
        $("#tipo_tema").html(nombre_tema_1);
        var nombre_tema = "Social-Demográfico";
        $("#perfil_titulo").attr('class','perfil-titulo perfil-titulo-color-1');
        $("#select_1").attr('class','select_1');
      } else if(get_tema==2) {
        $("#tipo_tema").html(nombre_tema_2);
        var nombre_tema = "Económico";
        $("#perfil_titulo").attr('class','perfil-titulo perfil-titulo-color-2');
        $("#select_1").attr('class','select_2');
      } else if(get_tema==3) {
        $("#tipo_tema").html(nombre_tema_3);
        var nombre_tema = "Ambiental";
        $("#perfil_titulo").attr('class','perfil-titulo perfil-titulo-color-3');
        $("#select_1").attr('class','select_3');
      } else if(get_tema==4) {
        $("#tipo_tema").html(nombre_tema_4);
        var nombre_tema = "Energético";
        $("#perfil_titulo").attr('class','perfil-titulo perfil-titulo-color-4');
        $("#select_1").attr('class','select_4');
      }


      var country = country.toLowerCase();
      $("#bandera").attr("src","images/flags/"+country+".png");






      /*********************************************************/
      /******************SECCION AGREGADA ANITA*******************/
                 var energeticList;
                 var indexEList;
                 var firstESection;
                 if(get_tema==4){
                      $.getJSON("../../charts/cepalstat/energy/json_country_energetic.json", function(data) {
                          energeticList=data;
                          var nc=compareList(energeticList, country);
                          firstESection= energeticList[nc].sect1;
                      });

                 }
                  function compareList(arr1, isoText){
                     var countryMatch=arr1.map(function(item){return item.iso3}).indexOf(isoText.toUpperCase());
                      return countryMatch;
                  }
       /******************FIN AGREGADO ANITA*******************/
      /*********************************************************/



      setTimeout(function(){


      $.getJSON("json_country_id.json?v=20220802", function(data) {
        var country = getQueryVariable('country');
        $.each(data, function(i, record) {
            if(langData=="es") {
                var dato_1 = record.name_es;
            } else {
              var dato_1 = record.name_en;
            }

          var dato_0 = record.iso3;
          var dato_0 = dato_0.toLowerCase();
          if(country==dato_0) {
            $("#titulo_nombre_pais").html(dato_1+": ");


            if(langData=="en") {
              if(get_tema==1) { var title_nombre_tema = "National Socio-Demographic Profile";
              } else if(get_tema==2) { var title_nombre_tema = "National Economic Profile";
              } else if(get_tema==3) { var title_nombre_tema = "National Environmental Profile";
              } else if(get_tema==4) { var title_nombre_tema = "National Energy Profile"; }
              $("title").html(title_nombre_tema+": "+dato_1+" - CEPALSTAT Statistic Database and Publications");
            } else {
              if(get_tema==1) { var title_nombre_tema = "Perfil nacional social-demográfico";
              } else if(get_tema==2) { var title_nombre_tema = "Perfil nacional económico";
              } else if(get_tema==3) { var title_nombre_tema = "Perfil nacional ambiental";
              } else if(get_tema==4) { var title_nombre_tema = "Perfil nacional energético"; }
              $("title").html(title_nombre_tema+": "+dato_1+" - CEPALSTAT Bases de Datos y Publicaciones Estadísticas");
            }

            var selected = "selected";
          } else {
            var selected = "";
          };


          /*********************************************************/
          /******************SECCION AGREGADA ANITA*******************/
             if (get_tema==4){
                 indexEList=compareList(energeticList, dato_0);
                if (indexEList != -1){
                    $("#select_1").append('<option value=\"'+dato_0+'\" '+selected+' >'+dato_1+'</option>');
                }
             }else{
             /******************FIN AGREGADO ANITA*******************/
              /*********************************************************/


                $("#select_1").append('<option value=\"'+dato_0+'\" '+selected+' >'+dato_1+'</option>');
              } //*****************************************LINEA AGREGADA ANITA



        });
      });





      $('#select_1').on('change', function () {
          var url = $(this).val(); // get selected value
          var get_tema = getQueryVariable('tema');
          if(get_tema>0) { } else  { var get_tema = getQueryVariable('theme'); };
          var get_lang = getQueryVariable('lang');
          if (url) { // require a URL
            if(url==0) {} else {
              if(get_lang=="en") {
                window.location = 'national-profile.html?theme='+get_tema+'&country='+url+'&lang=en';
              } else {
                window.location = 'perfil-nacional.html?theme='+get_tema+'&country='+url+'&lang=es';
              }

            }
          }
          return false;
      });


    }, 200);



      var array_charts = new Array();
      $.getJSON("json_national_chart_links.json", function(data) {
        $.each(data, function(i, record) {
          var var_1 = record.code;
          var var_2 = record.chart;
          var var_3 = record.height;
          var var_4 = record.column;
          var var_5 = record.oneChart;
          array_charts.push({code:var_1,chart:var_2,height:var_3,column:var_4,oneChart:var_5});
        });
      });


      setTimeout(function(){

      $.getJSON("json_temas_perfiles_nacionales.json", function(data) {
        var get_tema = getQueryVariable('tema');
        if(get_tema>0) { } else  { var get_tema = getQueryVariable('theme'); };
        var get_lang = getQueryVariable('lang');
        var data = data.filter(function(x) {
           return x.theme == get_tema
        });

        var chart_filter=new Array();

        /*********************************************************/
        /******************SECCION AGREGADA ANITA*******************/
            var showSect1=1;
              if(get_tema==4){
                        showSect1=firstESection;
                }
           /*********************************************************/
        /******************FIN ANITA*******************/


        $.each(data, function(i, record) {
          if(i!=0 || (i==0 && showSect1==1)){ //*********************LINEA AGREGADA ANITA
              if(get_lang=="en") {
                var name_es = record.name_en;
                var langData = "en";
              } else {
                var name_es = record.name_es;
                var langData = "es";
              }

          var code = record.code;

          //console.log(dato_1);
          var charts_theme = array_charts;
          var chart_filter = charts_theme.filter(function(x) {
             return x.code == code
          });
          console.log(chart_filter);

          $("#graficos_temas").append("<div class=\"perfil-subtitulo perfil-color"+get_tema+"\">"+name_es+"</div>");

          $("#graficos_temas").append("<div class=\"row\">");

          var row_chart = "";
          var numero=1;
          $.each(chart_filter, function(ii, record_chart) {
            var chart_link = record_chart.chart;
            var chart_height = record_chart.height;
            var chart_columns = record_chart.column;
            var chart_one = record_chart.oneChart;

            //Dependiendo del url poner o no el signo ? &
            var n = chart_link.indexOf("?");
            if(n<0) { var link_parameter="?" } else { var link_parameter="&" };


            if(chart_columns==1) {

              row_chart+="<div class=\"row\"><div class=\"col-lg-12\"><div class=\"box-country\"><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div></div>";

            } else if(chart_columns==2) {

              if(numero==1) {
                row_chart+="<div class=\"row\"><div class=\"col-lg-6\"><div class=\"box-country\"><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div>";
                numero=numero+1;
              } else if(numero==2) {
                row_chart +="<div class=\"col-lg-6\"><div class=\"box-country\"><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div></div>";
                numero=numero-1;
              };

            } else if(chart_columns==3) {


              //Excepción Ambientales
              if(code=="33") {
                var first_column=6;
                var next_column=3;
                var margin_left_1=" style=\" margin-left:-3%; margin-right:4%; \" ";
                var margin_left_2=" style=\"margin-left:-3%;  \" ";
                var width_first_box=" style=\"width:95%\" ";
                var width_nex_box=" style=\"width:108%\"  ";

              } else {
                var first_column=4;
                var next_column=4;
                var margin_left_1="";
                var margin_left_2="";
                var width_first_box="";
                var width_nex_box="";
              }


              if(numero==1) {
                row_chart+="<div class=\"row\"><div class=\"col-lg-"+first_column+"\"><div class=\"box-country\" "+width_first_box+" ><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div>";
                numero=numero+1;
              } else if(numero==2) {
                row_chart +="<div class=\"col-lg-"+next_column+"\" "+margin_left_1+" ><div class=\"box-country\" "+width_nex_box+" ><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div>";
                numero=numero+1;
              } else if(numero==3) {
                row_chart +="<div class=\"col-lg-"+next_column+"\" "+margin_left_2+" ><div class=\"box-country\" "+width_nex_box+" ><iframe scrolling=\"no\" height=\""+chart_height+"\" src=\""+chart_link+link_parameter+"lang="+langData+"&country="+country+"\" width=\"100%\"></iframe></div></div></div>";
                numero=numero-2;
              };

            }


          });

          $("#graficos_temas").append(row_chart);
            }   //************************************LINEA AGREGADA ANITA
        });
      });



    }, 600);


});

</script>