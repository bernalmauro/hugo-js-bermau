'use strict';
let economicChart = null;
function disposeEconomicChart() {
  if (economicChart) economicChart.dispose();
  economicChart = null;
}
function renderEconomicChart(series) {
  // Keep the existing SVG and export available when the chart library is unavailable.
  if (!window.echarts) return;
  const host = $('chart');
  const fallback = host.innerHTML;
  try {
    host.innerHTML = '<div class="economic-chart-canvas" role="img" aria-label="Gráfico interactivo de indicadores económicos"></div><button class="chart-reset" type="button">Restablecer vista</button><p class="chart-help">Desliza los extremos de la barra para ampliar un período. Toca la leyenda para ocultar o mostrar una serie. Consulta los valores con el cursor o tocando el gráfico.</p>';
    const canvas = host.querySelector('.economic-chart-canvas');
    economicChart = echarts.init(canvas, null, {renderer:'canvas'});
    const periods = [...new Set(series.flatMap(s => s.points.map(p => p[0])))].sort();
    const row = series[0].row;
    const regular = ['Anual','Mensual','Trimestral'].includes(row.frequency);
    // Fill missing regular periods with null, preserving real temporal spacing.
    let axisPeriods = periods;
    if (regular && periods.length && periods.every(p => ordinal(p) !== null)) {
      const first = ordinal(periods[0]), last = ordinal(periods.at(-1));
      if(last-first < 20000) axisPeriods = Array.from({length:last-first+1},(_,i) => {
        const n=first+i;
        if(row.frequency==='Anual') return String(n);
        if(row.frequency==='Mensual') return `${Math.floor(n/12)}-${String(n%12+1).padStart(2,'0')}`;
        return `${Math.floor(n/4)}-Q${n%4+1}`;
      });
    }
    const small = canvas.clientWidth < 500;
    economicChart.setOption({
      animation:false, backgroundColor:'#101114', color:colors,
      textStyle:{fontFamily:'system-ui, sans-serif',color:'#b3b4ba'},
      aria:{enabled:true},
      grid:{left:small?12:24,right:24,top:60,bottom:112,containLabel:true},
      legend:{type:'scroll',top:8,left:8,right:8,textStyle:{color:'#f4f2ed',width:small?155:270,overflow:'truncate'},pageTextStyle:{color:'#b3b4ba'},pageIconColor:'#ffad45',tooltip:{show:true}},
      tooltip:{trigger:'axis',confine:true,backgroundColor:'#181a1f',borderColor:'#ffad45',textStyle:{color:'#f4f2ed'},formatter:params => {
        const items=params.filter(p => p.data && Number.isFinite(p.data.value));
        return '<div class="chart-tooltip"><strong>'+esc(params[0]?.axisValueLabel||'')+'</strong>'+items.map(p => '<div style="margin-top:8px">'+p.marker+esc(p.seriesName)+'<br><strong>'+esc(fmt(p.data.value))+'</strong> '+esc(row.unit)+'<br>'+esc(p.data.status||'')+'</div>').join('')+'</div>';
      }},
      xAxis:{type:'category',boundaryGap:false,data:axisPeriods,axisLabel:{hideOverlap:true,color:'#b3b4ba'},axisLine:{lineStyle:{color:'#535762'}}},
      yAxis:{type:'value',scale:true,axisLabel:{color:'#b3b4ba',formatter:value=>fmt(value)},splitLine:{lineStyle:{color:'#30333b'}}},
      dataZoom:[{type:'slider',bottom:30,height:26,start:0,end:100,filterMode:'none',borderColor:'#535762',fillerColor:'rgba(255,173,69,.18)',textStyle:{color:'#b3b4ba'}},{type:'inside',filterMode:'none',zoomOnMouseWheel:'ctrl',moveOnMouseWheel:false}],
      graphic:[{type:'text',right:12,bottom:6,style:{text:'bernalmauricio.com · Economicus',fill:'#ffe18a',font:'12px sans-serif'}}],
      series:series.map(s=>{
        const points=new Map(s.points.map(p=>[p[0],p]));
        return {id:String(s.row.series_id),name:s.row.public_name,type:'line',connectNulls:false,showSymbol:s.points.length<=80,symbolSize:6,lineStyle:{width:2.5},emphasis:{focus:'series'},data:axisPeriods.map(period=>{const p=points.get(period);return {value:p&&Number.isFinite(p[1])?p[1]:null,status:p?.[2]||''};})};
      })
    });
    host.querySelector('.chart-reset').onclick=()=>{
      economicChart.dispatchAction({type:'dataZoom',start:0,end:100});
      economicChart.dispatchAction({type:'legendAllSelect'});
    };
  } catch (error) {
    disposeEconomicChart();
    host.innerHTML=fallback;
    console.warn('Economicus: se usa el gráfico de respaldo.',error);
  }
}
function downloadEconomicChart() {
  if(!economicChart) return false;
  // Export the visible zoom and legend selection, with title, units and sources.
  const picture=new Image();
  picture.onload=()=>{
    const width=1600, scale=width/picture.width;
    const lines=wrapText($('title').textContent,100);
    const sources=wrapText('Fuentes: '+[...new Set(st.selection.map(s=>sourceLabel(s.row)))].join(' · '),120);
    const top=55+lines.length*28, bottom=40+sources.length*23;
    const canvas=document.createElement('canvas');canvas.width=width;canvas.height=Math.ceil(picture.height*scale+top+bottom);
    const ctx=canvas.getContext('2d');ctx.fillStyle='#101114';ctx.fillRect(0,0,canvas.width,canvas.height);
    ctx.fillStyle='#f4f2ed';ctx.font='bold 24px sans-serif';lines.forEach((line,i)=>ctx.fillText(line,24,34+i*28));
    ctx.fillStyle='#b3b4ba';ctx.font='18px sans-serif';ctx.fillText($('subtitle').textContent,24,top-12);
    ctx.drawImage(picture,0,top,width,picture.height*scale);
    sources.forEach((line,i)=>ctx.fillText(line,24,top+picture.height*scale+28+i*23));
    canvas.toBlob(blob=>{if(blob)download(blob,'economicus-grafico.png');else message('No se pudo exportar el gráfico.');},'image/png');
  };
  picture.onerror=()=>message('No se pudo exportar el gráfico.');
  picture.src=economicChart.getDataURL({type:'png',pixelRatio:2,backgroundColor:'#101114'});
  return true;
}
