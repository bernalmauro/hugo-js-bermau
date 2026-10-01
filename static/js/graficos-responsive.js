(function () {
  const states = new WeakMap();
  const mobile = window.matchMedia('(max-wid  static/jsth: 767px)');
  const clone = value => JSON.parse(JSON.stringify(value));
  let pending;

    const palette = [
    '#218C83', '#B87532', '#4B87CC', '#916CC1',
    '#538B41', '#C65C77', '#788593', '#AD8A22',
    '#288DAA', '#AE705B', '#727AC7', '#95834D'
  ];

  function paintSeries(data) {
    data.forEach(function (trace, index) {
      const color = palette[index % palette.length];

      if (
        !trace.type ||
        trace.type === 'scatter' ||
        trace.type === 'scattergl'
      ) {
        trace.line = Object.assign({}, trace.line, {
          color: color,
          width: 2.8
        });

        if (!Array.isArray(trace.marker?.color)) {
          trace.marker = Object.assign({}, trace.marker, {
            color: color
          });
        }

        if (trace.fill && trace.fill !== 'none') {
          const rgb = color.slice(1).match(/../g)
            .map(part => parseInt(part, 16));

          trace.fillcolor = 'rgba(' + rgb.join(',') + ',0.22)';
        }
      } else if (
        trace.type === 'bar' &&
        !Array.isArray(trace.marker?.color)
      ) {
        trace.marker = Object.assign({}, trace.marker, {
          color: color
        });
      }
    });
  }

  async function updateChart(graph, state, updates) {
    if (state.paletteApplied) {
      return Plotly.relayout(graph, updates);
    }

    const data = clone(graph.data);
    paintSeries(data);

    const lines = [];
    const bars = [];

    data.forEach(function (trace, index) {
      if (
        !trace.type ||
        trace.type === 'scatter' ||
        trace.type === 'scattergl'
      ) {
        lines.push(index);
      } else if (trace.type === 'bar') {
        bars.push(index);
      }
    });

    if (lines.length) {
      await Plotly.update(graph, {
        line: lines.map(index => data[index].line),
        marker: lines.map(index => data[index].marker || {}),
        fillcolor: lines.map(index => data[index].fillcolor || null)
      }, updates, lines);
    }

    if (bars.length) {
      await Plotly.update(graph, {
        marker: bars.map(index => data[index].marker || {})
      }, lines.length ? {} : updates, bars);
    }

    if (!lines.length && !bars.length) {
      await Plotly.relayout(graph, updates);
    }

    state.paletteApplied = true;
  }


    function buildExport(graph, state) {
    const data = clone(graph.data);
    const layout = clone(graph.layout);
    const bg = '#10212A';
    const ink = '#E8F0F4';

    paintSeries(data);


    const count = state.original.showlegend === false
      ? 0
      : data.filter(trace =>
          trace.showlegend !== false &&
          trace.visible !== false &&
          trace.name
        ).length;

    const bottom = count ? count * 34 + 210 : 160;
    const height = 620 + bottom;

    Object.assign(layout, {
      width: 1200,
      height: height,
      autosize: false,
      paper_bgcolor: bg,
      plot_bgcolor: bg,

      font: {
        family: 'Arial, sans-serif',
        size: 20,
        color: ink
      },

      title: {
        text: typeof state.original.title === 'string'
          ? state.original.title
          : state.original.title?.text || '',
        x: .05,
        xanchor: 'left',
        y: .98,
        yanchor: 'top',
        font: {
          family: 'Arial, sans-serif',
          size: 28,
          color: ink
        }
      },

      margin: {
        l: 100,
        r: 40,
        t: 105,
        b: bottom,
        autoexpand: false
      },

      showlegend: state.original.showlegend !== false,

      legend: {
        orientation: 'v',
        x: 0,
        xanchor: 'left',
        y: -.13,
        yanchor: 'top',
        traceorder: state.original.legend?.traceorder || 'normal',
        font: {
          family: 'Arial, sans-serif',
          size: 20,
          color: ink
        },
        bgcolor: bg
      },

      updatemenus: [],
      sliders: []
    });

    Object.keys(layout)
      .filter(key => /^[xy]axis\d*$/.test(key))
      .forEach(function (key) {
        const axis = layout[key];

        axis.tickfont = {
          family: 'Arial, sans-serif',
          size: 18,
          color: ink
        };

        axis.linecolor = '#526774';
        axis.gridcolor = '#283E4A';
        axis.zerolinecolor = '#526774';

        if (axis.title && typeof axis.title === 'object') {
          axis.title.font = {
            family: 'Arial, sans-serif',
            size: 20,
            color: ink
          };
        }

        if (key.startsWith('x')) {
          axis.tickmode = 'auto';
          axis.nticks = 7;
          axis.tickangle = 0;

          if (axis.rangeslider) {
            axis.rangeslider.visible = false;
          }
        }
      });

    layout.annotations = (layout.annotations || [])
      .filter(note =>
        !/bernalmauricio\.com/i.test(note.text || '')
      )
      .map(note => Object.assign({}, note, {
        font: Object.assign({}, note.font, {
          color: ink
        })
      }));

    if (state.info) {
      layout.annotations.push({
        x: 0,
        y: -(bottom - 100) / 515,
        xref: 'paper',
        yref: 'paper',
        xanchor: 'left',
        yanchor: 'top',
        showarrow: false,
        text: state.info.source + '<br>' + state.info.period,
        align: 'left',
        font: {
          family: 'Arial, sans-serif',
          size: 18,
          color: ink
        }
      });
    }

    layout.annotations.push({
      x: 1,
      y: -(bottom - 32) / 515,
      xref: 'paper',
      yref: 'paper',
      xanchor: 'right',
      yanchor: 'middle',
      showarrow: false,
      text: 'bernalmauricio.com',
      font: {
        family: 'Arial, sans-serif',
        size: 22,
        color: '#66D5C5'
      }
    });

    const filename = 'BM-' + state.heading.textContent
      .normalize('NFD')
      .replace(/[\u0300-\u036f]/g, '')
      .replace(/[^a-zA-Z0-9]+/g, '-')
      .replace(/^-|-$/g, '')
      .slice(0, 90);

    return {
      data,
      layout,
      height,
      filename
    };
  }

  async function exportChart(graph, state) {
    if (state.exporting) return;

    state.exporting = true;
    state.download.disabled = true;
    state.download.textContent = 'Preparando imagen…';

    const temporary = document.createElement('div');
    temporary.className = 'bm-chart-export';

    Object.assign(temporary.style, {
      position: 'fixed',
      left: '-20000px',
      top: '0',
      width: '1200px'
    });

    document.body.append(temporary);

    try {
      const image = buildExport(graph, state);
      temporary.style.height = image.height + 'px';

      await Plotly.newPlot(
        temporary,
        image.data,
        image.layout,
        {
          staticPlot: true,
          displayModeBar: false,
          responsive: false
        }
      );

      await Plotly.downloadImage(temporary, {
        format: 'png',
        width: 1200,
        height: image.height,
        scale: 2,
        filename: image.filename
      });
    } catch (error) {
      console.error('No se pudo descargar el gráfico:', error);

      alert(
        'No se pudo preparar la imagen. Intenta descargarla nuevamente.'
      );
    } finally {
      Plotly.purge(temporary);
      temporary.remove();

      state.exporting = false;
      state.download.disabled = false;
      state.download.textContent = 'Descargar PNG';
    }
  }

  function prepare(graph) {
    if (states.has(graph)) return states.get(graph);

    const layout = graph.layout;

    const card = document.createElement('section');
    card.className = 'bm-chart-card';
    card.style.width = graph.style.width || '500px';

    graph.before(card);
    card.append(graph);

    const heading = document.createElement('h4');
    heading.className = 'bm-chart-heading';

    const title = document.createElement('div');

    title.innerHTML = (
      typeof layout.title === 'string'
        ? layout.title
        : layout.title?.text || ''
    ).replace(/<br\s*\/?\s*>/gi, '\n');

    heading.textContent = title.textContent;
    card.prepend(heading);

    const details = document.createElement('details');
    details.className = 'bm-chart-series';

    const summary = document.createElement('summary');
    summary.textContent = 'Ver y elegir series';
    details.append(summary);

    const list = document.createElement('div');
    list.className = 'bm-chart-series-list';
    details.append(list);

    const actions = document.createElement('div');
    actions.className = 'bm-chart-actions';

    const download = document.createElement('button');
    download.type = 'button';
    download.className = 'bm-chart-download';
    download.textContent = 'Descargar PNG';

    actions.append(download);
    card.append(actions, details);

    const state = {
      card,
      heading,
      details,
      list,
      download,
      original: clone(layout),
      width: graph.style.width,
      height: graph.style.height,
      signature: '',
      keys: ''
    };

    states.set(graph, state);

        if (window.BMChartInfo) {
      state.info = window.BMChartInfo.read(graph);

      const info = document.createElement('div');
      info.className = 'bm-chart-info';

      const source = document.createElement('div');
      const period = document.createElement('div');

      source.textContent = state.info.source;
      period.textContent = state.info.period;

      info.append(source, period);
      actions.before(info);
    }

    download.addEventListener('click', function () {
      exportChart(graph, state);
    });

    graph.addEventListener('click', function (event) {
      const button = event.target.closest('.modebar-btn');

      if (
        button &&
        /download plot|descargar|png/i.test(
          button.getAttribute('data-title') || ''
        )
      ) {
        event.preventDefault();
        event.stopImmediatePropagation();
        exportChart(graph, state);
      }
    }, true);

        graph.on('plotly_afterplot', function () {
      syncLegend(graph, state);

      const title = typeof graph.layout.title === 'string'
        ? graph.layout.title
        : graph.layout.title?.text || '';

      if (
        mobile.matches &&
        (
          title ||
          graph.layout.showlegend !== false ||
          graph.layout.height !== 350
        )
      ) {
        state.signature = '';
        schedule();
      }
    });

    return state;
  }

  function syncLegend(graph, state) {
    const traces = (graph.data || [])
      .map((trace, index) => ({ trace, index }))
      .filter(item =>
        item.trace.showlegend !== false &&
        item.trace.visible !== false &&
        item.trace.name
      );

    state.details.hidden =
      state.original.showlegend === false || !traces.length;

    const keys = JSON.stringify(
      traces.map(item => [item.index, item.trace.name])
    );

    if (keys !== state.keys) {
      state.keys = keys;
      state.list.replaceChildren();

      traces.forEach(function ({ trace, index }) {
        const label = document.createElement('label');

        const input = document.createElement('input');
        input.type = 'checkbox';
        input.dataset.traceIndex = index;

        const swatch = document.createElement('span');
        swatch.className = 'bm-chart-swatch';

        const full = graph._fullData[index];
        const color = full.line?.color || full.marker?.color;

        swatch.style.backgroundColor =
          typeof color === 'string' ? color : '#0b6170';

        const text = document.createElement('span');
        text.textContent = trace.name;

        label.append(input, swatch, text);
        state.list.append(label);

        input.addEventListener('change', function () {
          Plotly.restyle(
            graph,
            {
              visible: input.checked ? true : 'legendonly'
            },
            [index]
          );
        });
      });
    }

    state.list.querySelectorAll('input').forEach(function (input) {
      const trace = graph.data[Number(input.dataset.traceIndex)];

      input.checked =
        trace.visible !== false &&
        trace.visible !== 'legendonly';
              const full = graph._fullData[Number(input.dataset.traceIndex)];
      const color = full.line?.color || full.marker?.color;
      const swatch = input.parentElement.querySelector('.bm-chart-swatch');

      if (swatch && typeof color === 'string') {
        swatch.style.backgroundColor = color;
      }
    });
  }

  function adjust() {
    if (!window.Plotly) return;

    document.querySelectorAll(
      '.article-graficos .plotly.html-widget'
    ).forEach(function (graph) {
      if (
        !graph._fullLayout ||
        !graph.data ||
        !graph.getBoundingClientRect().width
      ) {
        return;
      }

      const state = prepare(graph);

      if (!state.card.querySelector('.bm-chart-credit')) {
        const credit = document.createElement('div');
        credit.className = 'bm-chart-credit';
        credit.textContent = 'bernalmauricio.com';
        graph.after(credit);
      }

      graph.style.width = mobile.matches ? '100%' : state.width;
      graph.style.height = mobile.matches ? '350px' : state.height;

      const width = Math.round(
        graph.getBoundingClientRect().width
      );

      const dark = document.body.classList.contains('modo-oscuro');
      const key = mobile.matches + ':' + width + ':' + dark;

      if (key === state.signature) return;

      state.signature = key;
      const original = state.original;

      const updates = {
        width: width,

        height: mobile.matches
          ? 350
          : parseFloat(state.height) || original.height || 480,

        autosize: true,
        paper_bgcolor: dark ? '#10212A' : '#ffffff',
        plot_bgcolor: dark ? '#10212A' : '#ffffff',

        title: mobile.matches ? { text: '' } : original.title,

        showlegend: mobile.matches ? false : original.showlegend,

        margin: mobile.matches
          ? { l: 56, r: 14, t: 70, b: 44 }
          : original.margin,

        updatemenus: mobile.matches
          ? (original.updatemenus || []).map(function (menu, index) {
              return Object.assign({}, menu, {
                x: 0,
                xanchor: 'left',
                y: 1.16 - index * .2,
                yanchor: 'top',
                font: { size: 12 },
                pad: { t: 0, r: 0, b: 0, l: 0 }
              });
            })
          : original.updatemenus,

        annotations: (original.annotations || [])
          .filter(function (note) {
            return !/bernalmauricio\.com/i.test(note.text || '');
          })
      };

      if (original.xaxis) {
        updates['xaxis.tickmode'] = mobile.matches
          ? 'auto'
          : original.xaxis.tickmode || 'auto';

        updates['xaxis.nticks'] = mobile.matches
          ? 4
          : original.xaxis.nticks || 0;

        updates['xaxis.tickangle'] = mobile.matches
          ? 0
          : original.xaxis.tickangle ?? 'auto';

        updates['xaxis.tickfont.size'] = mobile.matches
          ? 11
          : original.xaxis.tickfont?.size || 13;
      }

      if (original.yaxis) {
        updates['yaxis.tickfont.size'] = mobile.matches
          ? 11
          : original.yaxis.tickfont?.size || 13;
      }

            updateChart(graph, state, updates).then(function () {
        syncLegend(graph, state);
      }).catch(function (error) {
        state.signature = '';
        console.error('No se pudo ajustar el gráfico:', error);
      });

      syncLegend(graph, state);
    });
  }

  function schedule() {
    clearTimeout(pending);
    pending = setTimeout(adjust, 100);
  }

  function start() {
    document.querySelectorAll(
      '.article-graficos .tab-pane'
    ).forEach(function (panel) {
      new MutationObserver(schedule).observe(panel, {
        attributes: true,
        attributeFilter: ['class']
      });
    });

    new MutationObserver(schedule).observe(document.body, {
      attributes: true,
      attributeFilter: ['class']
    });

    window.addEventListener('resize', schedule);
    window.addEventListener('load', schedule);

    if (window.jQuery) {
      jQuery(document).on('shown.bs.tab', schedule);
    }

    if (window.HTMLWidgets) {
      HTMLWidgets.addPostRenderHandler(schedule);
    }

    schedule();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();