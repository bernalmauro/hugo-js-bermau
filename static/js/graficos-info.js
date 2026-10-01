(function () {
  const months = [
    'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
    'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'
  ];

  function sourceFor(graph) {
    const article = graph.closest('.article-graficos');

    if (
      !article ||
      !/\/graficos\/bolivia\/?$/.test(location.pathname)
    ) return '';

    let heading = '';

    article.querySelectorAll('h3').forEach(function (node) {
      if (
        node.compareDocumentPosition(graph) &
        Node.DOCUMENT_POSITION_FOLLOWING
      ) {
        heading = node.textContent.trim();
      }
    });

    if (/Banco Central de Bolivia/i.test(heading)) {
      return 'Banco Central de Bolivia (BCB)';
    }

    if (/Instituto Nacional de Estad/i.test(heading)) {
      return 'Instituto Nacional de Estadística (INE)';
    }

    if (/Ministerio de Econom/i.test(heading)) {
      return 'Ministerio de Economía y Finanzas Públicas (MEFP)';
    }

    return '';
  }

  function lastPeriod(trace, annual) {
    let last = '';

    (trace.x || []).forEach(function (value, index) {
      const y = trace.y?.[index];

      if (typeof y !== 'number' || !Number.isFinite(y)) return;

      const match =
        /^(\d{4})-(\d{2})-(\d{2})(?:$|T| )/.exec(String(value));

      if (!match) return;

      const date = new Date(
        Date.UTC(+match[1], +match[2] - 1, +match[3])
      );

      if (
        date.getUTCFullYear() !== +match[1] ||
        date.getUTCMonth() !== +match[2] - 1 ||
        date.getUTCDate() !== +match[3]
      ) return;

      const period = annual
        ? match[1]
        : match[1] + '-' + match[2];

      if (period > last) last = period;
    });

    return last;
  }

  function formatPeriod(period) {
    if (period.length === 4) return period;

    return months[Number(period.slice(5, 7)) - 1] +
      ' de ' + period.slice(0, 4);
  }

  function read(graph) {
    const title = typeof graph.layout?.title === 'string'
      ? graph.layout.title
      : graph.layout?.title?.text || '';

    const annual = /\banual\b/i.test(title);

    const traces = (graph.data || []).filter(trace =>
      trace.showlegend !== false && trace.name
    );

    const entries = traces.length ? traces : (graph.data || []);

    const periods = entries
      .map(trace => lastPeriod(trace, annual))
      .filter(Boolean)
      .sort();

    let period = 'Periodo: no identificado';

    if (periods.length) {
      const first = periods[0];
      const last = periods[periods.length - 1];

      period = first === last
        ? 'Último dato en este gráfico: ' + formatPeriod(last)
        : 'Últimos datos según serie: ' +
          formatPeriod(first) + ' – ' + formatPeriod(last);

      if (periods.length < entries.length) {
        period += ' (series con fecha identificable)';
      }
    }

    const source = sourceFor(graph);

    return {
      source: source
        ? 'Fuente: ' + source
        : 'Fuente: pendiente de identificar',
      period: period
    };
  }

  window.BMChartInfo = { read: read };
})();