(function () {
  const pending = new Set();
  let frame = 0;

  document.querySelectorAll('.article-graficos .plotly.html-widget')
    .forEach(function (graph) {
      if (graph.classList.contains('html-widget-static-bound')) return;

      graph.classList.remove('plotly');
      graph.classList.add('bm-plotly-pendiente');
      pending.add(graph);
    });

  function renderVisible() {
    frame = 0;

    if (!window.HTMLWidgets || !pending.size) return;

    let added = false;

    pending.forEach(function (graph) {
      if (!graph.isConnected) {
        pending.delete(graph);
        return;
      }

      if (
        !graph.getClientRects().length ||
        !graph.getBoundingClientRect().width
      ) {
        return;
      }

      graph.classList.remove('bm-plotly-pendiente');
      graph.classList.add('plotly');

      pending.delete(graph);
      added = true;
    });

    if (added) {
      window.HTMLWidgets.staticRender();
    }
  }

  function schedule() {
    if (!frame) {
      frame = requestAnimationFrame(renderVisible);
    }
  }

  document.querySelectorAll('.article-graficos .tab-pane')
    .forEach(function (panel) {
      new MutationObserver(schedule).observe(panel, {
        attributes: true,
        attributeFilter: ['class', 'style', 'hidden']
      });
    });

  window.addEventListener('resize', schedule);
  window.addEventListener('load', schedule);

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', schedule);
  } else {
    schedule();
  }
})();