/** @type {Dygraph.Ticker} */
Dygraph.numericTicks = function(min, max, pixels, opts, dygraph, vals) {
    var pixels_per_tick = /** @type{number} */(opts('pixelsPerLabel'));
    var ticks = [];
    var i, j, tickV, nTicks;
    if (vals) {
      for (i = 0; i < vals.length; i++) {
        ticks.push({v: vals[i]});
      }
    } else {
      // TODO(danvk): factor this log-scale block out into a separate function.
      if (opts("logscale")) {
        nTicks  = Math.floor(pixels / pixels_per_tick);
        var minIdx = Dygraph.binarySearch(min, Dygraph.PREFERRED_LOG_TICK_VALUES, 1);
        var maxIdx = Dygraph.binarySearch(max, Dygraph.PREFERRED_LOG_TICK_VALUES, -1);
        if (minIdx == -1) {
          minIdx = 0;
        }
        if (maxIdx == -1) {
          maxIdx = Dygraph.PREFERRED_LOG_TICK_VALUES.length - 1;
        }
        // Count the number of tick values would appear, if we can get at least
        // nTicks / 4 accept them.
        var lastDisplayed = null;
        if (maxIdx - minIdx >= nTicks / 4) {
          for (var idx = maxIdx; idx >= minIdx; idx--) {
            var tickValue = Dygraph.PREFERRED_LOG_TICK_VALUES[idx];
            var pixel_coord = Math.log(tickValue / min) / Math.log(max / min) * pixels;
            var tick = { v: tickValue };
            if (lastDisplayed === null) {
              lastDisplayed = {
                tickValue : tickValue,
                pixel_coord : pixel_coord
              };
            } else {
              if (Math.abs(pixel_coord - lastDisplayed.pixel_coord) >= pixels_per_tick) {
                lastDisplayed = {
                  tickValue : tickValue,
                  pixel_coord : pixel_coord
                };
              } else {
                tick.label = "";
              }
            }
            ticks.push(tick);
          }
          // Since we went in backwards order.
          ticks.reverse();
        }
      }
  
      // ticks.length won't be 0 if the log scale function finds values to insert.
      if (ticks.length === 0) {
        // Basic idea:
        // Try labels every 1, 2, 5, 10, 20, 50, 100, etc.
        // Calculate the resulting tick spacing (i.e. this.height_ / nTicks).
        // The first spacing greater than pixelsPerYLabel is what we use.
        // TODO(danvk): version that works on a log scale.
        var kmg2 = opts("labelsKMG2");
        var mults, base;
        if (kmg2) {
          mults = [1, 2, 4, 8, 16, 32, 64, 128, 256];
          base = 16;
        } else {
          mults = [1, 2, 5, 10, 20, 50, 100];
          base = 10;
        }
  
        // Get the maximum number of permitted ticks based on the
        // graph's pixel size and pixels_per_tick setting.
        var max_ticks = Math.ceil(pixels / pixels_per_tick);
  
        // Now calculate the data unit equivalent of this tick spacing.
        // Use abs() since graphs may have a reversed Y axis.
        var units_per_tick = Math.abs(max - min) / max_ticks;
  
        // Based on this, get a starting scale which is the largest
        // integer power of the chosen base (10 or 16) that still remains
        // below the requested pixels_per_tick spacing.
        var base_power = Math.floor(Math.log(units_per_tick) / Math.log(base));
        var base_scale = Math.pow(base, base_power);
  
        // Now try multiples of the starting scale until we find one
        // that results in tick marks spaced sufficiently far apart.
        // The "mults" array should cover the range 1 .. base^2 to
        // adjust for rounding and edge effects.
        var scale, low_val, high_val, spacing;
        for (j = 0; j < mults.length; j++) {
          scale = base_scale * mults[j];
          low_val = Math.floor(min / scale) * scale;
          high_val = Math.ceil(max / scale) * scale;
          nTicks = Math.abs(high_val - low_val) / scale;
          spacing = pixels / nTicks;
          if (spacing > pixels_per_tick) break;
        }
  
        // Construct the set of ticks.
        // Allow reverse y-axis if it's explicitly requested.
        if (low_val > high_val) scale *= -1;
        for (i = 0; i <= nTicks; i++) {
          tickV = low_val + i * scale;
          ticks.push( {v: tickV} );
        }
      }
    }
  
    var formatter = /**@type{AxisLabelFormatter}*/(opts('axisLabelFormatter'));
  
    // Add labels to the ticks.
    for (i = 0; i < ticks.length; i++) {
      if (ticks[i].label !== undefined) continue;  // Use current label.
      // TODO(danvk): set granularity to something appropriate here.
      ticks[i].label = formatter.call(dygraph, ticks[i].v, 0, opts, dygraph);
    }
  
    return ticks;
  };

  
g = new Dygraph(div, data,{
    axes: {
      y: {
        ticker: function(min, max, pixels, opts, dygraph, vals) {
          return [{v:0, label:0}, {v:5, label:5}, {v:10, label:10}];
        }
      }
    }
  });

  var g = new DyGraph(div,data,{
    axes: {
      y: {
        ticker: function(min, max) {
          ticks = [];
          for (var i = min; i <= max; i = i + 3) {
            ticks.push({v: i,label:i});
          }
          return ticks;
        }
      }
    }
  });
