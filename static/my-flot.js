// Do not edit, generated from parenscript.
dw.myFlot = (function () {
    var myFlot = this;
    var initScatterPlot = function (jq) {
        var setupPlot = function (instructions) {
            return jq.plot(instructions.series, instructions.details);
        };
        var d1 = function () {
            return { 'label' : 'up', 'data' : (function () {
                var collect2 = [];
                for (var i = 1; i <= 10; i += 1) {
                    collect2.push([i, i]);
                };
                return collect2;
            })() };
        };
        var d2 = function () {
            return { 'label' : 'random', 'data' : (function () {
                var collect3 = [];
                for (var i = 1; i <= 10; i += 1) {
                    collect3.push([i, Math.floor(10 * Math.random()) + 1]);
                };
                return collect3;
            })() };
        };
        return setupPlot({ 'series' : [d1(), d2()], 'details' : { 'series' : { 'lines' : { 'show' : null }, 'points' : { show : true } } } });
    };
    this.initScatterPlot = initScatterPlot;
    var setupFunction = function ($) {
        var revisePlot = function (plottingInstructions) {
            var series4 = plottingInstructions.series;
            var details5 = plottingInstructions.details;
            return this.each(function (i, x) {
                return $(x).plot(series4, details5);
            });
        };
        return $.fn.revisePlot = revisePlot;
    };
    setupFunction(jQuery);
    return this;
}).call({  });
// Do not edit, generated from parenscript.
