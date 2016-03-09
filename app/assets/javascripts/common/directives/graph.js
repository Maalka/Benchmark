/**
 * A building property directive
 * changes form inputs based on property type
 * and supports multiple property types
 */
define(['angular','highcharts', './main'], function(angular) {
  'use strict';

  var mod = angular.module('common.directives');

  mod.directive('graph', [function() {
      return {
          restrict: 'A',
          scope: {
            benchmarkResult: '=benchmarkResult',
          },
          controller: ["$scope", "$element","$timeout", function ($scope, $element, $timeout) {
            var bars = [];
            var k = 20;
            for (k; k < 120; k += 20) {
              bars.push([k, 120]);
            }
            var createMarker = function(topPNG, bottomPNG, title, yOff, x, color, series) {
              if (x !== undefined && !isNaN(x)) {
                return [{
                        type: 'flags',
                        color: '#333333',
                        fillColor: 'rgba(255,255,255,0.8)',
                        shape: "url(/assets/images/" + topPNG + ")",
                        style: {
                          color: color
                        },
                        y: yOff[0],
                        data: [
                            { x: fixX(x), 
                              title: ""+parseInt(x)}
                        ],
                        onSeries: series,
                        showInLegend: false
                    },
                    {
                        type: 'flags',
                        color: '#333333',
                        style: {
                          color: color
                        },                      
                        shape: "url(/assets/images/" + bottomPNG + ")",
                        fillColor: 'rgba(255,255,255,0.8)',
                        y: yOff[1],
                        data: [ 
                            { x: fixX(x), 
                              title: ""+zepiToEUI(x)}
                        ],
                        onSeries: series,                      
                        showInLegend: false
                    },
                    {
                        type: 'flags',
                        color: 'transparent',
                        style: {
                          color: "black",
                          fontWeight: 'bold',
                          fontSize: "13",
                        },
                        shape: "squarepin",
                        fillColor: 'rgba(255,255,255,0.8)',
                        y: yOff[2],
                        data: [
                            { x: fixX(x), 
                              title: ""+title}
                        ],
                        onSeries: series,
                        showInLegend: false
                    }];
                  } else {
                    return [];
                  }
            };
            // flags don't seem to work on series where the axis is reversed
            var maxX = 120;
            var minX = -20;
            var getBRByKey = function (key) {
              var r = $scope.benchmarkResult.filter(function (v) { 
                return v[key];
              });
              if (r.length !== 0) {
                return r[0][key];
              } else {
                return undefined;
              }
            };

            var baselineEUI;
            var labels = {};
            var fixX = function(x) {
              if (x > 100) {
                x = 110;
              }
              if (x < 0) {
                x = -10;
              }
              return maxX - x;
            };
            var zepiToEUI = function(zepi) { 
              return  parseInt(zepi / 100 * baselineEUI);
            };
            var plot = function () {
              var margin = [75, 0, 0, 0];
              baselineEUI = getBRByKey("medianSiteEUI");
              if (getBRByKey("actualZEPI") !== undefined) {
                margin[2] = 75; 
              }
              var options = {
                  chart: {
                      margin: margin,
                      height: 300,
                      events: {
                        'load': function () {
                          if (getBRByKey("actualZEPI") !== undefined) {
                            loadMarkers('bottom', this, this.series[6], 55, 45);
                         }
                          loadMarkers('top', this, this.series[3], 55, 4);
                        },
                        'redraw': function () {
                          if (getBRByKey("actualZEPI") !== undefined) {
                            loadMarkers('bottom', this, this.series[6], 55, 45);
                          }
                          loadMarkers('top', this, this.series[3], 55, 4);
                        }
                      }
                  },
                  title: {
                      text: null
                  },
                  subtitle: {
                      text: null
                  },
                  legend: {
                    enabled: false
                  },
                  xAxis: {
                      gridLineColor: "black",
                      gridLineDash: "transparent",
                      gridLineWidth: 0,
                      gridZIndex: 0,
                      minorTickLength: 0,
                      minorGridLineWidth: 0,
                      tickLength: 0,
                      lineWidth: 0,

                      labels: {
                          enabled: false
                      },
                  },
                  yAxis: {
                      title: {
                          enabled: false,
                      },
                      labels: {
                          enabled: false
                      },
                      gridLineColor: "black",
                      gridLineDash: "solid",
                      gridLineWidth: 0,
                      lineWidth: 1,
                      plotLines: [{
                        color: "black",
                        width: 1,
                        value: 0,
                        zIndex: 11,
                      }],
                      gridZIndex: 5
                      
                  },
                  tooltip: {
                    enabled: false,
                      pointFormat: '{series.name} produced <b>{point.y:,.0f}</b><br/>warheads in {point.x}'
                  },
                  plotOptions: {
                      area: {
                          pointStart: 1940,
                          marker: {
                              enabled: false,
                              symbol: 'circle',
                              radius: 2,
                              states: {
                                  hover: {
                                      enabled: true
                                  }
                              }
                          },
                          color: {
                            linearGradient: { x1: 0, x2: 1, y1: 0, y2: 0 },
                            stops: [
                              [0, '#ff0000'],
                              [1, '#00ff00']
                            ]
                          }
                      }
                  },
                  series: [{
                    name: 'zepi',
                    id: 'zepi',
                    type: 'area',
                    data: [[minX + 20, maxX], [maxX + 20, minX]],
                    tooltip: {
                        xDateFormat: '%B %Y',
                        valueSuffix: ' % of best month'
                    }

                  },
                  { type: 'column', 
                    name: 'lines',
                    color: "white",
                    pointWidth: 1,
                    borderWidth: 0,
                    animation: false,
                    data: bars
                  }]
                    .concat(createMarker("boxTop.png", "boxBottom.png", "Baseline", [-70, -41, -100], 100, "black", "zepi"))
                    .concat(createMarker("ratingTop.png", "ratingBottom.png", "Rating", [-30, -2, 22], getBRByKey("actualZEPI"), "white"))
                    .concat(createMarker("targetTop.png", "targetBottom.png", "Target", [-70, -41, -100], getBRByKey("percentBetterZEPI"), "white", "zepi"))
                    .concat(createMarker("boxTop.png", "boxBottom.png", "Net Zero", [-70, -41, -100], 0, "black", "zepi"))
              };
              var loadMarkers = function(tag, obj, series, xOff, yOff) { 
                if (labels[tag+ "1"] === undefined) {
                  labels[tag + "1"] = obj.renderer.label('zEPI', 
                      series.data[0].plotX - xOff,
                      series.data[0].plotY + yOff)
                                  .css({
                                      fontWeight: 'bold'
                                  })
                                  .add();

                } else {
                  labels[tag + "1"].xSetter(series.data[0].plotX - xOff);
                  labels[tag + "1"].ySetter(series.data[0].plotY + yOff);
                }
                if (labels[tag+ "2"] === undefined) {
                  labels[tag + "2"] = obj.renderer.label('EUI', 
                      series.data[0].plotX - xOff,
                      series.data[0].plotY + yOff + 30)
                                  .css({
                                      fontWeight: 'bold'
                                  })
                                  .add();
                } else {  
                  labels[tag + "2"].xSetter(series.data[0].plotX - xOff);
                  labels[tag + "2"].ySetter(series.data[0].plotY + yOff + 30);
                }
              };
              $timeout(function () {
                angular.element($element).highcharts(options);
              }, 0);
            };
            if ($scope.benchmarkResult !== undefined) {
              plot();
            }
            $scope.$watch("benchmarkResult", function (br) { 
              labels = {};
              if (br !== undefined) {
                plot();
              }
            });
          }]
        };
  }]);
});