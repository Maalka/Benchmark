/**
 * A building property directive
 * changes form inputs based on property type
 * and supports multiple property types
 */
define(['angular','highcharts', 'maalkaflags', './main'], function(angular) {
  'use strict';

  var mod = angular.module('common.directives');

  mod.directive('graph', [function() {
  // create the flag icon with anchor

      var round = function(x) { 
        return Math.ceil(x);
      };

      return {
          restrict: 'A',
          scope: {
            benchmarkResult: '=benchmarkResult',
            baselineConstant: '=baselineConstant',
            scoreGraph: '=scoreGraph',
          },
          controller: ["$scope", "$element","$timeout", function ($scope, $element, $timeout) {
            var chart,
                baselineEUI,
                maxX = 120,
                minX = -20,
                red = "#ee1516",
                green = "#3dab48";


            var compareDataElement = function(a, b) {
              var aX = a.x === undefined ? a[0] : a.x;
              var bX = b.x === undefined ? b[0] : b.x;
              if (aX  < bX) {
                return -1;
              } else if (aX > bX) { 
                return 1;
              } else {
                return 0;
              }
            };

            var sortData = function(data) {
              return data.sort(compareDataElement);
            };

            var showGreenEnergyChart = function () {
              if (showExtendedChart() && true) {
                return $scope.baselineConstant !== undefined;
              }
              return false;
            };

            var showExtendedChart = function () {
              return getBRByKey("actualZEPI") !== undefined;
            };

            /***
            Update or Add a series to the chart
            ***/
            var updateOrAddSeries = function(chart, options, refresh) {
              var s = chart.get(options.id);
              if (s !== undefined && s !== null && options.remove === true) {
                s.remove();
              } else if (s === undefined || s === null) {
                chart.addSeries(options);
              } else {
                s.update(options, refresh);
              }
            };

            /***
            Create the Tick marks, value data labels and area fill.  This is using a column chart because of an issues with 
            Highcharts where the tick lines arn't showing through an area fill.
            ****/
            var createBaseChartFeatures = function() {
              var bars = [];
              var k = 20;
              var dlOff = function(v) {
                if (v === 120) {
                  return -11;
                } else if (v === 20) {
                  return -28;
                } else {
                  return -20;
                }
              };
              for (k; k <= 140; k += 20) {
                bars.push(
                  {
                    x: k,
                    y: 110,
                    color: k === 140 ? 'transparent' : undefined,
                    dataLabels: {
                      x: dlOff(k),
                      y: 3
                    }
                  });
              }

              updateOrAddSeries(chart, {
                  name: 'zepi',
                  id: 'zepi',
                  type: 'area',
                  enableMouseTracking: false,
                  data: [[minX + 20, maxX], [maxX + 20, minX]],
                  tooltip: {
                      xDateFormat: '%B %Y',
                      valueSuffix: ' % of best month'
                  },
                  showInLegend: false
                }, false);
              updateOrAddSeries(chart, { type: 'column',
                  id: 'lines',
                  name: 'lines',
                  color: "white",
                  pointWidth: 2,
                  borderWidth: 0,
                  animation: false,
                  dataLabels: {
                    inside: true,
                    enabled: true,
                    align: "left",
                    useHTML: true,
                    formatter: function () {
                      var x = fixX(this.x, true);
                      if (x < 20) { 
                        return "<span style='color: #a6a8ab;'>" + x + "</span>";
                      } else {
                        return x;
                      }
                    },
                    verticalAlign: "bottom",
                    style: {
                      textShadow: false
                    }
                  },
                  enableMouseTracking: false,
                  data: bars,
                  showInLegend: false
                }, false);

              /*
                // the 0 use case... i don't think that i can dynamicly change the "0"
                updateOrAddSeries(chart, { type: 'column',
                  id: 'linesZero',
                  name: 'linesZero',
                  color: "transparent",
                  pointWidth: 2,
                  borderWidth: 0,
                  animation: false,
                  dataLabels: {
                    inside: true,
                    enabled: true,
                    align: "left",
                    useHTML: true,
                    formatter: function () {
                      var x = fixX(this.x, true);
                      return "<span style='color: #a6a8ab;'>" + x + "</span>";
                    },
                    verticalAlign: "bottom",
                    style: {
                      textShadow: false
                    }
                  },
                  enableMouseTracking: false,
                  data: [
                    {
                      x: 120, 
                      y: 110,
                      dataLabels: {
                        x: -11,
                        y: 3
                      }
                    },
                    {
                      x: 140, 
                      y: 110,
                      dataLabels: {
                        x: showExtendedChart() ? -39 : -33,
                        y: 3
                      }
                    }],
                  showInLegend: false
                }, false);
                */
              updateOrAddSeries(chart, {
                name: "axisLine",
                id: 'axisLine',
                enableMouseTracking: false,
                color: "transparent",
                lineWidth: 0,
                marker: {
                  enabled: false
                },
                data: [[minX + 20, 0], [maxX + 20, 0]],
                showInLegend: false
              });

               var yOffset = 0;
              if (round($scope.baselineConstant) - round(getBRByKey("percentBetterZEPI")) < 25) { 
                yOffset = 30;
              }
              var markers = createMarker("BASELINE", -40 - yOffset, $scope.baselineConstant, "maalkaFlag", "black", 'zepi', false)
                        .concat(createMarker("TARGET", -40, getBRByKey("percentBetterZEPI"), "maalkaFlag", "black", "zepi", false));

              for (var i = 0; i < markers.length; i ++) {
                updateOrAddSeries(chart, markers[i], i === markers.length - 1, false);
              }

            };

            var createExtendedChartFeatures = function (remove) {
              var gap = $scope.baselineConstant - getBRByKey("actualZEPI");
              if (remove) {
                updateOrAddSeries(chart, { "id": "progressLine", "remove": true}, false);
              } else {
                updateOrAddSeries(chart, { type: 'line', 
                  name: "progressLine",
                  id: 'progressLine',
                  data: sortData([
                    {
                      x: fixX(getBRByKey("actualZEPI")), 
                      y: -15,
                      marker: {
                        enabled: fixX(getBRByKey("percentBetterZEPI")) < fixX(getBRByKey("actualZEPI")),
                        symbol: "arrow-right"
                      } 
                    },
                    { 
                      x: fixX(getBRByKey("percentBetterZEPI")),
                      y: -15,
                      marker: {
                        enabled: fixX(getBRByKey("percentBetterZEPI")) > fixX(getBRByKey("actualZEPI")),
                        symbol: "arrow-right"
                      }
                    }
                  ]),
                  animation: false,
                  color: fixX(getBRByKey("percentBetterZEPI")) < fixX(getBRByKey("actualZEPI")) ? green : red,
                  arrow: true,
                  dashStyle: "ShortDot",
                  showInLegend: false,
                  enableMouseTracking: false
                });
              }

              updateOrAddSeries(chart, 
                createMarker("YOUR BUILDING", 40, getBRByKey("actualZEPI"), 
                  gap > 40 ? "maalkaFlagLeftBottom" : "maalkaFlagBottom", "black", "axisLine", false)[0],
                false
                );
              var better = fixX(getBRByKey("percentBetterZEPI")) < fixX(getBRByKey("actualZEPI"));

              var percentBetter = Math.abs(getBRByKey("actualZEPI") - getBRByKey("percentBetterZEPI"));
              updateOrAddSeries(chart, createMarker(isNaN(percentBetter) ? undefined : percentBetter, 
                              17, getBRByKey("percentBetterZEPI"), better ? "maalkaLongFlagLeftBottom": "maalkaLongFlagBottom", 
                              better ? green : red, "axisLine", true)[0],
                  false
                  );
            };

            var createGreenChartFeatures = function (remove) {
              var gap = $scope.baselineConstant - getBRByKey("actualZEPI");
              var energyEfficiency = 100;
              var onsiteRenewable = 40;
              var greenPower = 60;
              var total = energyEfficiency + onsiteRenewable + greenPower;
              if (remove) {
                updateOrAddSeries(chart, {id: "componentLineLeft", remove: true}, false);
                updateOrAddSeries(chart, {id: "componentLineRight", remove: true}, false);
                updateOrAddSeries(chart, {id: "energyEfficiency", remove: true}, false);
                updateOrAddSeries(chart, {id: "onsiteRenewable", remove: true}, false);
                updateOrAddSeries(chart, {id: "greenPower", remove: true}, false);
              } else {
                updateOrAddSeries(chart, { type: 'line', 
                  name: "componentLineLeft",
                  id: 'componentLineLeft',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant), 
                      y: 0,
                      marker: {
                        enabled: false
                      } 
                    },
                    { 
                      x: fixX($scope.baselineConstant),
                      y: -110,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  animation: false,
                  color: "#77aad4",
                  arrow: true,
                  dashStyle: "ShortDot",
                  showInLegend: false,
                  enableMouseTracking: false
                });

                updateOrAddSeries(chart, { type: 'line', 
                  name: "componentLineRight",
                  id: 'componentLineRight',
                  data: sortData([
                    {
                      x: fixX(getBRByKey("actualZEPI")), 
                      y: -85,
                      marker: {
                        enabled: false
                      } 
                    },
                    { 
                      x: fixX(getBRByKey("actualZEPI")), 
                      y: -110,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  animation: false,
                  color: "#77aad4",
                  arrow: true,
                  dashStyle: "ShortDot",
                  showInLegend: false,
                  enableMouseTracking: false
                });

                // add the energy source breakdown. 

                updateOrAddSeries(chart, { type: 'line', 
                  name: "energy efficiency",
                  id: 'energyEfficiency',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant),
                      y: -120,
                      marker: {
                        enabled: false
                      } 
                    },
                    { 
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -120,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  lineWidth: 20,
                  linecap: "square",
                  animation: false,
                  color: "#1c70b8",
                  showInLegend: true,
                  enableMouseTracking: false
                });

                updateOrAddSeries(chart, { type: 'line', 
                  name: "on-site renewable energy",
                  id: 'onsiteRenewable',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -120,
                      marker: {
                        enabled: false
                      } 
                    },
                    { 
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total)),
                      y: -120,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  lineWidth: 20,
                  linecap: "square",
                  animation: false,
                  color: "#77aad4",
                  showInLegend: true,
                  enableMouseTracking: false
                });

                updateOrAddSeries(chart, { type: 'line', 
                  name: "green power purchased",
                  id: 'greenPower',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total)),
                      y: -120,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total) -
                        gap * (greenPower / total)),
                      y: -120,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  lineWidth: 20,
                  linecap: "square",
                  animation: false,
                  color: "#a4c6e3",
                  showInLegend: true,
                  enableMouseTracking: false
                });
              }
            };

            /*** Flag Definition for the Markers  ***/
            var createMarker = function(title, yOff, x, shape, color, series, onlyTitle) {
              if (x !== undefined && !isNaN(x) && title !== undefined) {
                var text = "";
                if (onlyTitle) {
                  if (x) { 
                    text = round(title) + "%";
                  } else {
                    return;
                  }
                } else  if (title === "YOUR BUILDING") {
                  text = "Score <b>" + round(x) + "</b><br>FF-EUI <b>" + zepiToEUI(round(x)) + "</b><br><b>"+title + "</b>";
                } else {
                  text = "<b>"+title + "</b><br><b>" + zepiToEUI(round(x)) + "</b> FF-EUI" + "<br><b>" + round(x) + "</b> Rating";
                }
                var textColor;
                if (onlyTitle) { 
                  textColor = fixX(getBRByKey("percentBetterZEPI")) < fixX(getBRByKey("actualZEPI")) ? green : red;
                }
                return [
                    {   id: onlyTitle ? series + "marker" : title + "0",
                        dataLabels:{
                          useHTML:true,
                        },
                        style: {
                          fontSize: onlyTitle ? '13px' : '11px',
                          fontWeight: 'normal',
                          color: textColor
                        },
                        type: 'maalkaFlags',
                         data: [
                            { x: fixX(x), 
                              title: text
                            }
                        ],
                        y: yOff,
                        shape: shape,
                        color: color,
                        lineWidth: 2,
                        useHtml: true,
                        onSeries: series,
                        showInLegend: false
                    }];
                } else {
                  return [
                    {id: onlyTitle ? series + "marker" : title + "0", remove: true}
                  ];
                }
            };
            // flags don't seem to work on series where the axis is reversed

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


            var labels = {};
            var fixX = function(x, noClip) {
              if($scope.baselineConstant === 130){
                  x = 100 * x / 130;
              }

              if (x > 100 && !noClip) {
                x = 100;
              }

              if (x < 0 && !noClip) {
                x = -10;
              }
              return maxX - x;
            };

            var zepiToEUI = function(zepi) {
              return  round(zepi / $scope.baselineConstant * baselineEUI);
            };

           var loadSeries = function(chart) {
              createBaseChartFeatures();
              createExtendedChartFeatures(!showExtendedChart());
              createGreenChartFeatures(!showGreenEnergyChart());

              $element.css({height: getHeight() + "px"});
              chart.margin = getMargin();
              chart.isDirtyBox = true;
              chart.redraw();
              chart.reflow();
            };
            var getHeight = function() {
              var height = 200;
              baselineEUI = getBRByKey("medianSiteEUI");
              if (showExtendedChart()) {
                height = height + 120;
              }
              return height;
            };
            var getMargin = function() {
              var margin = [60, -20, 0, -30];
              baselineEUI = getBRByKey("medianSiteEUI");
              if (showExtendedChart()) {
                margin[0] = 50;
                margin[1] = -18;
                margin[2] = 20;
              }
              return margin;
            };
            var plot = function () {

              var options = {
                  chart: {
                    spacingTop: 0,
                    spacingRight: 0,
                    spacingBottom: 10,
                    spacingLeft: 0,
                    plotBorderWidth: 0,
                      events: {
                        'load': function () {
                          loadSeries(chart);
                        },
                        'redraw': function () {
                          if (showExtendedChart()) {
                            removeMarker('bottom');
                            loadMarkers('bottom', this, this.get("Rating" + 0), 61, 45);
                          } else {
                            removeMarker('bottom');
                          }
                          removeMarker('top');
                          loadMarkers('top', this, this.get("Baseline" + 0), 61, -1);

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
                    enabled: true,
                    margin: 0,
                    padding: 0,
                    symbolWidth: 6,
                    symbolHeight: 6,
                    itemDistance: 10,
                    itemStyle: {
                      fontSize:'10px',
                    },
                    useHTML: true,
                    labelFormat: "<span style='color: {color};'>{name}</span>"
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
                      maxPadding: 0,
                      minPadding: 0,
                      lineColor: 'transparent',
                      labels: {
                          enabled: false
                      },
                      padding: 0
                  },
                  yAxis: {
                      title: {
                          enabled: false,
                      },
                      labels: {
                          enabled: false
                      },
                      gridLineColor: "transparent",
                      gridLineWidth: 0,
                      lineWidth: 0,
                      offset: -15,
                      padding: 0,
                      maxPadding: 0,
                      minPadding: 0,
                      lineColor: 'transparent'
                  },
                  tooltip: {
                    enabled: false,
                      pointFormat: '{series.name} produced <b>{point.y:,.0f}</b><br/>warheads in {point.x}'
                  },
                  plotOptions: {
                      area: {
                          pointStart: 120,
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
                          fillOpacity: 1,
                          color: "transparent",
                          fillColor: {
                            linearGradient: { x1: 0, x2: 1, y1: 0, y2: 0 },
                            stops: [
                              [0, '#be2026'],
                              [0.5, '#f5ea16'],
                              [1, '#3aaa49']
                            ]
                          }
                      }
                  },
                  series: []
              };
              var removeMarker = function(tag) {
                if (labels[tag + "1"] !== undefined) {
                  labels[tag + "1"].destroy();
                  delete labels[tag + "1"];
                }
                if (labels[tag + "2"] !== undefined) { 
                  labels[tag + "2"].destroy();
                  delete labels[tag + "2"];
                }

              };
              var loadMarkers = function(tag, obj, series, xOff, yOff) { 
                if (series === null || series === undefined || true) {
                  return;
                }
                if (labels[tag+ "1"] === undefined) {
                  labels[tag + "1"] = obj.renderer.label($scope.scoreGraph,
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
                      series.data[0].plotX - xOff + 5,
                      series.data[0].plotY + yOff + 29)
                                  .css({
                                      fontWeight: 'bold'
                                  })
                                  .add();
                } else {  
                  labels[tag + "2"].xSetter(series.data[0].plotX - xOff + 5);
                  labels[tag + "2"].ySetter(series.data[0].plotY + yOff + 29);
                }
              };
              $timeout(function () {

                angular.element($element).highcharts(options, function () { 
                  chart = this;
                });
              }, 0);
            };
            if ($scope.benchmarkResult !== undefined) {
              getHeight();
              plot();
            }
            $scope.$watch("benchmarkResult", function (br) {
              if (chart !== undefined) {
                if (br !== undefined) {
                  getHeight();
                  loadSeries(chart);
                }
              }
            });

          }]
        };
  }]);
});