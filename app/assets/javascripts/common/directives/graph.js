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
            baseline: '=baseline',
          },
          controller: ["$scope", "$element", function ($scope, $element) {
            var bars = [];
            var k = 20;
            for (k; k < 120; k += 20) {
              bars.push([k, 120]);
            }
            var plot = function () {
              var options = {
                  chart: {
                      margin: [75, 0, 0, 0],
                      height: 250

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
                    name: 'Revenue',
                    id: 'revenue',
                    type: 'area',
                    data: [[0, 120], [140, -20]],
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
                  },
                  {
                      type: 'flags',
                      name: 'Events',
                      color: '#333333',
                      y: -75, 
                      fillColor: 'rgba(255,255,255,0.8)',
                      shape: "url(/assets/images/targetTop.png)",
                      style: {
                        color: "white"
                      },
                      data: [
                          { x: 0, 
                            text: 'Highsoft won "Entrepeneur of the Year" in Sogn og Fjordane, Norway', 
                            title: '100'}
                      ],
                      onSeries: 'revenue',
                      showInLegend: false
                  },
                  {
                      type: 'flags',
                      name: 'Events',
                      color: '#333333',
                      style: {
                        color: "white"
                      },                      
                      shape: "url(/assets/images/targetBottom.png)",
                      fillColor: 'rgba(255,255,255,0.8)',
                      y: -45,
                      data: [
                          { x: 0, 
                            text: 'Highsoft won "Entrepeneur of the Year" in Sogn og Fjordane, Norway', 
                            title: '100'}
                      ],
                      onSeries: 'revenue',
                      showInLegend: false
                  }
                ]
              };
              angular.element($element).highcharts(options);
            };
            plot();
          }]
        };
  }]);
});