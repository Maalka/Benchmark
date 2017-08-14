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

            var ticksHERS = {'12.0':20,'27.4':40,'42.8':60,'58.2':80,'73.6':100,'89.0':120,'104.4':140,'119.8':160,'135.2':180};
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
    
            var getTempZEPI = function() {

                if(getBRByKey("siteEnergyALL")) {
                    return getBRByKey("actualZEPIwOnAndOffSite") ? getBRByKey("actualZEPIwOnAndOffSite") : 0;
                }else{
                    return getBRByKey("actualZEPIwOnAndOffSite") ? getBRByKey("actualZEPIwOnAndOffSite") : undefined;
                }
            };

            var checkSiteEUI = function() {

                if (getTempZEPI() !== undefined) {
                    return getBRByKey("siteEUIwOnAndOffSite") ? Math.ceil(getBRByKey("siteEUIwOnAndOffSite")) : 0;
                }else {
                    return getBRByKey("siteEUIwOnAndOffSite") ? Math.ceil(getBRByKey("siteEUIwOnAndOffSite")) : undefined;
                }
            };

            var getEUIMetrics = function() {
                if (getTempZEPI() !== undefined) {
                    $scope.EE = getBRByKey("siteEUI") ? Math.ceil(getBRByKey("siteEUI")) : 0;
                    $scope.OnSite = getBRByKey("siteEUIwOnSite") ? Math.ceil(getBRByKey("siteEUIwOnSite")) : 0;
                    $scope.OffSite = getBRByKey("siteEUIwOffSite") ? Math.ceil(getBRByKey("siteEUIwOffSite")) : 0;
                    $scope.OnAndOffSite = getBRByKey("siteEUIwOnAndOffSite") ? Math.ceil(getBRByKey("siteEUIwOnAndOffSite")) : 0;

                    $scope.ZepiEE = getBRByKey("actualZEPI") ? Math.ceil(getBRByKey("actualZEPI")) : 0;
                    $scope.ZepiOnSite = getBRByKey("actualZEPIwOnSite") ? Math.ceil(getBRByKey("actualZEPIwOnSite")) : 0;
                    $scope.ZepiOffSite = getBRByKey("actualZEPIwOffSite") ? Math.ceil(getBRByKey("actualZEPIwOffSite")) : 0;
                    $scope.ZepiOnAndOffSite = getBRByKey("actualZEPIwOnAndOffSite") ? Math.ceil(getBRByKey("actualZEPIwOnAndOffSite")) : 0;

                }else {
                    $scope.EE = getBRByKey("siteEUI") ? Math.ceil(getBRByKey("siteEUI")) : undefined;
                    $scope.OnSite = getBRByKey("siteEUIwOnSite") ? Math.ceil(getBRByKey("siteEUIwOnSite")) : undefined;
                    $scope.OffSite = getBRByKey("siteEUIwOffSite") ? Math.ceil(getBRByKey("siteEUIwOffSite")) : undefined;
                    $scope.OnAndOffSite = getBRByKey("siteEUIwOnAndOffSite") ? Math.ceil(getBRByKey("siteEUIwOnAndOffSite")) : undefined;

                    $scope.ZepiEE = getBRByKey("actualZEPI") ? Math.ceil(getBRByKey("actualZEPI")) : undefined;
                    $scope.ZepiOnSite = getBRByKey("actualZEPIwOnSite") ? Math.ceil(getBRByKey("actualZEPIwOnSite")) : undefined;
                    $scope.ZepiOffSite = getBRByKey("actualZEPIwOffSite") ? Math.ceil(getBRByKey("actualZEPIwOffSite")) : undefined;
                    $scope.ZepiOnAndOffSite = getBRByKey("actualZEPIwOnAndOffSite") ? Math.ceil(getBRByKey("actualZEPIwOnAndOffSite")) : undefined;
                }
                return {
                    EE:$scope.EE,
                    OnSite:$scope.OnSite,
                    OffSite:$scope.OffSite,
                    OnAndOffSite:$scope.OnAndOffSite,

                    ZepiEE:$scope.ZepiEE,
                    ZepiOnSite:$scope.ZepiOnSite,
                    ZepiOffSite:$scope.ZepiOffSite,
                    ZepiOnAndOffSite:$scope.ZepiOnAndOffSite
                };
            };

            var getEEMarkerX = function() {
               var EEgap = $scope.baselineConstant - getTempZEPI();

               var totalPercentReduction = getBRByKey("percentBetterActual");

               var totalSiteEnergy = getBRByKey("siteEnergyALL")? getBRByKey("siteEnergyALL") : 0;
               var onsite = getBRByKey("onSiteRenewableTotal") ? getBRByKey("onSiteRenewableTotal") : 0;
               var purchased = getBRByKey("offSitePurchasedTotal") ? getBRByKey("offSitePurchasedTotal") : 0;
               var totalCombinedEnergy = totalSiteEnergy + onsite + purchased;

               var energyEfficiency = totalPercentReduction * totalSiteEnergy / totalCombinedEnergy;

               var total = totalPercentReduction;
               var EEx = $scope.baselineConstant - EEgap * (energyEfficiency/total);
               var ONx = $scope.baselineConstant - EEgap * (energyEfficiency/total) - EEgap * ((total * onsite / totalCombinedEnergy) / total);

               return {
                 EEx: EEx,
                 ONx: ONx
               };
           };


            var percentBetter = function() {
                if (getTempZEPI() !== undefined) {
                    return (checkSiteEUI() !== undefined) ? getPercentBetter(Math.ceil(checkSiteEUI())) : 0;
                }else {
                    return (checkSiteEUI() !== undefined) ? getPercentBetter(Math.ceil(checkSiteEUI())) : undefined;
                }
            };

            var getPercentBetter = function(siteEUI) {
                return (siteEUI > Math.ceil(getBRByKey("percentBetterSiteEUI"))) ? getBRByKey("percentBetterActualtoGoal") : getBRByKey("actualGoalBetter");
            };

            var showGreenEnergyChart = function () {
              if (goodZEPI()) {
                return $scope.baselineConstant !== undefined;
              }
              return false;
            };

            var goodZEPI = function () {
              return (checkSiteEUI() !== undefined) ? getTempZEPI() < 100 : false;
            };

            var showExtendedChart = function () {
              return (checkSiteEUI() !== undefined) ? true : false;
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
              var k = ($scope.baselineConstant === 130) ? 12 : 20;

              var dlOff = function(v) {
                var tempV = Math.floor(v);
                if (tempV === 120) {
                  return -11;
                } else if (tempV === 27 || tempV === 12 || tempV === 42 || tempV === 20) {
                  return -28;
                } else if (tempV === 135) {
                  return -15;
                } else if (tempV === 119) {
                  return -5;
                } else {
                  return -20;
                }
              };

              var dlYOff = function(v) {
                if (v > 120) {
                  return -13;
                } else {
                  return 3;
                }
              };

              var spaceBar = ($scope.baselineConstant === 130) ? 15.4 : 20;


              for (k; k <= 140; k += spaceBar) {
                if (k > 118 && k < 122) { 
                  continue;
                }
                bars.push(
                  {
                    x: k,
                    y: k > 120 ? -20 : 110,
                    color: k > 180 ? 'transparent' : undefined,
                    dataLabels: {
                      x: dlOff(k),
                      y: dlYOff(k)
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
                      var x = fixBig(this.x, true);
                      if (x < 5) {
                        return "<span style='font-weight: 100;color: #a6a8ab;'>" + x + "</span>";
                      } else {
                        return "<span style='font-weight: 100;'>" + x + "</span>";
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

              updateOrAddSeries(chart, { type: 'line',
                id: "zero",
                name: "zero",
                color: "black",
                animation: false,
                dataLabels: {
                  style: { "fontSize": "16px" },
                  enabled: true,
                  verticalAlign: "bottom",
                  color: "black",
                  y: -6,
                  x: -1
                },
                data: [[120, 0]]
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
              if (round($scope.baselineConstant) - round(getBRByKey("percentBetterZEPI")) < 10) {
                yOffset = 40;
              }
              else if (round($scope.baselineConstant) - round(getBRByKey("percentBetterZEPI")) >= 10 && round($scope.baselineConstant) - round(getBRByKey("percentBetterZEPI")) < 20) {
                yOffset = 30;
              }
              var markers = createMarker("BASELINE", -40 - yOffset, $scope.baselineConstant, "maalkaFlag", "black", 'zepi', false)
                        .concat(createMarker("TARGET", -40, getBRByKey("percentBetterZEPI"), "maalkaFlag", "black", "zepi", false));

              for (var i = 0; i < markers.length; i ++) {
                updateOrAddSeries(chart, markers[i], i === markers.length - 1, false);
              }

            };


            var createExtendedChartFeatures = function (remove) {
              if (remove) {
                //updateOrAddSeries(chart, { "id": "progressLine", "remove": true}, false);
              } else {
/*                updateOrAddSeries(chart, { type: 'line',
                  name: "progressLine",
                  id: 'progressLine',
                  data: sortData([
                    {
                      x: fixX(getTempZEPI()),
                      y: -15,
                      marker: {
                        enabled: fixX(getBRByKey("percentBetterZEPI")) < fixX(getTempZEPI()),
                        symbol: "arrow-right"
                      }
                    },
                    {
                      x: fixX(getBRByKey("percentBetterZEPI")),
                      y: -15,
                      marker: {
                        enabled: fixX(getBRByKey("percentBetterZEPI")) > fixX(getTempZEPI()),
                        symbol: "arrow-right"
                      }
                    }
                  ]),
                  animation: false,
                  color: fixX(getBRByKey("percentBetterZEPI")) < fixX(getTempZEPI()) ? green : red,
                  arrow: true,
                  dashStyle: "ShortDot",
                  showInLegend: false,
                  enableMouseTracking: false
                });*/

               //Gap between onsite scores and YOUR BUILDING flag
               var ONgap = (getTempZEPI() !== undefined) ? getEEMarkerX().ONx - getTempZEPI() : 0;

               //Gap between energy efficiency(EE) and YOUR BUILDING flag
               var EEgap = (getTempZEPI() !== undefined) ? getEEMarkerX().EEx - getTempZEPI() : 0;


                updateOrAddSeries(chart,
                    createMarker( "YOUR BUILDING", 60, getTempZEPI(),
                      ONgap > 15 || EEgap > 10 ? "maalkaFlagLeftBottom" : "maalkaFlagBottom",
                      "black", "axisLine", false)[0],false
                );


                /*var better = fixX(getBRByKey("percentBetterZEPI")) < fixX(getTempZEPI());
                updateOrAddSeries(chart,
                    createMarker("PROGRESS PERCENT", 17, getBRByKey("percentBetterZEPI"),
                    better ? "maalkaLongFlagLeftBottom": "maalkaLongFlagBottom",
                    better ? green : red, "axisLine", true)[0], false
                );*/
              }

              //Gap between EEscores and score text
              var EEendGap = (getEEMarkerX().EEx !== undefined) ? $scope.baselineConstant - getEEMarkerX().EEx : 0;

              //Gap between onsite scores and score text
              var ONendGap = (getEEMarkerX().ONx !== undefined) ? $scope.baselineConstant - getEEMarkerX().ONx : 0;



              //Score text marker
              updateOrAddSeries(chart,
                  createMarker("scoreText", 160, $scope.baselineConstant, EEendGap < 10 || ONendGap < 10 ? "maalkaFlagLeftBottom": "maalkaFlag", "transparent", 'zepi',false)[0], false
              );

              //Gap between onsite and EE scores.
              var componentsGap = (getEEMarkerX().EEx !== undefined && getEEMarkerX().ONx !== undefined) ? getEEMarkerX().EEx - getEEMarkerX().ONx : 0;

              //Energy Efficiency flag
              updateOrAddSeries(chart,
                  createMarker("EEscores", 60, getEEMarkerX().EEx, "maalkaFlagLeftBottom", "transparent", 'axisLine',false)[0], false
              );

              //Onsite scores flag
              updateOrAddSeries(chart,
                  createMarker("ONScores", 60, getEEMarkerX().ONx, componentsGap < 5 ? "maalkaFlagBottom": "maalkaFlagLeftBottom", "transparent", 'axisLine',false)[0], false
              );

            };



            var createGreenChartFeatures = function (remove) {

              if (remove) {
                updateOrAddSeries(chart, {id: "componentLineLeft", remove: true}, false);
                updateOrAddSeries(chart, {id: "componentLineRight", remove: true}, false);
                updateOrAddSeries(chart, {id: "ONcomponentLine", remove: true}, false);
                updateOrAddSeries(chart, {id: "EEcomponentLine", remove: true}, false);
                updateOrAddSeries(chart, {id: "energyEfficiency", remove: true}, false);
                updateOrAddSeries(chart, {id: "onsiteRenewable", remove: true}, false);
                updateOrAddSeries(chart, {id: "greenPower", remove: true}, false);
                return;
              } else {
                var gap = $scope.baselineConstant - getTempZEPI();

                var totalPercentReduction = getBRByKey("percentBetterActual");

                var totalSiteEnergy = getBRByKey("siteEnergyALL")? getBRByKey("siteEnergyALL") : 0;
                var onsite = getBRByKey("onSiteRenewableTotal") ? getBRByKey("onSiteRenewableTotal") : 0;
                var purchased = getBRByKey("offSitePurchasedTotal") ? getBRByKey("offSitePurchasedTotal") : 0;
                var totalCombinedEnergy = totalSiteEnergy + onsite + purchased;

                //divide by zero condition check
                if (totalCombinedEnergy === 0) {
                  return;
                }

                var onsiteRenewable = totalPercentReduction * onsite / totalCombinedEnergy;
                var greenPower = totalPercentReduction * purchased / totalCombinedEnergy;
                var energyEfficiency = totalPercentReduction * totalSiteEnergy / totalCombinedEnergy;

                var total = totalPercentReduction;

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
                      y: -41,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  animation: false,
                  color: "#595959",
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
                      x: fixX(getTempZEPI()),
                      y: -50,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX(getTempZEPI()),
                      y: -75,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  animation: false,
                  color: "#transparent",
                  arrow: true,
                  showInLegend: false,
                  enableMouseTracking: false
                });

                updateOrAddSeries(chart, { type: 'line',
                  name: "ONcomponentLine",
                  id: 'ONcomponentLine',
                  data: sortData([

                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                               gap * (onsiteRenewable / total)),
                      y: -40,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                               gap * (onsiteRenewable / total)),
                      y: -75,
                      marker: {
                        enabled: false
                      }
                    }

                  ]),
                  animation: false,
                  color: ($scope.baselineConstant - gap * (energyEfficiency / total) - gap * (onsiteRenewable / total)) - getTempZEPI() > 5 ? "#595959 ": "transparent",
                  arrow: true,
                  dashStyle: "ShortDot",
                  showInLegend: false,
                  enableMouseTracking: false
                });

               updateOrAddSeries(chart, { type: 'line',
                  name: "EEcomponentLine",
                  id: 'EEcomponentLine',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -40,

                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -75,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  animation: false,
                  color: "#595959 ",
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
                      y: -30,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -30,
                      marker: {
                        enabled: false
                      }
                    }
                  ]),
                  lineWidth: 20,
                  linecap: "square",
                  animation: false,
                  color: "#595959",
                  showInLegend: true,
                  enableMouseTracking: false
                });

                updateOrAddSeries(chart, { type: 'line',
                  name: "on-site renewable energy",
                  id: 'onsiteRenewable',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total)),
                      y: -30,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total)),
                      y: -30,
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
                  name: "green power purchased",
                  id: 'greenPower',
                  data: sortData([
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total)),
                      y: -30,
                      marker: {
                        enabled: false
                      }
                    },
                    {
                      x: fixX($scope.baselineConstant - gap * (energyEfficiency / total) -
                        gap * (onsiteRenewable / total) -
                        gap * (greenPower / total)),
                      y: -30,
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

//            var showMetricsFlag = function() {
//                if (getEUIMetrics().EE -  getEUIMetrics().OnSite === 0) {
//                    return true;
//                }
//                return false;
//            };

            /*** Flag Definition for the Markers  ***/
            var createMarker = function(title, yOff, x, shape, color, series, onlyTitle) {
              if (x !== undefined && !isNaN(x) && title !== undefined) {
                var text = "";
                if (title === "YOUR BUILDING") {
                   if (shape === "maalkaFlagLeftBottom") {
                      text = "<b>" + checkSiteEUI() + "</b><br><b>" + round(x) + "</b><br><br><b>" + title + "</b>";
                   } else {
                       text = "<b>" + checkSiteEUI() + "</b><br><b>" + round(x) + "</b><br><br><b> YOUR BLDG </b>";
                   }
                }
                else if (title === "scoreText") {
                    if (shape === "maalkaFlagLeftBottom") {
                        text = "EUI <br> Score ";
                    } else {
                        text = "EUI <br> Zero Score ";
                  }
                } else if (title === "BASELINE") {
                  text = "<b>"+title + "</b><br><b>" + Math.ceil(getBRByKey("medianSiteEUI")) + "</b> FF-EUI" + "<br><b>" + $scope.baselineConstant + "</b> Score";
                } else if (title === "PROGRESS PERCENT") {
                  text = round(percentBetter()) + "%";
                } else if (title === "TARGET") {
                  text = "<b>"+title + "</b><br><b>" + Math.ceil(getBRByKey("percentBetterSiteEUI")) + "</b> FF-EUI" + "<br><b>" + round(x) + "</b> Score";
                } else if (title === "EEscores") {
                      text = "<b>" + getEUIMetrics().EE + "</b><br><b>" + getEUIMetrics().ZepiEE + "</b><br><br><b>";
                } else if (title === "ONScores") {
                    if(getEUIMetrics().EE - getEUIMetrics().OnSite === 0 || getEEMarkerX().ONx - getTempZEPI() <= 5) {
                        text = " ";
                    }
                    else {
                        text = "<b>" + getEUIMetrics().OnSite + "</b><br><b>" + getEUIMetrics().ZepiOnSite + "</b><br><br><b>";
                    }
                }
                var textColor;
                if (onlyTitle) {
                  textColor = fixX(getBRByKey("percentBetterZEPI")) < fixX(getTempZEPI()) ? green : red;
                }
                return [
                    {   id: title + "0",
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

            //var labels = {};
            var fixBig = function(x, noClip) {

                if($scope.baselineConstant === 130){
                  return 160 - ticksHERS[x.toFixed(1)];
                }

                if (x > 150  && !noClip) {
                    x = 150;
                }

                if (x < 0  && !noClip) {
                    x = -10;
                }
                return maxX - x;
            };
            var fixX = function(x, noClip) {

                if($scope.baselineConstant === 130){
                  x = 100 * x / 130;
                }

                if (x > 110  && !noClip) {
                    x = 110;
                }

                if (x < -15  && !noClip) {
                    x = -15;
                }

                return maxX - x;
            };

            var loadSeries = function(chart) {

                createBaseChartFeatures();
                createExtendedChartFeatures(!showExtendedChart());
                createGreenChartFeatures(!showGreenEnergyChart());

                if (!showExtendedChart()) {
                  for(var chartCnt = 0; chartCnt < chart.series.length; chartCnt++){
                     switch(chart.series[chartCnt].options.id) {
                        case "YOUR BUILDING0":
                            updateOrAddSeries(chart, {id: "YOUR BUILDING0", remove: true}, false);
                            break;
                        case "axisLinemarker":
                            updateOrAddSeries(chart, {id: "axisLinemarker", remove: true}, false);
                            break;
                        case "progressLine":
                            updateOrAddSeries(chart, {id: "progressLine", remove: true}, false);
                            break;
                        case "PROGRESS PERCENT0":
                            updateOrAddSeries(chart, {id: "PROGRESS PERCENT0", remove: true}, false);
                            break;
                     }
                  }
                }

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
                if ($scope.baselineConstant - getBRByKey("percentBetterZEPI") > 30) {
                  height = height + 45;
                } else {
                  height = height + 65;
                }
              if (showGreenEnergyChart()) {
                height = height + 47;
              }
              }
              return height;
            };
            var getMargin = function() {
              var margin = [60, -20, 0, -30];
              baselineEUI = getBRByKey("medianSiteEUI");
              if (showExtendedChart()) {
                if ($scope.baselineConstant - getBRByKey("percentBetterZEPI") > 30) {
                  margin[0] = 20;
                  margin[2] = 40;
                } else {
                  margin[0] = 50;
                  margin[2] = 40;
                }
                if (showGreenEnergyChart()) {
                  margin[2] = 20;
                }
                margin[1] = -18;
               // margin[2] = 20;
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
                              [0, '#bd2227'],
                              [0.15, '#cd2027'],
                              [0.5, '#f4ea0e'],
                              [0.85, '#39a949'],
                              [1, '#3b5829']
                            ]
                          }
                      }
                  },
                  series: []
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
                  console.log(getEUIMetrics());

                }
              }
            });

          }]
        };
  }]);
});