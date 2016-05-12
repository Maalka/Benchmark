/**
 * A benchmark cell directive
 * prints out the value or an error based on the response from the benchmark web server
 * that references them all.
 */
define(['angular','./main'], function(angular) {
  'use strict';

  var mod = angular.module('common.directives');
  mod.directive('benchmarkResultRow', ['$log', function($log) {

    var getValue = function(root, key) {
      var returnValue;
      for (var i =0; i < root.length; i ++) {
        if (root[i][key] !== undefined) {
          returnValue = root[i][key].toFixed(1);
          break;
        }
      }
      return returnValue;
    };
    return {
      restrict: 'A',
      scope: {
        'result': "=",
        'field': "=",
        'units': "=",
        'columns': "="
      },
      templateUrl: 'javascripts/common/partials/benchmark_result_row.html',
      link: function(scope) {

        var getResultValue = function (key) {
          // look in errors
          var value = getValue(scope.result, key);
          var returnValue;
          if(value !== undefined) {
              if(isNaN(value)) {
                returnValue = {
                'ok': false,
                'value': "N/A"
                };
              }else{
                returnValue = {
                'ok': true,
                'value': getValue(scope.result, key)
                };
              }

          } else {
            $log.debug("Failed to fetch value (" + key + "): " + value);
            returnValue = {
              'ok': false,
              'value': "N/A"
            };
          }
          return returnValue;
        };

        scope.$watch('result', function(result) {
          if (result === undefined || result === null) {
            return;
          }
          var columns = [];
          if (result !== undefined){
            for (var i =0; i < scope.columns.length; i++) {
              columns[i] = getResultValue(scope.columns[i]) || "undefined";
            }
          }
          scope.resultColumns = columns;

          scope.title = scope.field;
          scope.subTitle =  scope.units;
        });
      }
    };
  }]);
  return mod;
});
