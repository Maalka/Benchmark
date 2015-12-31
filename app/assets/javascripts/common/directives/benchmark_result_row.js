/**
 * A bencharmk cell directive
 * prints out the value or an error based on the response from the benchmark web server
 * that references them all.
 */
define(['angular'], function(angular) {
  'use strict';

  var mod = angular.module('common.directives', []);
  mod.directive('benchmarkResultRow', ['$log', function($log) {
    var getValue = function(root, key) { 
      var returnValue;
      for (var i =0; i < root.length; i ++) { 
        if (root[i][key] !== undefined) { 
          returnValue = root[i][key];
          break;
        }
      }
      return returnValue;
    };
    return {
      restrict: 'A',
      scope: {
        'result': "=",
        'title': "=",
        'columns': "="
      },
      templateUrl: '/assets/javascripts/common/partials/benchmark_result_row.html',
      link: function(scope) {
        $log.info('Here prints the example directive from /common/directives.');
        

        var getResultValue = function (key) { 
          // look in errors
          var value = getValue(scope.result.errors, key);
          var returnValue;
          if(value === undefined) {
            returnValue = {
              'ok': true,
              'value': getValue(scope.result.values, key)
            };
          } else {
            $log.debug("Failed to fetch value (" + key + "): " + value);
            returnValue = {
              'ok': false,
              'value': "Not Available"
            };
          }
          return returnValue;
        };
        scope.$watch('result', function(result) { 
          var columns = [];
          if (result !== undefined){ 
            for (var i =0; i < scope.columns.length; i++) {
              columns[i] = getResultValue(scope.columns[i]) || "undefined";
            } 
          }
          scope.resultColumns = columns;
        });
      }
    };
  }]);
  return mod;
});
