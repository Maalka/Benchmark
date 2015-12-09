/**
 * Dashboard controllers.
 */
define([], function() {
  'use strict';
  var DashboardCtrl = function($rootScope, $scope, benchmarkServices) {
    $rootScope.pageTitle = "Hello world";
    $scope.targetModel = {};
    $scope.submit = function () {
      benchmarkServices.submit($scope.targetModel).then(function (response) { 
        console.log(response);
      });
      console.log($scope.targetModel);
    };
  };
  DashboardCtrl.$inject = ['$rootScope', '$scope', 'benchmarkServices'];
  return {
    DashboardCtrl: DashboardCtrl
  };
});
