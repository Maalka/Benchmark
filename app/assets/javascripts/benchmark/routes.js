/**
 * Benchmark routes.
 */
define(['angular', './controllers', 'common'], function(angular, controllers) {
  'use strict';

  var mod = angular.module('benchmark.routes', ['benchmark.common']);
  mod.config(['$routeProvider', function($routeProvider) {
    $routeProvider
      .when('/',  {templateUrl: '/assets/javascripts/benchmark/benchmark.html',  controller:controllers.DashboardCtrl});
  }]);
  return mod;
});
