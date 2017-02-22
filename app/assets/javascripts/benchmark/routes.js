/**
 * Benchmark routes.
 */
define(['angular', './controllers', 'common'], function(angular, controllers) {
  'use strict';

  var mod = angular.module('benchmark.routes', ['benchmark.common']);
  mod.config(['$routeProvider', function($routeProvider) {
    $routeProvider
        .when('/',  {templateUrl: 'javascripts/benchmark/benchmark.html',  controller:controllers.DashboardCtrl})
        .when('/about',  {templateUrl: 'javascripts/benchmark/about.html'})
        .when('/guide',  {templateUrl: 'javascripts/benchmark/guide.html'})
        .when('/bulk',  {templateUrl: 'javascripts/benchmark/bulk.html'});
  }]);
  return mod;
});
