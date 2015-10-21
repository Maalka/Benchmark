/**
 * Dashboard routes.
 */
define(['angular', './controllers', 'common'], function(angular, controllers) {
  'use strict';

  var mod = angular.module('baseline.routes', ['yourprefix.common']);
  mod.config(['$routeProvider', 'userResolve', function($routeProvider, userResolve) {
    $routeProvider
      .when('/baseline',  {templateUrl: '/assets/javascripts/baseline/baseline.html',  controller:controllers.BaselineCtrl, resolve:userResolve});
      //.when('/admin/dashboard',  {templateUrl: '/assets/templates/dashboard/admin.html',  controller:controllers.AdminDashboardCtrl})
  }]);
  return mod;
});
