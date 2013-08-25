/**
 * Dashboard controllers.
 */
define(["angular"], function(angular) {
  "use strict";

  var DashboardCtrl = function($scope, userService) {
    $scope.user = userService.getUser();
  };
  DashboardCtrl.$inject = ["$scope", "userService"];

  var AdminDashboardCtrl = function($scope, userService) {

  };
  AdminDashboardCtrl.$inject = ["$scope", "userService"];

  return {
    DashboardCtrl: DashboardCtrl,
    AdminDashboardCtrl: AdminDashboardCtrl
  };

});
