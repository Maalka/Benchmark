/**
 * Dashboard controllers.
 */
define([], function() {
  'use strict';

  /**
   * user is not a service, but stems from userResolve (Check ../user/services.js) object used by dashboard.routes.
   */
  var BaselineCtrl = function($scope, user) {
    $scope.user = user;
  };
  BaselineCtrl.$inject = ['$scope', 'user'];

  return {
    BaselineCtrl: BaselineCtrl
  };

});
