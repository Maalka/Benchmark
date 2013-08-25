/**
 * Main routes.
 */
define(["angular", "./controllers", "common/common"], function(angular, controllers) {
  var mod = angular.module("main.routes", ["yourprefix.common"]);
  mod.config(["$routeProvider", function($routeProvider) {
    $routeProvider
      .when("/",  {templateUrl: "/assets/templates/main/home.html", controller:controllers.HomeCtrl})
      .otherwise( {templateUrl: "/assets/templates/main/notFound.html"});
  }]);
  return mod;
});
