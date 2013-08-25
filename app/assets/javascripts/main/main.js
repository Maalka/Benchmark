/**
 * Main, shows the start page and provides controllers for the header and the footer.
 * This the entry module which serves as an entry point so other modules only have to include a
 * single module.
 */
define(["angular", "./routes", "./controllers"], function(angular, routes, controllers) {
  var mod = angular.module("yourprefix.main", ["main.routes"]);
  mod.controller("HeaderCtrl", controllers.HeaderCtrl);
  mod.controller("FooterCtrl", controllers.FooterCtrl);
  return mod;
});
