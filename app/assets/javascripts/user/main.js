/**
 * User package module.
 * Manages all sub-modules so other RequireJS modules only have to import the package.
 */
define(["angular", "./routes", "./controllers", "./services"], function(angular) {
  return angular.module("yourprefix.user", ["ngCookies", "user.routes", "user.services"]);
});
