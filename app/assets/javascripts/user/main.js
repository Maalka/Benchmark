/**
 * User module.
 * This shows a very explicit approach of dependency management.
 * Downside is you have to declare every needed module in every file that needs it.
 */
define(["angular", "./routes", "./controllers", "./services"], function(angular) {
  return angular.module("yourprefix.user", ["ngCookies", "user.routes", "user.services"]);
});
