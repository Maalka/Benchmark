/**
 * Common functionality.
 * Serves as an entry point so other modules only have to include one module.
 */
define(["angular", "./services/helper", "./services/playRoutes", "./filters", "./directives/example"],
  function(angular) {
    return angular.module("yourprefix.common", ["common.helper", "common.playRoutes", "common.filters",
      "common.directives.example"]);
});
